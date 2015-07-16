#lang racket
(require (rename-in bitsyntax 
                    [bit-string-length length-in-bits])
         racket/tcp
         racket/port)

(provide mqtt-handlers
         mqtt%
         mqtt-debug)

(define DEBUG? false)
(define (mqtt-debug bool)
  (set! DEBUG? bool))

(define-syntax-rule (debug fmt-str args ...)
  (when DEBUG?
    (printf fmt-str args ...)))


(define (packet-dbg bs)
  (when (bit-string? bs)
    (set! bs (bit-string-pack bs)))
  
  (for ([b bs])
    (debug "[0x~a]\t~a\t~a~n" 
           (hexpad (number->string b 16))
           b
           (number->string b 2)
           )))


(define PROTOCOL-VERSION 'v3.1.1)
(define RESERVED-ZERO 0)
(define RESERVED-ONE 1)

(define packet-identifier (make-parameter 0))
(define (next-packet-identifier)
  (define return (packet-identifier))
  (packet-identifier (add1 (packet-identifier)))
  return)

(define broker (make-parameter '("mq.thingmq.com" 1883)))
(broker '("broker.mqttdashboard.com" 1883))
(broker '("localhost" 9999))

(define (encode/utf-8 str)
  (bit-string
   [(bytes-length (string->bytes/utf-8 str)) :: bytes 2]
   [(string->bytes/utf-8 str) :: binary]))

(define/contract (bit-string-length/bytes bs)
  (-> bit-string? integer?)
  (bytes-length (bit-string->bytes bs)))

(define (build-bit-string . bools)
  (define bs 0)
  (for ([flag (reverse (rest (reverse bools)))])
    (define bit (if flag 1 0))
    (set! bs (bitwise-ior bs bit))
    (set! bs (arithmetic-shift bs 1)))
  (set! bs (bitwise-ior (if (first (reverse bools)) 1 0) bs))
  bs)

(define (build-version-string version)
  (define version-string (make-parameter ""))
  (case version
    [(v3 version3 v3.1) 
     (version-string (bit-string
                      ;; Tag Length
                      [#x06 :: bytes 2]
                      ;; Protocol Tag
                      [#"MQlsdp" :: binary]
                      ;; Protocol Level
                      [#x03 :: bytes 1]))]
    [(v4 version4 v3.1.1)
     (version-string (bit-string
                      ;; Tag Length
                      [#x04 :: bytes 2]
                      ;; Protocol Tag
                      [#"MQTT" :: binary]
                      ;; Protocol Level
                      [#x04 :: bytes 1]))])
  (version-string))

(define (variable-header version
                         #:username [username false]
                         #:password [password false]
                         #:will-retain [will-retain false]
                         #:will-qos [will-qos false]
                         #:will-flag [will-flag false]
                         #:clean-session [clean-session true]
                         #:keepalive [keepalive 60])
  (bit-string
   [(build-version-string version) :: binary]
   ;; Connect Flags - one byte
   [(build-bit-string username
                      password
                      will-retain
                      will-qos
                      will-flag
                      clean-session
                      RESERVED-ZERO) :: bytes 1]
   ;; Keepalive - 60 second default, two bytes
   [keepalive :: bytes 2]
   ))


(define (hexpad str)
  (if (< (string-length str) 2)
      (format "0~a" str)
      str))

(struct connection (uri port in out))

(define (send-msg  conn msg)
  (define bls (bytes->list (bit-string->bytes msg)))
  (packet-dbg bls)
  (for ([b bls])
    (write-byte b (connection-out conn)))
  (flush-output (connection-out conn)))

(define (new-mqtt-connection uri port)
  (define-values (inp outp)
    (tcp-connect uri port))
  (connection uri port inp outp))


(require racket/match)



(define (connect #:id [id "racket"]
                 #:version [version 'v3.1.1]
                 #:username [username false]
                 #:password [password false]
                 #:will-retain [will-retain false]
                 #:will-qos [will-qos false]
                 #:will-flag [will-flag false]
                 #:clean-session [clean-session true]
                 #:keepalive [keepalive 60])
  (bit-string 
   ;; 3.1 CONNECT – Client requests a connection to a Server
   ;; 3.1.1  Fixed header
   #x10
   ;; 2.2.3 Remaining Length
   [(+ (bit-string-length/bytes
        (variable-header version))
       (bit-string-length/bytes (encode/utf-8 id)))
    :: bytes 1]
   [(variable-header version
                     #:username username
                     #:password password
                     #:will-retain will-retain
                     #:will-qos will-qos
                     #:will-flag will-flag
                     #:keepalive keepalive
                     #:clean-session clean-session) :: binary]
   [(encode/utf-8 id) :: binary]
   ))

(define (connack conn 
                 #:session-present [sp false])
  (define packet (read-bytes 4 (connection-in conn)))
  (match (bytes->list packet) 
    [(list #x20 #x02 flags retcode)
     (cond
       [(or (and sp (= flags 1))
            (and (not sp) (= flags 0)))
        (case retcode
          [(#x00) 'CONNACK:ACCEPTED]
          [(#x01) 'CONNACK:REFUSED]
          [(#x02) 'CONNACK:IDENTIFIER-REJECTED]
          [(#x03) 'CONNACK:SERVER-UNAVAILABLE]
          [(#x04) 'CONNACK:BAD-USERNAME-OR-PASSWORD]
          [(#x05) 'CONNACK:NOT-AUTHORIZED]
          [else 'CONNACK:RESERVED]
          )]
       [else 'CONNACK:BADFLAGS])]
    [else 'CONNACK:BADPACKET]))


(define (pingreq)
  (bit-string
   #b11000000 #x00))

(define (pingresp conn)
  (define packet (read-bytes 2 (connection-in conn)))
  (match (bytes->list packet)
    [(list #b11010000 #x00)
     'PINGRESP:OK]
    [else 'PINRESP:ERROR]))

(define (disconnect conn)
  (define packet (bit-string #b11100000 #x00))
  (send-msg conn packet)
  (close-input-port (connection-in conn))
  (close-output-port (connection-out conn)))

(define (connect/connack c #:id [id "racket"])
  (send-msg c (connect #:id id 
                       #:clean-session true))
  (connack c #:session-present false))

(define (pingreq/resp c)
  (send-msg c (pingreq))
  (pingresp c))  

(define/contract (subscription-payload channel)
  (-> bytes? bit-string?)
  (bit-string
   [(bytes-length channel) :: bytes 2]
   [channel :: binary]
   ;; FIXME: QoS
   [#x00 :: bytes 1]))

(define/contract (subscribe channel)
  (-> string? bit-string?)
  (when (string? channel)
    (set! channel (string->bytes/utf-8 channel)))
  (define payload (subscription-payload channel))
  (bit-string
   ;; Control packet type (8)
   #b10000010
   ;; Remaining Length - ONE BYTE
   ;; Length of var header - 2 bytes
   ;; Length of payload: variable
   [(+ (bit-string-length/bytes payload) 2) :: bytes 1]
   ;; Variable header
   [(next-packet-identifier) :: bytes 2]
   [payload :: binary]
   ))

(define/contract (publish-variable-header channel msg)
  (-> bytes? bytes? bit-string?)
  (bit-string 
   [(bytes-length channel) :: bytes 2]
   [channel :: binary]
   ;; FIXME: Packet identifier should not be random.
   [(random 1000) :: bytes 2]
   ;; The server knows how long the message is by subtracting the variable
   ;; header length from the total length.
   [msg :: binary]))
   
(define-syntax-rule (->bytes! str)
  (set! str (string->bytes/utf-8 str)))

(define/contract (publish channel msg)
  (-> string? string? bit-string?)
  (->bytes! channel)
  (->bytes! msg)
  (define payload (publish-variable-header channel msg))
  (bit-string
   ;; FIXME: DUP and other flags here.
   #b00110000
   [(bit-string-length/bytes payload) :: bytes 1]
   [payload :: binary]))

;; FIXME: It would be nice if we had the identifiers
;; that we were ACKing. This would allow for a check.
(define (suback conn)
  (define packet (read-bytes 2 (connection-in conn)))
  (match (bytes->list packet)
    [(list #b10010000 remaining-length)
     (define var-header
       (read-bytes remaining-length
                   (connection-in conn)))
     (match (bytes->list var-header)
       [(list packet-id-msb packet-id-lsb qos-responses ...)
        'SUBACK:SUCCESS]
       [else
        'SUBACK:BAD-VAR-HEADER])]
    [else 'SUBACK:ERROR]))

(define (subscribe/ack c channel)
  (send-msg c (subscribe channel))
  (suback c))

;;;;;;;;;;;;;;;;;;;;;

(define mqtt%
  (class object%
    (init-field uri port 
                [id "racket"]
                [keepalive 60]
                [QoS 0]
                )
    
    ;; STATEFUL ASPECTS OF CONNECTION
    (define session-present false)
    (define connected? false)
    
    ;; INTERNAL STATE
    (define router-thread (make-parameter false))
    (define keepalive-thread (make-parameter false))
    (define <c> (new-mqtt-connection uri port))
    (define handlers (make-hash))
    
    (define (kill-all-threads)
      (set! connected? false)
      (for ([th (list (router-thread)
                      (keepalive-thread))])
        (when th
          (kill-thread th))))
    
    (define/public (start)
      (when (not connected?)
        ;; DO THE CONNECTION ON OBJECT CREATION
        (send-msg <c> (connect #:id id
                               #:keepalive keepalive
                               #:clean-session true))
        (set! connected? true)
        (debug "START: hash-length: ~a~n" (hash-count handlers))
        (for ([(ch fun) handlers])
          (debug "START: Subscribing to ~a~n" ch)
          (send-msg <c> (subscribe ch)))))
    
    (define/public (stop)
      (when connected?
        (disconnect <c>)
        (kill-all-threads)))
    
    
    (define/public (ping)
      (when connected?
        (send-msg <c> (pingreq))))
    
    (define/public (sub channel handler)
      (hash-set! handlers channel handler)
      (when connected?
        (send-msg <c> (subscribe channel))))
    
    (define/public (pub channel str)
      (when connected?
        (debug "PUB: ~a <- ~a~n" channel str)
        (define packet (publish channel str))
        (packet-dbg packet)
        (send-msg <c> packet)))
    
    (define (handle-server-msg conn header)
      (when connected?
        (cond
          [(eof-object? header)
           (kill-all-threads)
           'MQTT:DISCONNECTED]
          [else
           (define bls (bytes->list header)) 
           (match* ((first bls) (second bls))
             ;; PINGREQ
             [(#b11010000 #x00)
              'PINGRESP:OK]
             ;; SUBSCRIBE
             [(#b10010000 remaining-length)
              (define var-header
                (read-bytes remaining-length
                            (connection-in conn)))
              (match (bytes->list var-header)
                [(list packet-id-msb packet-id-lsb qos-responses ...)
                 'SUBACK:SUCCESS]
                [else
                 'SUBACK:BAD-VAR-HEADER])]
             ;; CONNACK
             [(#x20 #x02)
              (define-values (flags retcode)
                (apply values (bytes->list 
                               (read-bytes 2 (connection-in conn)))))
              (cond
                [(or (and session-present (= flags 1))
                     (and (not session-present) (= flags 0)))
                 (case retcode
                   [(#x00) 'CONNACK:ACCEPTED]
                   [(#x01) 'CONNACK:REFUSED]
                   [(#x02) 'CONNACK:IDENTIFIER-REJECTED]
                   [(#x03) 'CONNACK:SERVER-UNAVAILABLE]
                   [(#x04) 'CONNACK:BAD-USERNAME-OR-PASSWORD]
                   [(#x05) 'CONNACK:NOT-AUTHORIZED]
                   [else 'CONNACK:RESERVED]
                   )]
                [else 'CONNACK:BADFLAGS])]
             ;; PUBLISH
             [(flags remaining-length-b) 
              #:when (and (>= flags #b00110000)
                          (<= flags #b00111111))
              ;; FIXME: Flags are DUP, QoS, and RETAIN.
              ;; I should handle them.
              (define identifier-length 
                (integer-bytes->integer (read-bytes 2 (connection-in conn))
                                        false true))
              (define remaining-length
                (integer-bytes->integer (bytes 0 remaining-length-b)
                                        false true))
              (define content-length (- remaining-length identifier-length 2))
              
              (debug "PUB: READING ~a ~a~n" identifier-length content-length)
              (define incoming-channel-bytes 
                (read-bytes identifier-length (connection-in conn)))
              (define pub-content (read-bytes content-length (connection-in conn)))
              (debug "~a: ~a~n" incoming-channel-bytes pub-content)
              ;; Apply the handler to the published content
              ;; Handle wildcards
              (define incoming-channel (bytes->string/utf-8 incoming-channel-bytes))
              (for ([(chan fun) handlers])
                (when (regexp-match "#" chan)
                  (define pattern (regexp-replace "\\$"
                                                  (regexp-replace* "#" chan "*")
                                                  "\\\\$"))
                  (debug "Checking '~a' with '~a'~n" incoming-channel pattern)
                  (when (regexp-match pattern incoming-channel)
                    (debug "MATCHED~n")
                    (cond
                      [(= (procedure-arity fun) 1)
                       (fun pub-content)]
                      [(= (procedure-arity fun) 2)
                       (fun incoming-channel pub-content)])
                    )))
              
              ;; Or, a straight lookup
              (define fun (hash-ref handlers incoming-channel false))
              (when fun
                (cond
                  [(= (procedure-arity fun) 1)
                   (fun pub-content)]
                  [(= (procedure-arity fun) 2)
                   (fun incoming-channel pub-content)]))
              'SERVER:HANDLER-INVOKED
              ]
             [(else1 else2) 
              (debug "NO MATCH: ~a ~a~n" else1 else2)
              'SERVER:NOMATCH]
             )]
          )))
    
    (define (pinger)
      (keepalive-thread
       (thread (λ ()
                 (with-handlers ([exn:fail?
                                  (λ (e)
                                    (debug "P.ERR: ~a~n" e)
                                    (printf "Shutting down pinger.~n")
                                    (kill-all-threads))])
                   (let loop ()
                     (sleep (- keepalive (/ keepalive 10)))
                     (send this ping)
                     ;; Lets keep the server collected
                     (collect-garbage)
                     (debug "MEM: ~a~n" (current-memory-use))
                     (loop)))))))
    
    (define (router conn)
      (router-thread
       (thread (λ ()
                 (with-handlers ([exn:fail?
                                  (λ (e)
                                    (debug "R.ERROR: ~a~n" e)
                                    (printf "Shutting down router.~n")
                                    (kill-all-threads))])
                   (let loop ()
                     (sync (connection-in conn))
                     (define header (read-bytes 2 (connection-in conn)))
                     (define server-response  (handle-server-msg conn header))
                     (debug "SR: ~a~n" server-response)
                     (unless (equal? server-response 'MQTT:DISCONNECTED)
                       (loop))
                     ))))))
    
    ;; Launch under handlers that kill the threads.
    (router <c>)
    (pinger)
    
    (super-new)))
#|
(define local (new mqtt% 
                  [uri "localhost"]
                  [port 9999]
                  [id "alpha"]
                  [keepalive 5]))

(send local start)
(send local sub "$SYS/broker/bytes/received"
      (λ (v) (printf "L: ~a~n" v)))
|#

#|
(define global (new mqtt%
                    [uri "test.mosquitto.org"]
                    [port 1883]
                    [id "racket"]
                    [keepalive 10]))

(for ([ch (list "$SYS/broker/bytes/#"
                "$SYS/broker/clients/#"
                "$SYS/broker/messages/sent"
                )])
  (send global sub ch (λ (chan v) (printf "~a: ~a~n" chan v))))

(send global start)
(sleep 20)
(send global stop)
|#

(define-syntax mqtt-handlers
  (syntax-rules (config channels)
    [(_ (confg configs ...) (channels (channel function) ...))
     (let ([mqtt (new mqtt% configs ...)])
       (for ([ch (list channel ...)]
             [fun (list function ...)])
         (send mqtt sub ch fun))
       (send mqtt start)
       mqtt)]
    ))

