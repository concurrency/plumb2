;; The MIT License (MIT)
;; 
;; Copyright (c) 2015 Matthew C. Jadud
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
         yaml
         racket/serialize
         net/base64
         ;; json
         ;; racket/date
         "debug.rkt"
         "config.rkt"
         "encoding.rkt"
         "util.rkt"
         (prefix-in cmds: "cmds.rkt")
         )

(define (ping req)
   ;; Encode it with an 'OK and send it back.
  (encode-response #hash((pong . "Hello"))
                   ))

(define compile-counter
  (let ([c 0])
    (λ ()
      (set! c (add1 c))
      c)))

(define (compile-driver req)
  ;; Clean up the request
  (define b64 (request-post-data/raw req))
  (debug 'RAW (~s b64))
  (define decoded (base64-decode b64))
  (debug 'DECODED (~s decoded))
  (define parsed
    (deserialize 
     (read (open-input-string 
            (bytes->string/utf-8 decoded)))))
  
  ;; Copy so it is mutable
  (define conf (make-hash))
  (for ([(k v) parsed])
    (hash-set! conf k v))
  
  (debug 'PARSED (~s parsed))
  
  (define response (make-hash))
  ;; Part of my debugging exploration...
  ;; It seems collecting garbage keeps useage down. Racket probably
  ;; does a collection as needed, so this isn't strictly necessary.
  ;; 1000 sequential requests and no apparent file/memory leaks.
  (collect-garbage)
 
  (hash-set! conf "RAM" (current-memory-use))
  (hash-set! conf "seq" (compile-counter))
  (hash-set! conf "start" (current-milliseconds))
  (hash-set! conf "session-id" (make-id 4))
  
  ;; Compile the file.
  (define compile-result (cmds:compile conf))

  (define resp false)

  (cond
    [(equal? (hash-ref compile-result "code") 200)
     ;; Link the file
     (define link-result (cmds:plink conf))
     
     ;; Binhex the file.
     (define binhex-result (cmds:binhex conf))
     
     (hash-set! conf "end" (current-milliseconds))
     (hash-set! conf "result" (~s compile-result))
     (set! resp (encode-response conf))
     ]
    [else
     (set! resp (encode-response compile-result))])
  resp
  )


(define-values (dispatch blog-url)
  (dispatch-rules
   [("ping") ping]
   [("compile") #:method "post" compile-driver]
   
   #|
   [("log" (string-arg) (string-arg)) 'client-log]
   [("start-session") 'return-session-id]
   [("add-file" (string-arg)) 'add-file]
   [("compile" (string-arg) (string-arg) (string-arg)) 'guarded-compile-session]
   ;; Need guards and session ID checks on retrieve.
   [("board" (string-arg)) 'retrieve-board-config]
   [("firmware" (string-arg)) 'retrieve-firmware]
|#   
   ))

(define (serve)
  (with-handlers ([exn:fail? 
                   (lambda (e) 
                     (debug 'SERVEFAIL "We died: ~a" e)
                     'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? false
                   #:port (conf-get 'port)
                   #:listen-ip (conf-get 'server)
                   #:server-root-path (current-directory)
                   #:extra-files-paths 
                   (list 
                    (build-path (current-directory) "static"))
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                   ;; Will this help with leaks?
                   #:stateless? true
                   )))

;; Defaults
(config-file "server.yaml")

(define occsrv
  (command-line
   #:program "occsrv"
   #:once-each 
   [("-d" "--debug") "Turn on verbose debugging."
                     (enable-debug! 'ALL)]
   
   
   [("--config") c
                 "Choose the server YAML config."
                 (config-file c)]
   #:args () ;; No command-line args
   (set-textual-debug)
   (load-config)
   (debug 'SERVER "Listening on port ~a" (conf-get 'port))
   (serve)
   ))
