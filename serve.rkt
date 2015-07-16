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
         racket/serialize
         net/base64
         web-server/dispatchers/dispatch
         racket/runtime-path
         ;; json
         ;; racket/date
         "debug.rkt"
         "config.rkt"
         "encoding.rkt"
         "util.rkt"
         "constants.rkt"
         (prefix-in cmds: "cmds.rkt")
         )

(define one-hour (* 60 60))
(define one-day (* 24 one-hour))
(define two-days (* 2 one-day))
(define one-week (* 7 one-day))
(define hold-temp-files-for two-days)

(define (ping req)
  ;; Encode it with an 'OK and send it back.
  (encode-response #hash((pong . "Hello"))
                   ))

(define compile-counter
  (let ([c 0])
    (λ ()
      (set! c (add1 c))
      c)))

(define (cleanup-temp-directory)
  
  (define (too-old d)
    (let ([s (file-or-directory-modify-seconds d)])
      (> s hold-temp-files-for)))
  
  (for ([d (directory-list (conf-get 'temp-dir))])
    (define full-path (build-path (conf-get 'temp-dir) d))
    (when (directory-exists? full-path)
      (when (too-old full-path)
        ;(printf "Deleting: ~a~n" d)
        (delete-directory/files full-path)
        ))))

(define (check-driver req)
  (debug 'CHECKDRIVER "Start check-driver.")
  
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
  
  ;; Also clean up the temp directory.
  (cleanup-temp-directory)
  
  (hash-set! conf "RAM" (current-memory-use))
  (hash-set! conf "seq" (compile-counter))
  (hash-set! conf "start" (current-milliseconds))
  (hash-set! conf "session-id" (make-id 4))
  
  ;; Compile the file.
  (define resp false)
  (define STATE 'compile)
  
  (let loop ([STATE 'compile]
             [result false])
    (case STATE
      [(compile)
       (define res (cmds:compile conf))
       (if (equal? (hash-ref res "code") OK.COMPILE)
           (loop 'done res)
           (loop 'error res))]
      [(done)
       (hash-set! conf "end" (current-milliseconds))
       (hash-set! conf "result" result)
       (set! resp (encode-response conf))]
      [(error)
       (set! resp (encode-response result))]))
  
  ;; Return the response
  resp
  )

(define (compile-driver req)
  (debug 'COMPILEDRIVER "Start compile-driver.")
  
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
  (define resp false)
  (define STATE 'compile)
  
  (let loop ([STATE 'compile]
             [result false])
    (debug 'COMPILEDRIVER "State: ~a" STATE)
    (case STATE
      [(compile)
       (debug 'COMPILE-DRIVER "STATE: ~a" STATE)
       (define res (cmds:compile conf))
       (debug 'COMPILE-DRIVER "RES:~n")
       (pretty-print res)
       (if (equal? (hash-ref res "code") OK.COMPILE)
           (loop 'link res)
           (loop 'error res))]
      [(link)
       (debug 'COMPILE-DRIVER "STATE: ~a" STATE)
       (define res (cmds:plink conf))
       (if (equal? (hash-ref res "code") OK.LINK)
           (loop 'binhex res)
           (loop 'error res))]
      [(binhex)
       (debug 'COMPILE-DRIVER "STATE: ~a" STATE)
       (define res (cmds:binhex conf))
       (if (equal? (hash-ref res "code") OK.BINHEX)
           (loop 'ihexmerge res)
           (loop 'error res))]
      [(ihexmerge)
       (debug 'COMPILE-DRIVER "STATE: ~a" STATE)
       (define res (cmds:ihexmerge conf))
       (if (equal? (hash-ref res "code") OK.IHEXMERGE)
           (loop 'done res)
           (loop 'error res))]
      [(done)
       (debug 'COMPILE-DRIVER "STATE: ~a" STATE)
       (hash-set! conf "end" (current-milliseconds))
       (hash-set! conf "result" result)
       (hash-set! conf "hex" (hash-ref result "hex"))
       (set! resp (encode-response conf))]
      [(error)
       (debug 'COMPILE-DRIVER "STATE: ~a" STATE)
       (debug 'COMPILE-DRIVER "ERROR:~n")
       (pretty-print resp)
       (set! resp (encode-response result))]))
  
  ;; Return the response
  resp
  )

(define (get-version req)
  (encode-response (conf-get "version")))

(define-values (dispatch blog-url)
  (dispatch-rules
   [("ping") ping]
   [("compile") #:method "post" compile-driver]
   [("check") #:method "post" check-driver]
   [("version") get-version]
   ;; This is necessary to serve static files, I think.
   [else (next-dispatcher)]
   ))


(define-runtime-path ide (build-path "ide"))
(define-runtime-path here (build-path "."))

(define (serve)
  (with-handlers ([exn:fail? 
                   (lambda (e) 
                     (debug 'SERVEFAIL "We died: ~a" e)
                     'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? false
                   #:port (conf-get 'port)
                   #:listen-ip (conf-get 'server)
                   ;;#:server-root-path (current-directory)
                   #:extra-files-paths (list here ide "htdocs")
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                   ;; Will this help with leaks?
                   #:stateless? true
                   )
    ))

;; Defaults
(config-file "server.yaml")
(define docker? false)

(define occsrv
  (command-line
   #:program "occsrv"
   #:once-each 
   [("-d" "--debug") "Turn on verbose debugging."
                     (enable-debug! 'ALL)]
   
   
   [("--config") c
                 "Choose the server YAML config."
                 (config-file c)]
   [("--docker") path-to-binaries 
                 "If running under Docker, set a binary path."
                 (set! docker? path-to-binaries)]
   
   #:args () ;; No command-line args
   (set-textual-debug)
   (load-config)
   (conf-add 'docker docker?)
   (debug 'SERVER "Listening on port ~a" (conf-get 'port))
   (serve)
   ))
