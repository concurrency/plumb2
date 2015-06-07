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
         ;; web-server/http
         web-server/servlet-env
         yaml
         ;; net/base64
         ;; json
         ;; racket/date
         "debug.rkt"
         "config.rkt"
         "encoding.rkt"
         )

(define (ping req)
   ;; Encode it with an 'OK and send it back.
  (encode-response #hash((pong . "Hello"))
                   ))

(define (compile req json)
  (debug 'COMPILE "~a" json)
  (encode-response #hash((status . "OK")
                         (code . 200)))
  )

(define-values (dispatch blog-url)
  (dispatch-rules
   [("ping") ping]
   [("compile" (string-arg)) compile]
   
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
