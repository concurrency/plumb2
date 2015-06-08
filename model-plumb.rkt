;; The MIT License (MIT)
;;
;; Copyright (c) 2013 Matthew C. Jadud
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

(provide plumb%)

(require 
  net/dns
  yaml
  racket/runtime-path
  )

(require "arduino-interaction.rkt"
         "util.rkt"
         "mvc.rkt"
         "debug.rkt"
         "util.rkt"
         "cmds.rkt"
         "config.rkt"
         "version.rkt"
         )

;; To reference client.yaml
(define-runtime-path here (build-path "."))

(define MIN-FIRMWARE-SIZE 10000)
;(define CLIENT-CONF-ROOT "http://concurrency.cc/plumb/client-conf")
(define CLIENT-CONF-ROOT "http://45.55.232.116:8080/ide/conf")

(define plumb%
  (class model%
    (super-new)
    (inherit update add-view)
    
    ;   ;;;;;;; ;;  ;;;;;;;  ;;      ;;;;;;       ;;
    ;   ;;;;;;; ;;  ;;;;;;;  ;;      ;;;;;;;    ;;;;;
    ;   ;;      ;;  ;;       ;;      ;;    ;;   ;; ;
    ;   ;;      ;;  ;;       ;;      ;;     ;;  ;;
    ;   ;;;;;;  ;;  ;;;;;;   ;;      ;;      ;  ;;;
    ;   ;;;;;;  ;;  ;;;;;;   ;;      ;;      ;    ;;;
    ;   ;;      ;;  ;;       ;;      ;;      ;      ;;
    ;   ;;      ;;  ;;       ;;      ;;     ;;      ;;
    ;   ;;      ;;  ;;       ;;      ;;    ;;   ;   ;;
    ;   ;;      ;;  ;;       ;;;;;;; ;;;;;;;    ;; ;;;
    ;   ;;      ;;  ;;;;;;;  ;;;;;;; ;;;;;;      ;;;;
    
    (field [host false]
           [port false]
           [version 0]
           [id false]
           
           [config false]
           [board-config false]
           
           [arduino-ports empty]
           [arduino-port false]
           [board-type false]
           [board-mapping (make-hash)]
           
           [main-file false]
           [temp-dir false]
           [firmware-location false]
           
           [compilation-result false]
           [message "Parallel programming for makers."]
           [error-message ""]
           [error-line -1]
           
           [first-compilation? true]
           [first-check-or-compile? true]
           [examples-root false]
           )
    
    
    
    (define/public (get-error-line) error-line)
    
    (define/public (set-first-check-or-compile? b)
      (set! first-check-or-compile? b))
    
    (define/public (get-first-check-or-compile?)
      first-check-or-compile?)
    
    (define (client-conf str)
      (format "~a/~a" CLIENT-CONF-ROOT str))
    
    (define/public (get-host)
      host)
    (define/public (get-port)
      port)
    
       ;; Grab the host and port from the server
    (define/public (compilation-server-config)
      (with-handlers ([exn:fail? 
                       (λ (e)
                         ;(alert-dialog (->string e) 'exit)
                         (printf "EXITING: ~a~n" (->string e))
                         (exit)
                         )])
        
        (define client.yaml (file->yaml (build-path here "client.yaml")))
        
        (debug 'APP-LAUNCH "HOST CONFIG: ~a" client.yaml)
        (cond
          [(hash? client.yaml)
           (let ([host (hash-ref client.yaml "server")]
                 [port (hash-ref client.yaml "port")]
                 ;; [examples (hash-ref h 'examples)]
                 )
             (debug 'APP-LAUNCH "HOST ~a PORT ~a" host port)
             (send this set-remote-host host port)
             (set-version! (hash-ref client.yaml "version"))
             
             ;;(send this set-examples-root examples)
             )]
          [else (printf "~a~n" "Something went very wrong.")
                (exit)
                ])
        ))
    
    ;; For debugging
    (define/public (get-config key)
      (send config get-config key))    
    
    (define/public (set-examples-root str)
      (set! examples-root str))
    (define/public (get-examples-root) examples-root)
    
    
    (define/public (get-static #:as [as 'sexp] #:default [default "get-static error"] . args)
      (with-handlers ([exn:fail?
                       (λ (e)
                         ;(alert-dialog (->string e) 'exit)
                         (printf "EXITING: ~a~n" (->string e))
                         (exit)
                         )])
        (debug 'GET-STATIC "[~a] ~a" as args)
        (let ([slurper (case as
                         [(sexp) read]
                         [(text) (λ (p)
                                   (filter (λ (s) (>= (string-length s) 1))
                                           (regexp-split "\n" (port->string p))))])])
          (safe-url-fetch
           slurper
           (apply make-server-url
                  (append (list host port)
                          args))
           #:default '()
           ))))
    
    
    ;     ;;    ;;;;;;;    ;;      ;;    ;;     ;;;;     ;;      ;;    ;;
    ;   ;;;;;   ;;;;;;;  ;;;;;   ;;;;;   ;;   ;;;;;;;;   ;;;     ;;  ;;;;;
    ;   ;; ;    ;;       ;; ;    ;; ;    ;;  ;;;    ;;;  ;;;;    ;;  ;; ;
    ;   ;;      ;;       ;;      ;;      ;;  ;;      ;;  ;;;;;   ;;  ;;
    ;   ;;;     ;;;;;;   ;;;     ;;;     ;;  ;        ;  ;; ;;   ;;  ;;;
    ;     ;;;   ;;;;;;     ;;;     ;;;   ;;  ;        ;  ;;  ;;  ;;    ;;;
    ;       ;;  ;;           ;;      ;;  ;;  ;        ;  ;;   ;; ;;      ;;
    ;       ;;  ;;           ;;      ;;  ;;  ;;      ;;  ;;   ;;;;;      ;;
    ;   ;   ;;  ;;       ;   ;;  ;   ;;  ;;  ;;;    ;;   ;;    ;;;;  ;   ;;
    ;   ;; ;;;  ;;       ;; ;;;  ;; ;;;  ;;   ;;;;;;;;   ;;     ;;;  ;; ;;;
    ;    ;;;;   ;;;;;;;   ;;;;    ;;;;   ;;     ;;;;     ;;      ;;   ;;;;
    
    (define/public (get-id) id)
    
    (define (default-metadata)
      (b64-encode
       (format "~a,~a,~a,~a"
               (system-type)
               (system-language+country)
               (current-seconds)
               VERSION)))
    
    (define/public (say-hello)
     'FIXME
      )
    
    (define (get-new-session-id #:action [action false])
      'FIXME
      )
    
    (define/public (set-remote-host h p)
      (debug 'SET-REMOTE-HOST "Setting remote host for ~a and ~a" h p)
      (cond
        [(regexp-match "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" (format "~a" h))
         (debug 'SET-REMOTE-HOST "Found a dotted quad.")
         (set! host h)]
        [else
         (debug 'SET-REMOTE-HOST "Looking up DNS name.")
         (set! host (dns-get-address (dns-find-nameserver) h))])
      (set! port (string->number (~a p)))
      (update))
    
    
    ;   ;;;;;;     ;;;;     ;;;;;   ;;;;;;;;    ;;
    ;   ;;  ;;;  ;;;;;;;;   ;;;;;;; ;;;;;;;;  ;;;;;
    ;   ;;   ;; ;;;    ;;;  ;;   ;;    ;;     ;; ;
    ;   ;;   ;; ;;      ;;  ;;   ;;    ;;     ;;
    ;   ;;   ;; ;        ;  ;;   ;;    ;;     ;;;
    ;   ;;;;;;; ;        ;  ;;;;;;     ;;       ;;;
    ;   ;;;;;;  ;        ;  ;;;;;;     ;;         ;;
    ;   ;;      ;;      ;;  ;;  ;;     ;;         ;;
    ;   ;;      ;;;    ;;   ;;   ;;    ;;     ;   ;;
    ;   ;;       ;;;;;;;;   ;;    ;;   ;;     ;; ;;;
    ;   ;;         ;;;;     ;;    ;;;  ;;      ;;;;
    
    (define/public (enumerate-arduinos)
      (set! arduino-ports (map ->string (list-arduinos)))
      (update))
    
    (define/public (get-arduino-ports)
      arduino-ports)
    
    (define (port->platform-specific-port sp)
      (case (system-type)
        [(macosx unix) (format "/dev/~a" sp)]
        [(windows) sp]))
    
    (define/public (set-arduino-port p)
      (set! arduino-port (port->platform-specific-port p)))
    
    
    ;   ;;;;;;      ;;;;        ;;     ;;;;;    ;;;;;;
    ;   ;;  ;;    ;;;;;;;;      ;;     ;;;;;;;  ;;;;;;;
    ;   ;;  ;;;  ;;;    ;;;    ;;;;    ;;   ;;  ;;    ;;
    ;   ;;  ;;;  ;;      ;;    ;;;;    ;;   ;;  ;;     ;;
    ;   ;;  ;;   ;        ;    ;  ;;   ;;   ;;  ;;      ;
    ;   ;;;;;;;  ;        ;   ;;  ;;   ;;;;;;   ;;      ;
    ;   ;;   ;;; ;        ;   ;;   ;   ;;;;;;   ;;      ;
    ;   ;;   ;;; ;;      ;;  ;;;;;;;;  ;;  ;;   ;;     ;;
    ;   ;;   ;;; ;;;    ;;   ;;;;;;;;  ;;   ;;  ;;    ;;
    ;   ;;;;;;;   ;;;;;;;;   ;;     ;; ;;    ;; ;;;;;;;
    ;   ;;;;;;      ;;;;    ;;      ;; ;;    ;;;;;;;;;
    
    ;; Should be a list of strings.
    (define/public (get-board-choices)
      (filter string?
              (map (λ (s)
                     (let ([m (regexp-match #rx"\\s*(.*)\\s*:\\s*(.*)\\s*" s)])
                       (cond
                         [m
                          (let ([board (list-ref m 1)]
                                [mapping (list-ref m 2)])
                            (hash-set! board-mapping board mapping)
                            board)]
                         [else 'error])))
                   (get-static #:as 'text "ide" "board-choices.rkt"))))
    
    (define (board-choice->board-type choice)
      (define mapping (hash-ref board-mapping choice))
      (debug 'BC->BT "~a => ~a" choice mapping)
      mapping)
    
    (define/public (set-board-type b)
      (set! board-type (board-choice->board-type b)))
    
    (define/public (get-board-type) board-type)
    
    (define (get-board-config)
      'FIXME
      )
    
    
    ;   ;;;;;;; ;;  ;;      ;;;;;;;    ;;
    ;   ;;;;;;; ;;  ;;      ;;;;;;;  ;;;;;
    ;   ;;      ;;  ;;      ;;       ;; ;
    ;   ;;      ;;  ;;      ;;       ;;
    ;   ;;;;;;  ;;  ;;      ;;;;;;   ;;;
    ;   ;;;;;;  ;;  ;;      ;;;;;;     ;;;
    ;   ;;      ;;  ;;      ;;           ;;
    ;   ;;      ;;  ;;      ;;           ;;
    ;   ;;      ;;  ;;      ;;       ;   ;;
    ;   ;;      ;;  ;;;;;;; ;;       ;; ;;;
    ;   ;;      ;;  ;;;;;;; ;;;;;;;   ;;;;
    
    
    (define/public (get-temp-dir)
      temp-dir)
    
    (define/public (set-main-file f)
      (set! main-file f)
      (update))
    
    (define/public (get-main-file)
      main-file)
    
    (define/public (main-file-set?)
      (and main-file (file-exists? main-file)))
    
    ;; Subdir is typically id
    (define/public (create-temp-dir subdir)
      (debug 'CREATE-TEMP-DIR
             "Creating subdir [~a]" subdir)
      (set! temp-dir
            (case (->sym (system-type))
              [(macosx)
               (build-path (find-system-path 'temp-dir) subdir)]
              [(win windows)
               (let ([result (make-parameter false)])
                 (for ([p (map getenv '("TMP" "TEMP" "USERPROFILE"))])
                   (debug 'CREATE-TEMP-DIR "Checking for [~a]" p)
                   (when (and p
                              (directory-exists? p)
                              (not (result)))
                     (debug 'CREATE-TEMP-DIR
                            "Combining [~a] and [~a]"
                            p id)
                     (result (build-path p subdir))))
                 (debug 'CREATE-TEMP-DIR "Using [~a]" (result))
                 (result))]))
      (cond
        [(directory-exists? temp-dir)
         (debug 'CREATE-TEMP-DIR "Temp dir [~a] exists" temp-dir)]
        [else
         (debug 'CREATE-TEMP-DIR "Creating [~a]" temp-dir)
         (make-directory temp-dir)])
      temp-dir)
    
    (define (cleanup-temp-dir)
      (define extensions '(hex))
      (when (directory-exists? temp-dir)
        (for ([f (directory-list temp-dir)])
          (debug 'TEMP-DIR "Checking [~a] for removal." f)
          (when (member (->sym (file-extension f)) extensions)
            (debug 'TEMP-DIR "Removing [~a]." f)
            (delete-file (build-path temp-dir f))))
        (debug 'TEMP-DIR "Removing temp directory [~a]" temp-dir)
        (delete-directory temp-dir)
        (set! temp-dir false)))
    
    
    ;   ;;       ;;    ;;        ;;;;;    ;;
    ;   ;;;     ;;;  ;;;;;     ;;;  ;;; ;;;;;
    ;   ;;;;   ;;;;  ;; ;      ;      ; ;; ;
    ;   ;; ;; ;; ;;  ;;       ;         ;;
    ;   ;; ;; ;; ;;  ;;;      ;         ;;;
    ;   ;;  ;;;  ;;    ;;;    ;    ;;;;   ;;;
    ;   ;;   ;   ;;      ;;   ;      ;;     ;;
    ;   ;;       ;;      ;;   ;      ;;     ;;
    ;   ;;       ;;  ;   ;;   ;;     ;; ;   ;;
    ;   ;;       ;;  ;; ;;;    ;;;;;;;; ;; ;;;
    ;   ;;       ;;   ;;;;      ;;;;;;   ;;;;
    
    
    (define/public (get-message) message)
    (define/public (set-message m)
      (set! message m))
    
    
    (define/public (get-error-message) error-message)
    (define/public (set-error-message e)
      (set! error-message e)
      (update))
    
    (define/public (get-compilation-result) compilation-result)
    
    
    ;                   ;;   ;;;     ;;;;      ;; ;;;;;;;;     ;;     ;;     ;;;
    ;                 ;;;;;   ;;    ;; ;;;     ;; ;;;;;;;;     ;;      ;;   ;;
    ;                 ;; ;     ;;  ;;  ;;;;    ;;    ;;       ;;;;      ;;  ;;
    ;                 ;;        ;;;;   ;;;;;   ;;    ;;       ;;;;      ;;;;;
    ;          ;      ;;;        ;;;   ;; ;;   ;;    ;;       ;  ;;      ;;;
    ;          ;        ;;;      ;;    ;;  ;;  ;;    ;;      ;;  ;;      ;;;
    ;          ;          ;;     ;;    ;;   ;; ;;    ;;      ;;   ;      ;;;;
    ;          ;          ;;     ;;    ;;   ;;;;;    ;;     ;;;;;;;;    ;; ;;
    ;         ;       ;   ;;     ;;    ;;    ;;;;    ;;     ;;;;;;;;   ;;   ;;
    ;         ;       ;; ;;;     ;;    ;;     ;;;    ;;     ;;     ;; ;;     ;;
    ;         ;        ;;;;      ;;    ;;      ;;    ;;    ;;      ;;;;;      ;;
    ;    ;    ;
    ;   ; ;  ;
    ;      ; ;
    ;       ;;
    ;       ;;
    ;        ;
    
    (define (any? v) v)
    
    
    ;; FIXME
    ;; Need better checks down below
    
    (define/public (check-syntax)
      (when (send this get-first-check-or-compile?)
        (send this set-first-check-or-compile? false)
        ;; This loads things from Bitbucket.
        (load-error-regexps))
      (compile* 'check-syntax))
    
    
    (define error-regexps (make-parameter false))
    
    ;; Error-occ21-error.occ(5)- foo is not declared
    ;; foo undeclared on "error.occ" line 5
    
    (struct err-pat (name msg subs pattern parts) #:transparent)
    
    (define BASE "Error-occ21-(.*?)\\.occ\\(([0-9]+)\\)- ")
    (define BASE-PARTS '(filename line-number))
    
    (define-syntax-rule (e-p name format-str subs str ids ...)
      (err-pat (quote name)
               format-str
               (quote subs)
               (string-append BASE str)
               (append BASE-PARTS (list (quote ids) ...))))
    
    (error-regexps
     (list
      (err-pat 'default
               "Line ~a, ~a"
               '(line-number message)
               (string-append BASE "(.*?)$")
               (append BASE-PARTS '(message)))
      ))
    
    (define/public (load-error-regexps)
      (define gh-read
        (send this get-static #:as 'sexp "plumbing-syntax-errors" "errors.rkt"))
      
      (debug 'SYNTAX-ERROR-HANDLING "~a~n" gh-read)
      
      (let ([pats
             (map (λ (e)
                    (err-pat (->sym (first e))
                             (second e)
                             (map ->sym (third e))
                             (string-append BASE (fourth e))
                             (append BASE-PARTS (list (->sym (fifth e))))
                             ))
                  gh-read)])
        (debug 'SYNTAX-ERROR-HANDLING "~a" pats)
        (error-regexps pats)))
    
    ;; (struct err-pat (name msg subs pattern parts) #:transparent)
    (define (index-of o ls)
      (cond
        [(empty? ls) 10000]
        [(equal? (first ls) o) 0]
        [else
         (add1 (index-of o (rest ls)))]))
    
    (define (extract-part id match ep)
      (list-ref match
                (add1 (index-of id (err-pat-parts ep)))))
    
    (define (build-error-message ls)
      (let ([h (first ls)]
            [ep (second ls)]
            [line (third ls)])
        (let ([format-string (string-append "[~a, line ~a] " (err-pat-msg ep))]
              [field-order (err-pat-subs ep)]
              [match (regexp-match (err-pat-pattern ep) line)])
          (list
           (string->number (extract-part 'line-number match ep))
           (apply format
                  `(,format-string
                    ,@(append
                       (list (err-pat-name ep)
                             (extract-part 'line-number match ep))
                       (map (λ (id)
                              (extract-part id match ep))
                            (err-pat-subs ep))
                       ))))
          )))
    
    (define (process-error-message h)
      (define response (make-parameter false))
      (when (hash-has-key? h 'errormessage)
        (let ([msg (hash-ref h 'errormessage)])
          (for ([line (regexp-split "\n" (first msg))])
            (for ([ep (error-regexps)])
              (when (and (not (response))
                         (regexp-match (err-pat-pattern ep) line))
                (response (list h ep line))))))
        (build-error-message (response))
        ))
    
    
    
    
    ;   ;;;;;;; ;;  ;;;;;    ;;       ;; ;;     ;     ;;    ;;     ;;;;;    ;;;;;;;
    ;   ;;;;;;; ;;  ;;;;;;;  ;;;     ;;;  ;    ;;;    ;     ;;     ;;;;;;;  ;;;;;;;
    ;   ;;      ;;  ;;   ;;  ;;;;   ;;;;  ;;   ;;;   ;;    ;;;;    ;;   ;;  ;;
    ;   ;;      ;;  ;;   ;;  ;; ;; ;; ;;  ;;   ;;;   ;;    ;;;;    ;;   ;;  ;;
    ;   ;;;;;;  ;;  ;;   ;;  ;; ;; ;; ;;   ;  ;; ;;  ;     ;  ;;   ;;   ;;  ;;;;;;
    ;   ;;;;;;  ;;  ;;;;;;   ;;  ;;;  ;;   ;; ;; ;; ;;    ;;  ;;   ;;;;;;   ;;;;;;
    ;   ;;      ;;  ;;;;;;   ;;   ;   ;;   ;; ;; ;; ;;    ;;   ;   ;;;;;;   ;;
    ;   ;;      ;;  ;;  ;;   ;;       ;;   ;;;;   ;;;    ;;;;;;;;  ;;  ;;   ;;
    ;   ;;      ;;  ;;   ;;  ;;       ;;    ;;;   ;;;    ;;;;;;;;  ;;   ;;  ;;
    ;   ;;      ;;  ;;    ;; ;;       ;;    ;;;   ;;;    ;;     ;; ;;    ;; ;;
    ;   ;;      ;;  ;;    ;;;;;       ;;    ;;     ;;   ;;      ;; ;;    ;;;;;;;;;;
    
    (define (write-firmware)
      'FIXME
      )
    
    (define (upload-firmware)
      'FIXME
      )
    
    
    (define/public (user-upload-firmware)
      (debug 'FIRMWARE "User requested firmware upload.")
      'FIXME
      )
    
    ;      ;;;;       ;;;;     ;;       ;;  ;;;;;;  ;;  ;;      ;;;;;;;
    ;    ;;;;;;;;   ;;;;;;;;   ;;;     ;;;  ;;  ;;; ;;  ;;      ;;;;;;;
    ;   ;;;     ;  ;;;    ;;;  ;;;;   ;;;;  ;;   ;; ;;  ;;      ;;
    ;   ;;         ;;      ;;  ;; ;; ;; ;;  ;;   ;; ;;  ;;      ;;
    ;  ;;          ;        ;  ;; ;; ;; ;;  ;;   ;; ;;  ;;      ;;;;;;
    ;  ;;          ;        ;  ;;  ;;;  ;;  ;;;;;;; ;;  ;;      ;;;;;;
    ;  ;;          ;        ;  ;;   ;   ;;  ;;;;;;  ;;  ;;      ;;
    ;  ;;          ;;      ;;  ;;       ;;  ;;      ;;  ;;      ;;
    ;   ;;      ;  ;;;    ;;   ;;       ;;  ;;      ;;  ;;      ;;
    ;    ;;;  ;;;   ;;;;;;;;   ;;       ;;  ;;      ;;  ;;;;;;; ;;
    ;     ;;;;;;      ;;;;     ;;       ;;  ;;      ;;  ;;;;;;; ;;;;;;;
    
    (define (add-file file-path)
      'FIXME
      )
    
    
    (define (compile-main-file)
      'FIXME
      )
    
    
    (define (write-and-upload-code hex-hash)
      'FIXME
      )
    
    
    
    (define/public (compile)
      (compile* 'compile))
    
    (define (compile* flag)
     'FIXME
      )
    
    ))
