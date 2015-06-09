#lang racket
(require yaml
         racket/runtime-path
         "debug.rkt")

(provide (all-defined-out))

;; (define-runtime-path here (build-path "."))

(define config-file (make-parameter false))
(define config (make-parameter false))

(define (conf-add key value)
  (debug 'ADD (format "~a <- ~a" key value))
  (hash-set! (config) (format "~a" key) value))

(define (conf-get key)
  (hash-ref (config) (format "~a" key)))

(define (load-config)
  (config (file->yaml (config-file)))
  (show-config))

(define (show-config)
  ;; Show the config
  (for ([(k v) (config)])
    (debug 'CONFIG "~a <- ~a" k v)))

;; Used in avrdude in client-cmds.rkt
(define (show-conf conf)
  (for ([(k v) conf])
    (debug 'CONF "~a <- ~a"
           k
           (cond
             [(and (string? v) (> (string-length v) 30))
              (~a v #:max-width 30)]
             [(hash? v) 
              (debug 'CONF "~a~n---~n" k)
              (show-conf v)]
             [else
              (~a v)]))))