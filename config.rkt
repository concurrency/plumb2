#lang racket
(require yaml
         "debug.rkt")

(provide (all-defined-out))

(define config-file (make-parameter false))
(define config (make-parameter false))

(define (conf-add key value)
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

