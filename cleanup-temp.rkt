#lang racket

(require "config.rkt"
         racket/file
         racket/runtime-path)

;; To reference client.yaml
(define-runtime-path here (build-path "."))

(config-file (build-path here "server.yaml"))
(load-config)

(define (too-old d)
  (let ([s (file-or-directory-modify-seconds d)])
    (define one-hour (* 60 60))
    (define one-day (* 24 one-hour))
    (define two-days (* 2 one-day))
    (define one-week (* 7 one-day))
    (> s two-days)))
      

(for ([d (directory-list (conf-get 'temp-dir))])
  (define full-path (build-path (conf-get 'temp-dir) d))
  (when (directory-exists? full-path)
    (when (too-old full-path)
      ;(printf "Deleting: ~a~n" d)
      (delete-directory/files full-path)
      )))