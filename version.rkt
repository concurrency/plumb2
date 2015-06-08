#lang racket
(provide (all-defined-out))

(define VERSION "20150606")

(define (set-version! v)
  (set! VERSION (~a v)))
