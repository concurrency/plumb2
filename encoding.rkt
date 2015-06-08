#lang racket
(require web-server/http
         net/base64
         json
         racket/serialize
         "debug.rkt"
         "util.rkt")

(provide (all-defined-out))

(define (->json64 o)
  (bytes->string/utf-8
   (b64-encode
    (jsexpr->string o))))

(define (encode-response json )
  (response/xexpr 
   (bytes->string/utf-8
    (b64-encode 
     (~s (serialize json))))))
