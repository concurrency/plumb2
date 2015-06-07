#lang racket
(require web-server/http
         net/base64
         json
         "debug.rkt"
         "util.rkt")

(provide (all-defined-out))

(define (->json64 o)
  (bytes->string/utf-8
   (b64-encode
    (jsexpr->string o))))

(define (encode-response json )
  (response/xexpr (->json64 json)))

(define CODE.OK 200)