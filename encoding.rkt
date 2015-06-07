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
    (jsexpr->string (format "~a" o)))))

(define (encode-response json #:success? [success? true])
  (response/xexpr #:code (if success? 200 400)
                  (->json64 json)))
