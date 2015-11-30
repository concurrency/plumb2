#lang racket
(require "mqtt.rkt")

(mqtt-debug true)

;; Signal.
(define mqtt (new mqtt%
                  [uri  "localhost"]
                  [port  8338]
                  [id "racket"]))

(send mqtt sub 
      "plumbing"
      (λ (msg) (printf "MSG:~n~a~n" msg)))

(send mqtt start)


