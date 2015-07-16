#lang racket
(require "mqtt.rkt")

(mqtt-debug true)

;; Signal.
(define mqtt (new mqtt%
                  [uri  "localhost"]
                  [port  8338]
                  [id "racket"]))

(send mqtt start)
(send mqtt pub
      "plumbing"
      (format "plumbware.hex - ~a" (random 1000)))
