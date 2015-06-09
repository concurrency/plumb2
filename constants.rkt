#lang racket
(provide (all-defined-out))

(define OK.COMPILE 'OK.COMPILE)
(define OK.LINK 'OK.LINK)
(define OK.BINHEX 'OK.BINHEX)
(define OK.IHEXMERGE 'OK.IHEXMERGE)
(define OK.AVRDUDE 'OK.AVRDUDE)

(define ERR.COMPILE 'ERR.COMPILE)
(define ERR.LINK 'ERR.LINK)
(define ERR.BINHEX 'ERR.BINHEX)
(define ERR.IHEXMERGE 'ERR.IHEXMERGE)
(define ERR.AVRDUDE 'ERR.AVRDUDE)


    
      (define positives '("Everything's groovy."
                              "Five-by-five on the Arduino."
                              "Super-freaky code is running on the Arduino."
                              "I'm running AMAZING code."
                              "You should be well chuffed."
                              "Good job."
                              "One giant program for Arduino kind."
                              "Help, I'm stuck in an Arduino factory!"))
    