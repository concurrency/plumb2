;; The MIT License (MIT)
;; 
;; Copyright (c) 2015 Matthew C. Jadud
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang racket
(require "util.rkt"
         "debug.rkt"
         "syscalls.rkt"
         "config.rkt")

(provide compile
         binhex
         output-exists?
         exe-in-session 
         compile-cmd
         compile
         plink
         plinker-cmd
         binhex-cmd)

;; RUNNING COMMANDS
;; We'll define commands as S-expressions. The language
;; looks like
;; (cmd -flag0 (-flag1 value1) (= -p1 value2))
;; which becomes
;; "cmd -flag0 -flag1 value1 -p1=value2"
;; Note we don't insert hyphens, but we make sure
;; spaces come out right.


     

(define (compile session-dir cmd)
  (parameterize ([current-directory (build-path (conf-get "temp-dir") session-dir)])
    (unless (directory-exists? (current-directory))
      (make-directory* (current-directory)))
    
    (debug 'COMPILE "Current directory: ~a~n" (current-directory))
    (debug 'COMPILE "~n****~n~a~n****~n" cmd)
           
    (let-values ([(stdout stdin pid stderr control)
                  (apply values (process cmd))])
      (define result (make-hash))
      
      (let loop ([status (control 'status)])
        (case status
          [(running) (sleep 1) (loop (control 'status))]
          [(done-ok) 
           ;; FIXME Some kind of response
           (hash-set! result "code" 200)
           ]
          [(done-error)
           (let ([err-msg (read-all stdout)]
                 [details (read-all stderr)])
             (close-input-port stdout)
             (close-input-port stderr)
             (close-output-port stdin)
             (control 'kill)
             ;; FIXME Build an error response
             (hash-set! result "err-msg" err-msg)
             (hash-set! result "details" details)
             (hash-set! result "code" 500)
             )]))
      
      (debug 'COMPILERESULT (~a result))
      result
      )))
    
;; NEED TO PARAMETERIZE
#|
 avr-occbuild --program fastblink.occ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/arch/common/ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/arch/m328p/  
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/platforms/arduino
-D F.CPU=16000000
|#

;; FIXME : check the defaults in the hash-gets...
(define (compile-cmd config main)
  (system-call
   (conf-get "occbuild")
   `(--search ,(conf-get "include")
              --search 
              ,(build-path (conf-get "include") "arch" "common")
              --search 
              ,(build-path (conf-get "include") "arch" 
                           (hash-ref config "cpu" "m328p"))
              --search 
              ,(build-path (conf-get "include") "platforms" 
                           (hash-ref config "platform" "arduino"))
              -D ,(format "F.CPU=~a" (hash-ref config 'mhz "16000000"))
              ;; --program needs to come last
              --program ,main)))

(define (output-exists? session-dir names ext)
  (parameterize ([current-directory (build-path (conf-get 'temp-dir) session-dir)])
    (file-exists? (hash-ref names ext))))

(define (plink config session-id names)
  (define result 
    (make-parameter 
     (exe-in-session 
      config
      session-id 
      (plinker-cmd config 
                   (hash-ref names 'tce)
                   (hash-ref names 'tbc)))))

  (cond
    [(zero? (result)) 
     ;; FIXME Some kind of response
     'OK
     ]
    [else (error)]))
  

(define (plinker-cmd config tce tbc)
  (system-call
   (send (config) get-config 'LINKER)
   `(-s -o ,tbc
        ,(->string ((send (config) get-config 'occam-lib-path) 'forall))
        ,tce)))

(define (binhex config session-id names)
  (define result
    (make-parameter
     (exe-in-session config session-id (binhex-cmd config names))))
  (cond
    [(zero? (result)) 
     ;;FIXME Some kind of response
     'OK
     ]
    [else (error)]))

;;FIXME : Magic Number (bytecode location)
(define (binhex-cmd config names)
  (system-call
   (send (config) get-config 'BINHEX)
   `(,(number->string (hash-ref (send (config) get-config 'BOARD) 'start-address) 16)
     ,(hash-ref names 'tbc) 
     ,(hash-ref names 'hex))))