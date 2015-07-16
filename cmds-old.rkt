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
         "config.rkt"
         "constants.rkt")

(provide compile
         binhex
         output-exists?
         exe-in-session 
         compile
         plink
         ihexmerge
         )

;; RUNNING COMMANDS
;; We'll define commands as S-expressions. The language
;; looks like
;; (cmd -flag0 (-flag1 value1) (= -p1 value2))
;; which becomes
;; "cmd -flag0 -flag1 value1 -p1=value2"
;; Note we don't insert hyphens, but we make sure
;; spaces come out right.


     

;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                                                                       
;      ;;;;;     ;;;;     ;;;      ;;;  ;;;;;;;   ;;  ;;       ;;;;;;;; 
;     ;;   ;   ;;;  ;;    ;;;      ;;;  ;;    ;;  ;;  ;;       ;;       
;    ;;        ;     ;;   ;;;     ;;;;  ;;     ;  ;;  ;;       ;;       
;   ;;        ;;      ;;  ;;;;    ; ;;  ;;     ;  ;;  ;;       ;;       
;   ;;        ;;      ;;  ;; ;    ; ;;  ;;    ;;  ;;  ;;       ;;       
;   ;;        ;       ;;  ;; ;;  ;; ;;  ;;;;;;;   ;;  ;;       ;;;;;;;  
;   ;;        ;;      ;;  ;;  ;  ;  ;;  ;;        ;;  ;;       ;;       
;   ;;        ;;      ;;  ;;  ;  ;  ;;  ;;        ;;  ;;       ;;       
;    ;;        ;     ;;   ;;  ;;;   ;;  ;;        ;;  ;;       ;;       
;     ;;   ;   ;;;  ;;    ;;   ;;   ;;  ;;        ;;  ;;       ;;       
;      ;;;;;     ;;;;     ;;   ;;   ;;  ;;        ;;  ;;;;;;;  ;;;;;;;; 
;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                                                                       

(define (compile conf)
  (debug 'COMPILE "***** COMPILE *****")
  (define cmd (compile-cmd conf (hash-ref conf "main")))
  (define session-dir (hash-ref conf "session-id"))
  
  (parameterize ([current-directory (build-path (conf-get "temp-dir") session-dir)])
    
    ;; Give us a place to put everything.
    (unless (directory-exists? (current-directory))
      (make-directory* (current-directory)))
    
    ;; Dump all the files in the configuration
    (for ([file (hash-ref conf "files")])
      (with-output-to-file
          (build-path (current-directory) file)
        (λ ()
          (printf "~a" (hash-ref conf file)))))
    
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
           (hash-set! result "code" OK.COMPILE)
           ]
          [(done-error)
           (let ([err-msg (read-all stdout)]
                 [details (read-all stderr)])
             (close-input-port stdout)
             (close-input-port stderr)
             (close-output-port stdin)
             (control 'kill)
             ;; Build an error response
             (hash-set! result "err-msg" err-msg)
             (hash-set! result "details" details)
             (hash-set! result "code" ERR.COMPILE)
             
             (debug 'COMPILE:ERR "msg: ~a~ndetails: ~a~n" err-msg details)
             
             ;; Clean up the temporary space.
             (debug 'DELETE "Deleting ~a" session-dir)
             ;; (delete-directory/files (build-path (conf-get "temp-dir") session-dir))
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
     ,@(map (λ (lib)
              `(--search ,(build-path (conf-get "include") lib)))
            '("convert" "forall" "hostio" "hostsp" "maths" "streamio" "string")) 
     --search ,(build-path (conf-get "include") "plumbing")
     --search 
     ,(build-path (conf-get "include") 
                  "plumbing" "arch" "common")
     --search 
     ,(build-path (conf-get "include") 
                  "plumbing" "arch" 
                  (hash-ref config "cpu" "m328p"))
     --search 
     ,(build-path (conf-get "include") 
                  "plumbing" "platforms" 
                  (hash-ref config "platform" "arduino"))
     -D ,(format "F.CPU=~a" (hash-ref config 'mhz "16000000"))
     ;; --program needs to come last
     --program ,main)))

(define (output-exists? session-dir names ext)
  (parameterize ([current-directory (build-path (conf-get 'temp-dir) session-dir)])
    (file-exists? (hash-ref names ext))))

(define (plink conf)
  (define result (exe-in-session conf (plinker-cmd conf)))
  (cond
    [(zero? result) 
     (make-hash `(("code" . OK.LINK)))]
    [else 
     (make-hash `(("code" . ERR.LINK)))])
  )
  

(define (replace-extension file orig new)
  (regexp-replace (format "~a$" orig) (extract-filename file) new))



;                                                                   
;                                                                   
;                                                                   
;                                                                   
;                                                                   
;                                                                   
;                                                                   
;                                                                   
;   ;;;;;;;   ;;       ;;  ;;;    ;;  ;;     ;;  ;;;;;;;;  ;;;;;;;  
;   ;;    ;;  ;;       ;;  ;;;    ;;  ;;    ;;   ;;        ;;    ;; 
;   ;;     ;  ;;       ;;  ;;;;   ;;  ;;   ;;    ;;        ;;     ; 
;   ;;     ;  ;;       ;;  ;; ;   ;;  ;;  ;;     ;;        ;;     ;;
;   ;;    ;;  ;;       ;;  ;; ;;  ;;  ;; ;;      ;;        ;;    ;; 
;   ;;;;;;;   ;;       ;;  ;;  ;  ;;  ;;;;;;     ;;;;;;;   ;;;;;;;  
;   ;;        ;;       ;;  ;;  ;; ;;  ;;;  ;;    ;;        ;;   ;   
;   ;;        ;;       ;;  ;;   ; ;;  ;;   ;;    ;;        ;;   ;;  
;   ;;        ;;       ;;  ;;   ;;;;  ;;    ;;   ;;        ;;    ;  
;   ;;        ;;       ;;  ;;    ;;;  ;;     ;;  ;;        ;;    ;; 
;   ;;        ;;;;;;;  ;;  ;;    ;;;  ;;      ;; ;;;;;;;;  ;;     ;;
;                                                                   
;                                                                   
;                                                                   
;                                                                   
;                                                                   

(define (plinker-cmd conf)
  (define tbc (replace-extension (hash-ref conf "main") "occ" "tbc"))
  (define tce (replace-extension (hash-ref conf "main") "occ" "tce"))
  (system-call
   (conf-get "linker")
   `(-s -o ,tbc
        ,(->string (build-path (conf-get "include") "forall" "forall.lib"))
        ,tce)))

(define (binhex conf)
  (define result (exe-in-session conf (binhex-cmd conf)))
  (cond
    [(zero? result) 
     (make-hash `(("code" . OK.BINHEX)))]
    [else 
     (make-hash `(("code" . ERR.BINHEX)))])
  )



;                                                            
;                                                            
;                                                            
;                                                            
;                                                            
;                                                            
;                                                            
;                                                            
;   ;;;;;;;    ;;  ;;;    ;;  ;;     ;;  ;;;;;;;;  ;;     ;; 
;   ;;    ;;   ;;  ;;;    ;;  ;;     ;;  ;;        ;;    ;;  
;   ;;     ;   ;;  ;;;;   ;;  ;;     ;;  ;;         ;;  ;;   
;   ;;     ;   ;;  ;; ;   ;;  ;;     ;;  ;;          ;; ;    
;   ;;    ;;   ;;  ;; ;;  ;;  ;;     ;;  ;;           ;;;    
;   ;;;;;;;    ;;  ;;  ;  ;;  ;;;;;;;;;  ;;;;;;;      ;;     
;   ;;     ;   ;;  ;;  ;; ;;  ;;     ;;  ;;          ;;;;    
;   ;;     ;;  ;;  ;;   ; ;;  ;;     ;;  ;;          ;  ;;   
;   ;;     ;;  ;;  ;;   ;;;;  ;;     ;;  ;;         ;;   ;;  
;   ;;    ;;   ;;  ;;    ;;;  ;;     ;;  ;;        ;;     ;  
;   ;;;;;;;    ;;  ;;    ;;;  ;;     ;;  ;;;;;;;; ;;      ;;;
;                                                            
;                                                            
;                                                            
;                                                            
;                                                            

;;FIXME : Magic Number (bytecode location)
(define (binhex-cmd conf)
  (define tbc (replace-extension (hash-ref conf "main") "occ" "tbc"))
  (define hex (replace-extension (hash-ref conf "main") "occ" "hex"))
  (system-call
   (conf-get "binhex")
   `(,(format "0x~a" (hash-ref (hash-ref (hash-ref conf "client-config") "board") "start-address"))
     ,tbc
     ,hex)))


;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             
;   ;;  ;;     ;;  ;;;;;;;;  ;;     ;;  ;;;      ;;;  ;;;;;;;;  ;;;;;;;      ;;;;;   ;;;;;;;; 
;   ;;  ;;     ;;  ;;        ;;    ;;   ;;;      ;;;  ;;        ;;    ;;    ;;   ;   ;;       
;   ;;  ;;     ;;  ;;         ;;  ;;    ;;;     ;;;;  ;;        ;;     ;   ;;        ;;       
;   ;;  ;;     ;;  ;;          ;; ;     ;;;;    ; ;;  ;;        ;;     ;; ;;         ;;       
;   ;;  ;;     ;;  ;;           ;;;     ;; ;    ; ;;  ;;        ;;    ;;  ;;         ;;       
;   ;;  ;;;;;;;;;  ;;;;;;;      ;;      ;; ;;  ;; ;;  ;;;;;;;   ;;;;;;;   ;;         ;;;;;;;  
;   ;;  ;;     ;;  ;;          ;;;;     ;;  ;  ;  ;;  ;;        ;;   ;    ;;   ;;;;  ;;       
;   ;;  ;;     ;;  ;;          ;  ;;    ;;  ;  ;  ;;  ;;        ;;   ;;   ;;     ;;  ;;       
;   ;;  ;;     ;;  ;;         ;;   ;;   ;;  ;;;   ;;  ;;        ;;    ;    ;;    ;;  ;;       
;   ;;  ;;     ;;  ;;        ;;     ;   ;;   ;;   ;;  ;;        ;;    ;;    ;;   ;;  ;;       
;   ;;  ;;     ;;  ;;;;;;;; ;;      ;;; ;;   ;;   ;;  ;;;;;;;;  ;;     ;;    ;;;;;   ;;;;;;;; 
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             

(define (ihexmerge conf)
  (define result (ihexmerge-cmd conf))
  (hash-set! result "code" OK.IHEXMERGE)
  result)

(define (ihexmerge-cmd conf)
  (parameterize ([current-directory (build-path (conf-get "temp-dir") 
                                                (hash-ref conf "session-id"))])
    (define tvm (build-path
               (conf-get "install")
               "firmwares"
               (hash-ref (hash-ref (hash-ref conf "client-config") "board") "firmware")))
    
    (debug 'IHEX "Firmware path: ~a" tvm)
    
  (define hex 
    (replace-extension (hash-ref conf "main") "occ" "hex"))
 
    (debug 'IHEX "Target hex to merge: ~a" hex)
    
  (define cmd
    (system-call
     (conf-get "ihexmerge")
     `(,tvm
       ,hex
       "> plumbware.hex")))
  
  (define result (make-hash))
  
  (let-values ([(stdout stdin pid stderr control)
                (apply values (process cmd))])
    (let loop ([status (control 'status)])
        (case status
          [(running) 
           (debug 'IHEX "STATE: ~a" status)
           (sleep 1) 
           (loop (control 'status))]
          [(done-ok) 
           (debug 'IHEX "STATE: ~a" status)
           (let (#;[hex (read-all stdout)])
             (debug 'IHEX "STATE: ~a" status)
             (close-input-port stdout)
             (close-input-port stderr)
             (close-output-port stdin)
             (control 'kill)
             ;; Build an ok response
             (define hex (file->string "plumbware.hex"))
             (debug 'IHEX "Read plumbware: ~a bytes" (string-length hex))
             (hash-set! result "hex" hex)
             (hash-set! result "code" OK.IHEXMERGE)
             
             ;; Clean up the temporary space.
             ;;(debug 'DELETE "Deleting ~a" session-dir)
             ;; (delete-directory/files (build-path (conf-get "temp-dir") session-dir))
             )
           ]
          [(done-error)
           (debug 'IHEX "STATE: ~a" status)
           (close-input-port stdout)
           (close-input-port stderr)
           (close-output-port stdin)
           (control 'kill)
           
           (hash-set! result "code" ERR.IHEXMERGE)
           ])))
  result))

;; avrdude -V -F -p atmega328p -c arduino -U flash:w:firmware.hex:i -b 57600 -P /dev/tty.usbserial-A901N6UH




  
