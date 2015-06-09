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
(require yaml
         "util.rkt"
         "config.rkt"
         "debug.rkt"
         (prefix-in ccmds: "client-cmds.rkt")
         (prefix-in cmds: "cmds.rkt"))

 (define (driver)
  (define resp (ccmds:compile))
  (define outp (open-output-file "firmware.hex" #:exists 'replace))
  (fprintf outp (hash-ref resp "hex"))
  (close-output-port outp)
  (flush-ports)
  
   ;; Last step:
   ;; Config does not seem to be essential...
   ;; But will be on Windows, Mac.
   ;; avrdude -C /usr/share/arduino/hardware/tools/avrdude.conf \
   ;;         -V -F -P /dev/ttyUSB0 -p m328p -b 57600 -c arduino -U flash:w:firmware.hex:i
 
  (cmds:avrdude resp)
   )

  
;; Defaults
(config-file "client.yaml")

(define cmd-line-params (make-hash))

(define board-string
  (let ()
    (define boards (file->yaml "boards.yaml"))
    (map (λ (b) (hash-ref b "name")) boards)))

(define (get-board b)
  (define boards (file->yaml "boards.yaml"))
  (define result false)
  (for ([brd boards])
    (when (equal? b (hash-ref brd "name"))
      (set! result (hash-ref brd "params"))))
  result)

(set-textual-debug)

(define plumb
  (command-line
   #:program "plumb"
   #:once-each 
   [("-d" "--debug") "Turn on verbose debugging."
                     (enable-debug! 'ALL)]
   
   [("--config") c
                 "Choose the client YAML config."
                 (config-file c)]

   [("--board") board
                ("Choose the target board."
                 "Allowed options: "
                 (apply string-append
                        (map (λ (b) (format "\t* ~a~n" b)) board-string)))
                
                (define board-params (get-board board))
                (debug 'BOARDPARAMS (~s board-params))
                (hash-set! cmd-line-params "board" board-params)
                ]
   
   [("--serial") serial
                 "Arduino serial port."
                 (debug 'SERIAL "Setting serial port to: ~a~n" serial)
                 (hash-set! cmd-line-params "serial" serial)]
   
   ;; One file on the command line
   #:args (file)
   
   ;; Load our configuration (client.yaml by default)
   (load-config)
   
   ;; Merge the command-line params with the standing config.
   (for ([(k v) cmd-line-params])
     (conf-add k v))
   
   ;; Noisy. Announce our presence.
   (debug 'PLUMB "Server is at http://~a:~a/" 
          (conf-get 'server)
          (conf-get 'port))
   
   ;; Store the source file's path in the config.
   ;; I don't know if these are ever used.
   (conf-add 'source-path (extract-filedir (path->complete-path file)))
   (conf-add 'source-file file)
   
   ;; Drive the compilation.
   (driver)
   ))
