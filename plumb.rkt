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
         net/url
         json
         racket/serialize
         net/base64
         "util.rkt"
         "config.rkt"
         "debug.rkt"
         "encoding.rkt"
         (prefix-in cmds: "cmds.rkt"))


(define (create-temp-dir)
  (make-directory* (build-path (conf-get 'temp-dir) "sources"))
  (conf-add 'temp-sources (build-path (conf-get 'temp-dir) "sources")))

(define (copy-files-to-temp)
  (for ([file (directory-list (conf-get 'source-path))])
    (when (occam-file? file)
      (copy-file (build-path (conf-get 'source-path)
                             file)
                 (build-path (conf-get 'temp-sources)
                             file)))))

(define (build-request-package)
  (define req (make-hash))
  
  ;; Grab all the sources from the temporary directory.
  (for ([file (directory-list (extract-filedir (conf-get 'source-file)))])
    (set! file (build-path
                (extract-filedir (conf-get 'source-file))
                file))
    ;; When they are occam files (.occ, .inc. .module), 
    ;; slurp them up into the request.
    (when (occam-file? file)
      ;; Add the file to the list of files being shipped.
      (hash-set! req
                 "files"
                 (cons (extract-filename file) (hash-ref req "files" empty)))
      ;; Load the file into the hash. No special treatment;
      ;; we'll B64 it later.
      (hash-set! req
                 (extract-filename file) 
                 (file->string file))
      ))
  
  ;; Flag the main file so we know where to 
  ;; point the compiler.
  (hash-set! req "main" (extract-filename (conf-get 'source-file)))
  
  ;; In case we want to know the client OS later.
  (hash-set! req "os" (~a (system-type)))
  
  ;; FIXME MCJ 20150608
  ;; Attach the client config... we rely on it, somewhat, and
  ;; should probably remove that dependency.
  (hash-set! req "client-config" (config))
  
  ;; Return the request hash
  req)
    
(define (post-request req)
  (debug 'POST "Request: ~n~a~n" req)
  (post-pure-port
     (make-server-url (conf-get 'server) (conf-get 'port)
                      "compile")
     (base64-encode 
      (string->bytes/utf-8
       (~s (serialize req))))
     ))
      
(define (read-response resp)
  ;;(debug 'RAW (format "~a" resp))
  (define decoded (b64-decode resp))
  ;;(debug 'DECODED (~s decoded))
  (define parsed (deserialize (read (open-input-bytes decoded))))
  ;;(debug 'READRESPONSE (~s parsed))
  parsed)
  
(define (compile)
  ;; Create a temp directory
  ;; Also not necessary?
  ;;(create-temp-dir)
  
  ;; Copy all of the .occ, .module, and .inc files to that dir.
  ;; Not necessary?
  ;;(copy-files-to-temp)
  
  ;; Build the hash
  (define req (build-request-package))
  
  (debug 'COMPILE "Sending request.")
  
  ;; Send it. Save the response... 
  (define resp
    (let ()
      (define resp-port (post-request req))
      (define r (read-all resp-port))
      (close-input-port resp-port)
      (read-response r)))
  
  ;; We're done here. The next step is to handle the result of
  ;; what the server has handed us.
  ;;(debug 'SERVERRESPONSE (~s resp))
  resp
  )



(define (driver)
  (define resp (compile))
  (define outp (open-output-file "firmware.hex" #:exists 'replace))
  (fprintf outp (hash-ref resp "hex"))
  (close-output-port outp)
  (flush-ports)
  
  (cmds:avrdude resp)
  ;; Last step:
  ;; Config does not seem to be essential...
  ;; avrdude -C /usr/share/arduino/hardware/tools/avrdude.conf -V -F -P /dev/ttyUSB0 -p m328p -b 57600 -c arduino -U flash:w:firmware.hex:i
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
