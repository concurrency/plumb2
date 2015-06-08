#lang racket
(require yaml
         file/zip
         net/url
         json
         racket/serialize
         net/base64
         "util.rkt"
         "config.rkt"
         "debug.rkt"
         "encoding.rkt")


(define (create-temp-dir)
  (make-directory* (build-path (conf-get 'temp-dir) "sources"))
  (conf-add 'temp-sources (build-path (conf-get 'temp-dir) "sources")))

(define (clear-temp-dir)
  ;; Remove the zipfile
  (for ([file (directory-list (conf-get 'temp-dir))])
    (when (member (file-extension file) '("zip"))
      (delete-file (build-path (conf-get 'temp-dir) file))))
  
  ;; Remove temporary sources
  (for ([file (directory-list (conf-get 'temp-sources))])
    (when (occam-file? file)
      (delete-file (build-path (conf-get 'temp-sources) file)))))

(define (copy-files-to-temp)
  (for ([file (directory-list (conf-get 'source-path))])
    (when (occam-file? file)
      (copy-file (build-path (conf-get 'source-path)
                             file)
                 (build-path (conf-get 'temp-sources)
                             file)))))

(define (build-request-package)
  (define req (make-hash))
  (for ([file (directory-list (conf-get 'temp-sources))])
    (when (occam-file? file)
      (debug 'BUILD "Packing up file: ~a" (extract-filename file))
      ;; Add the file to the list of files being shipped.
      (hash-set! req "files" (cons (extract-filename file) (hash-ref req "files" empty)))
      ;; Load the file into the hash
      (hash-set! req (extract-filename file) (file->string (build-path (conf-get 'temp-sources) file)))
      ))
  
  ;; Add additional metadata
  (hash-set! req "main" (extract-filename (conf-get 'source-file)))
  (hash-set! req "os" (~a (system-type))) 
  ;; Send ALL the local config data.
  (hash-set! req "client-config" (config))
  
  ;; Return the request hash
  req)
    
(define (post-request req)
  (debug 'POST "Request: ~n~a~n" req)
  (post-pure-port
     (make-server-url (conf-get 'server) (conf-get 'port)
                      "compile")
     ;;(string->bytes/utf-8 (->json64 req))
     (base64-encode 
      (string->bytes/utf-8
       (~s (serialize req))))
     ))
      
(define (read-response resp)
  (debug 'RAW (format "~a" resp))
  (define decoded (b64-decode resp))
  (debug 'DECODED (~s decoded))
  ;;(define parsed  (string->jsexpr decoded))
  (define parsed (deserialize (read (open-input-bytes decoded))))
  (debug 'READRESPONSE (~s parsed))
  parsed)
  
(define (compile)
  
 ;; Create a temp directory
  (create-temp-dir)
  ;; Clear files from it.
  ;; Should only be .occ, .inc, and .module files.
  (clear-temp-dir)
  
  ;; Copy all of the .occ, .module, and .inc files to that dir.
  (copy-files-to-temp)
  
  ;; Build the hash
  (define req (build-request-package))
  ;; Flag the main file in the hash table.
  
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
  (debug 'SERVERRESPONSE (~s resp))
  resp
  )


(define (flush-ports)
  (flush-output (current-output-port))
  (flush-output (current-error-port)))

(define (driver)
  (define resp (compile))
  (define outp (open-output-file "firmware.hex" #:exists 'replace))
  (fprintf outp (hash-ref resp "hex"))
  (close-output-port outp)
  (flush-ports)
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
   #|
   [("--cpu") cpu
              "Choose the cpu we're compiling for."
              (hash-set! cmd-line-params 'cpu cpu)]
   
   [("--mhz") mhz
              "Choose the target's speed in MHz."
              (hash-set! cmd-line-params 'mhz mhz)]
   |#
   [("--board") board
                ("Choose the target board."
                 "Allowed options: "
                 (apply string-append
                        (map (λ (b) (format "\t* ~a~n" b)) board-string)))
                
                (define board-params (get-board board))
                (debug 'BOARDPARAMS (~s board-params))
                (hash-set! cmd-line-params "board" board-params)
                ]
   
   #:args (file) ;; No command-line args
   
   (load-config)
   
   (for ([(k v) cmd-line-params])
     (conf-add k v))
   
   (debug 'PLUMB "Server is at http://~a:~a/" 
          (conf-get 'server)
          (conf-get 'port))
   
   ;; Store the source file's path in the config.
   (conf-add 'source-path (extract-filedir (path->complete-path file)))
   (conf-add 'source-file file)
   
   (driver)
   ))
