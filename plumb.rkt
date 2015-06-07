#lang racket
(require yaml
         file/zip
         net/url
         json
         "util.rkt"
         "config.rkt"
         "debug.rkt"
         "encoding.rkt")


(define (create-temp-dir)
  (make-directory* (build-path (conf-get 'tempdir) "sources"))
  (conf-add 'temp-sources (build-path (conf-get 'tempdir) "sources")))

(define (clear-temp-dir)
  ;; Remove the zipfile
  (for ([file (directory-list (conf-get 'tempdir))])
    (when (member (file-extension file) '("zip"))
      (delete-file (build-path (conf-get 'tempdir) file))))
  
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

(define (build-zipfile)
  (conf-add 'zipfile (build-path (conf-get 'tempdir) "occ.zip"))
  (zip-verbose true)
  (parameterize ([current-directory (conf-get 'tempdir)])
    (zip (extract-filename (conf-get 'zipfile))
         (conf-get 'temp-sources)
         ))
  )
   
(define (build-request-package)
  (define req (make-hash))
  (for ([file (directory-list (conf-get 'temp-sources))])
    (when (occam-file? file)
      (debug 'BUILD "Packing up file: ~a" (extract-filename file))
      ;; Add the file to the list of files being shipped.
      (hash-set! req 'files (cons (extract-filename file) (hash-ref req 'files empty)))
      ;; Load the file into the hash
      (hash-set! req (string->symbol (extract-filename file)) (file->string (build-path (conf-get 'temp-sources) file)))
      ))
  
  ;; Add additional metadata
  (hash-set! req 'main (conf-get 'source-file))
  (hash-set! req 'os (~a (system-type))) 
  
  ;; Return the request hash
  req)
    
(define (send-request req)
  (debug 'SEND "Request: ~n~a~n" req)
  (get-pure-port
     (make-server-url (conf-get 'server) (conf-get 'port)
                      "compile" 
                      (->json64 req))))

(define (post-request req)
  (debug 'POST "Request: ~n~a~n" req)
  (post-pure-port
     (make-server-url (conf-get 'server) (conf-get 'port)
                      "compile")
     (string->bytes/utf-8 (->json64 req))))
      
(define (read-response resp)
  (debug 'RAW (~a resp))
  (define decoded (b64-decode resp))
  (debug 'DECODED (~a decoded))
  (define parsed  (string->jsexpr decoded))
  (debug 'RAM (~a (hash-ref parsed 'RAM)))
  (if (= (hash-ref parsed 'code) CODE.OK)
      (values true (hash-ref parsed 'hex))
      (values false false)))
  
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
  ;; Send it. Save the response... it's a download URL for the hex.
  (define-values (success? hex-url)
    (let ()
      (define resp-port (post-request req))
      (define resp (read-all resp-port))
      (close-input-port resp-port)
      (debug 'PLUMB "Server Response: ~a" resp)
      (read-response resp)))
  
  (debug 'COMPILE "Hex URL: ~a" hex-url)
  ;; Pass the URL on to the next function.
  hex-url
  )

;; Defaults
(config-file "client.yaml")

(define plumb
  (command-line
   #:program "plumb"
   #:once-each 
   [("-d" "--debug") "Turn on verbose debugging."
                     (enable-debug! 'ALL)]
   
   [("--config") c
                 "Choose the client YAML config."
                 (config-file c)]
   
   #:args (file) ;; No command-line args
   (set-textual-debug)
   (load-config)
   
   (debug 'PLUMB "Server is at http://~a:~a/" 
          (conf-get 'server)
          (conf-get 'port))
   
   ;; Store the source file's path in the config.
   (conf-add 'source-path (extract-filedir (path->complete-path file)))
   (conf-add 'source-file file)
   
   (compile)
   ))
