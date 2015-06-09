#lang racket
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

(require yaml
         json
         racket/serialize
         net/base64
         net/url)

(require "util.rkt"
         "debug.rkt"
         "syscalls.rkt"
         "config.rkt"
         "constants.rkt")

(provide compile)




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