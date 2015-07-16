#lang racket

;; for make-unique-name
(require ;; file/sha1
  json
  net/url
  net/base64
  "debug.rkt"
  )

(provide (all-defined-out))


(define (quote-path path)
  (debug 'QP "System type: ~a" (system-type))
  (cond
    [(path? path)
     (case (system-type)
       ;; FIXME
       ;; Might be a problem on the Mac as well.
       [(macosx) (path->string path)]
       [(windows)
        (format "\"~a\"" (path->string path))]
       [else (path->string path)])]
    [else
     (debug 'QP "Passing string '~a' straight through." path)
     path]))
     

(define (win-string-quote str)
  (if (equal? 'windows (system-type))
      (format "\"~a\"" str)
      str))

(define (flush-ports)
  (flush-output (current-output-port))
  (flush-output (current-error-port)))

;; CONTRACT :: (list-of any) any -> (list-of any)
;; Intersperses the object 'o' throughout the list.
(define (list-intersperse ls o)
  (cond
    [(empty? ls) ls]
    [(empty? (rest ls)) ls]
    [else
     (cons (first ls)
           (cons o 
                 (list-intersperse (rest ls) o)))]))

(define (extract-filename path)
  (define-values (base name dir?) (split-path path))
  (->string name))


(define (extract-filedir path)
  (define-values (base name dir?) (split-path path))
  (debug 'EFD "base: ~a~n" base)
  (if (equal? base 'relative)
    (simplify-path "./")
    (->string base)))


(define (occam-file? filename)
  (regexp-match #px"\\.(occ|inc|module)$" filename))

(define (hex-file? filename)
  (regexp-match #px"\\.hex$" filename))

(define (file-extension filename)
  (let ([m (regexp-match #px"^(.*)\\.(.*?)$" (extract-filename filename))])
    (cond
      [m
       (third m)]
      [else "EXTENSIONERROR"])))

;; CONTRACT :: any -> string
;; Converts any object to a string.
;; Potentially in an ugly way.
(define (->string o)
  (format "~a" o))

(define (b64-decode str)
  (base64-decode 
   (string->bytes/locale
    (regexp-replace* #px"_" (~a str) "/"))))

(define (b64-encode str)
  (regexp-replace* #px"/"
                   (base64-encode (string->bytes/utf-8 str))
                   "_"))


(define (json-encode h)
  (let ([os (open-output-string)])
    (write-json h os)
    (get-output-string os)))


(define (read-all port)
  (let ([content ""])
    (let ([ip port])
      (let loop ([line (read-line ip)])
        (unless (eof-object? line)
          (set! content (format "~a~a~n" content line))
          (loop (read-line ip))))
      (close-input-port ip)
      )
    content))

(define (read-url str)
  (debug 'READURL "Reading all from: ~a~n" str)
  (read-all (get-pure-port 
             (string->url str)
             (list "User-Agent: PLT Racket/6.0.0 (Plumb)"))))

(define (strip str)
  (for ([pat '("^[ ]+" "[ ]+$" "\n" "\r")])
    (set! str (regexp-replace pat str "")))
  str)

(define (snoc ls o)
  (reverse (cons o (reverse ls))))

(define (->sym v)
  (string->symbol (format "~a" v)))


(define (safe-url-fetch reader #:default [default-message ""] url-string )
  (debug 'SUF "Fetching URL-STRING ~a" url-string)
  (let ([result ""])
    (with-handlers ([exn? (λ (e)
                            (debug 'SUF "Can't fetch ~a" url-string)
                            (debug 'SUF "exn: ~a~n" e)
                            (set! result default-message))])
      (set! result (reader (get-pure-port 
                            (cond
                              [(string? url-string) 
                               (string->url url-string)]
                              [else url-string]))))
      (debug 'SUF "result: ~a" result)
      result)))

(define make-server-url 
  (λ args
    (let* ([url-str 
            (format "http://~a:~a~a"
                    (first args)
                    (second args)
                    (apply string-append
                           (map (λ (p) (format "/~a" p)) 
                                (rest (rest args)))))]
           [the-url (string->url url-str)])
      (debug 'MAKE-SERVER-URL "url: ~a~n" url-str)
      the-url)))

(define words
  '(judd moldy neck overobject nonthreatening
         pedatilobate plasmolytic antihierarchal axiomatical bighead
         cloisterlike churlish swage tweezers tableaux
         telegnostic unintercepted	universalizer radiobroadcast prejudice
         preinjurious protagonist danger dermatic dejecta
         deluxe enterprise scarier siren skewness
         sleekit soutine struggle sumptuous fried
         gallerylike gent gliomatous hetaira island
         resignal unhemmed realign transfiguration lavada gritter icao
         unreserved thomisid cranebill unevil manue savorer prosuffrage sollar kuvaszok
         evagination whistling crimean evoked salugi interpaving annuitant homonym 
         hoidenish bassenthwaite lavatory outrung subpeduncled amalgamative cofunction 
         splore drawbar anapaest unsquirming overpartiality unfevered loopy marbles 
         viosterol antilepton superhet aleyard adoptively prelabelled baccy parodistically 
         tammy satirisation wettability haole reinterrupt climacterical immediatist 
         ostiole consecrator kazakstan perceivedness effeminate ramee transcendentalist
         checkrail hersh heelless facility carcass changchowfu nettle cornstarch
         ))

(define (make-id n)
  (define ls '())
  (for ([n (range n)])
    (set! ls (cons (~a (list-ref words (random (length words))))
                   ls)))
  (apply string-append (list-intersperse ls "-")))