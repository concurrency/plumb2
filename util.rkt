#lang racket

;; for make-unique-name
(require ;; file/sha1
  json
  net/url
  net/base64
  "debug.rkt"
  )

(provide (all-defined-out))

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
  (->string base))


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
  (read-all (get-pure-port 
             (string->url str)
             (list "User-Agent: PLT Racket/5.3.3 (Plumb)"))))

(define (strip str)
  (for ([pat '("^[ ]+" "[ ]+$" "\n" "\r")])
    (set! str (regexp-replace pat str "")))
  str)


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