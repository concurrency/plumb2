#lang racket
(require yaml
         racket/runtime-path
         "debug.rkt"
         "util.rkt")

(provide (all-defined-out))

;; (define-runtime-path here (build-path "."))

(define config-file (make-parameter false))
(define config (make-parameter false))

(define (conf-add key value)
  (debug 'ADD (format "~a <- ~a" key value))
  (hash-set! (config) (format "~a" key) value))

(define (conf-get key)
  (hash-ref (config) (format "~a" key)))

(define (load-config)
  (config (file->yaml (config-file)))
  (show-config))

(define (show-config)
  ;; Show the config
  (for ([(k v) (config)])
    (debug 'CONFIG "~a <- ~a" k v)))

;; Used in avrdude in client-cmds.rkt
(define (show-conf conf)
  (for ([(k v) conf])
    (debug 'CONF "~a <- ~a"
           k
           (cond
             [(and (string? v) (> (string-length v) 30))
              (~a v #:max-width 30)]
             [(hash? v) 
              (debug 'CONF "~a~n---~n" k)
              (show-conf v)]
             [else
              (~a v)]))))


;; Handling for IDE/BYOE
;; This requires a bit of extra work, because we need to know where
;; some files are at runtime. This is code grabbed from the original
;; plumb, and hacked for the new config format.

(define (load-additional-client-config)
  (define platform
    (case (->sym (system-type))
      [(windows) "windows"]
      [(mac macosx osx) "macosx"]
      [(unix linux) "linux"]))
  
  (conf-add 'HOST-TYPE (system-type))
  ;; This should give us the root of the Plumb.app
  (conf-add 'APP-ROOT (find-system-path 'run-file))
  ;; When we are inside a .app bundle, set the contents path
  ;; one way. When we're running from the command line (which
  ;; is primarily a development activity), change things around.
  (debug 'CONFIG "APP-ROOT is [~a]" (conf-get 'APP-ROOT))
  
  ;; Special handling for running as a compiled app or on the
  ;; local host while testing.
  (cond
    [(and (regexp-match ".app" (~a (conf-get 'APP-ROOT)))
          ;; but we're not running under DrRacket
          (not (regexp-match "DrRacket.app" (~a (conf-get 'APP-ROOT)))))
     (conf-add 'CONTENTS (build-path
                          (extract-filedir (conf-get 'APP-ROOT))
                          'up
                          ))]
    ;; This really shouldn't apply... well. It might.
    ;; MCJ 20150609
    [else
     (conf-add 'APP-ROOT (current-directory))
     (conf-add 'CONTENTS (conf-get 'APP-ROOT))])
  
  (conf-add 'BINPATH (build-path 
                      (conf-get 'CONTENTS)
                      "client-config" 
                      platform
                      "bin"))
  (conf-add 'CONFPATH (build-path 
                       (conf-get 'CONTENTS)
                       "client-config" 
                       platform
                       "conf"))
  
  (conf-add 'TEMPPATH (find-system-path 'temp-dir))
  
  (conf-add "firmware-name" (build-path
                             (conf-get 'TEMPPATH)
                             "plumbware.hex"))
  
  (conf-add 'AVRDUDE.CONF (build-path 
                           (conf-get 'CONFPATH)
                           "avrdude.conf"))
  (case (->sym platform)
    [(windows)
     (conf-add 'AVRDUDE (build-path 
                         (conf-get 'BINPATH)
                         "avrdude.exe"))]
    [(macosx)
     (conf-add 'AVRDUDE (build-path 
                         (conf-get 'BINPATH)
                         "avrdude"))]
    [else
     ;; Use the system avrdude.
     (conf-add 'AVRDUDE "avrdude")])
  
  )