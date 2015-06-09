#lang racket

(require racket/gui
         racket/runtime-path
         yaml
         )

(require "debug.rkt"
         "model-plumb.rkt"
         "byoe-window.rkt"
         "byoe-feedback.rkt"
         "util-gui.rkt"
         "config.rkt"
         "util.rkt"
         )


;; To reference client.yaml
(define-runtime-path here (build-path "."))

;; DO STUFF

(define (build-ide hardware)
  ;; Initialize the main view
  (define win-main (new win-main% [model hardware]))
  (send hardware add-view win-main)
  ;; Init the feedback view
  (define win-feedback (new win-feedback% [model hardware]))
  ;; Update everyone
  (send hardware update)
  ;; Say Hello
  (send hardware say-hello)
  ;; Show the main view
  (send win-main show true)
  
  (check-version (send win-main get-frame))
  )

(define (create)
  
  ;; Interaction
  ;; Need this to build the IDE
  (define hardware (new plumb%))
  ;; (send hardware load-config)
  ;; Load our configuration (client.yaml by default)
  (config-file (build-path here "client.yaml"))
  (load-config)
  
  (conf-add "boards" 
            (string->yaml
             (read-url (format "http://~a:~a/ide/boards.yaml" 
                               (conf-get 'server)
                               (conf-get 'port)))))
  
  (send hardware enumerate-arduinos)
  
  
  (send hardware compilation-server-config)
  
  (enable-debug! 'ALL)
  (set-textual-debug)
  (build-ide hardware)
  )

(create)
