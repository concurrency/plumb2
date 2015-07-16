#lang racket
(require yaml
         "util.rkt"
         "config.rkt"
         "debug.rkt"
         "model-plumb.rkt"
         (prefix-in ccmds: "client-cmds.rkt")
         (prefix-in cmds: "cmds.rkt"))


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
                ((format "~n\tChoose the board family for your Arduino.")
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
