(define-module (gn services rshiny)
  #:export (<rshiny-configuration>
            rshiny-configuration
            rshiny-configuration?
            rshiny-configuration-package
            rshiny-configuration-binary
            rshiny-shepherd-service
            rshiny-service-type))

(use-modules (gnu)
             (guix records)
             (ice-9 match))
(use-service-modules shepherd)
(use-package-modules cran)

(define-record-type* <rshiny-configuration>
  rshiny-configuration
  make-rshiny-configuration
  rshiny-configuration?
  (package          rshiny-configuration-package    ; package
                    (default r-shiny))
  (binary           rshiny-configuration-binary     ; string
                    (default "rshiny")))

(define rshiny-shepherd-service
  (match-lambda
    (($ <rshiny-configuration> package binary)
     (list
       (shepherd-service
         (documentation (string-append "R-Shiny service for " binary))
         (provision (list (symbol-append 'rshiny- (string->symbol
                                                    (string-take binary 9)))))
         (requirement '(networking))
         (start
           #~(exec-command
               (list
                 #$(string-append "/run/current-system/profile/bin/" binary))
               ;#:log-file #$(string-append "/var/log/" binary ".log") ; kills shepherd
               #:environment-variables
               (list "R_LIBS_USER=/run/current-system/profile/site-library/")))
         ;; Now lets try it with fork+exec
         ;; Outcome: shepherd says failed, runs in the background.
         ;(start
         ;  #~(fork+exec-command
         ;      (list
         ;        #$(string-append "/run/current-system/profile/bin/" binary))
         ;      #:environment-variables
         ;      (list "R_LIBS_USER=/run/current-system/profile/site-library/")))
         ;; More fork+exec trials, this time with a log file:
         ;; Outcome: shepherd says failed, runs in the background, log populated.
         ;(start
         ;  #~(fork+exec-command
         ;      (list
         ;        "/run/current-system/profile/bin/nohup"
         ;        #$(string-append "/run/current-system/profile/bin/" binary) "&")
         ;      #:log-file #$(string-append "/var/log/" binary ".log")
         ;      #:environment-variables
         ;      (list "R_LIBS_USER=/run/current-system/profile/site-library/")))
         ;; Now lets try it with make-forkexec:
         ;; Outcome: Kills shepherd, does not run in the background.
         ;(start
         ;  #~(make-forkexec-command
         ;      (list
         ;        "/run/current-system/profile/bin/nohup"
         ;        #$(string-append "/run/current-system/profile/bin/" binary) "&")
         ;        "/run/current-system/profile/bin/bash" "-c"
         ;        #$(string-append "/run/current-system/profile/bin/" binary))
         ;      #:environment-variables
         ;      (list "R_LIBS_USER=/run/current-system/profile/site-library/")))
         (stop #~(make-kill-destructor)))))))

(define rshiny-service-type
  (service-type
    (name 'rshiny)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           rshiny-shepherd-service)
        (service-extension profile-service-type
                           ;; We want the package installed so that it
                           ;; pulls in the propagated inputs as well.
                           (lambda (config)
                             (list
                               (rshiny-configuration-package config))))))
    (description
     "Run an R-Shiny webapp as a Guix Service.")))
