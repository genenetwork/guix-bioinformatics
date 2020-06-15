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
         ;; This one works:
         (start
           #~(lambda _
               (setenv "R_LIBS_USER" "/run/current-system/profile/site-library/")
               (invoke #$(string-append "/run/current-system/profile/bin/" binary))))
         ;; Let's try in a container:
         ;(start #~(make-forkexec-constructor/container
         ;           (list #$(file-append package "/bin/" binary))
         ;           #:environment-variables
         ;           (list "R_LIBS_USER=/run/current-system/profile/site-library/")))
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
;     (string-append "Run " (rshiny-configuration-package config) ", an R-Shiny
;webapp, as a Guix Service.")
     "Run an R-Shiny webapp as a Guix Service.")))
