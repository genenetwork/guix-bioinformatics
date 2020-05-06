(define-module (gn services bnw-container))

(use-modules (gnu)
             (gn packages bnw)
             (guix records)
             (ice-9 match))
(use-service-modules networking web)

(define-record-type* <bnw-configuration>
  bnw-configuration
  make-bnw-configuration
  bnw-configuration?
  (package          bnw-configuration-package       ; package
                    (default bnw))
  (deploy-directory bnw-deploy-directory            ; string
                    (default "/srv/http"))
  (port             bnw-configuration-port          ; list of strings
                    (default '("8880"))))

(define bnw-activation
  (match-lambda
    (($ <bnw-configuration> package deploy-directory port)
     #~(begin
         (mkdir-p #$deploy-directory)
         (copy-recursively #$package #$deploy-directory)
         (invoke #$(file-append coreutils "/bin/chmod") "a+w"
                 (string-append #$deploy-directory "/sourcecodes/data"))))))

(define bnw-nginx-config
  (match-lambda
    (($ <bnw-configuration> package deploy-directory port)
     (list
       (nginx-server-configuration
         (server-name '("Bayesian Network"))
         (listen port)
         ;(root package)
         (root deploy-directory)
         (locations
           (list
             (nginx-php-location)
             ;(nginx-location-configuration
             ;  (uri "/sourcecodes/data/")
             ;  (body (list "alias /tmp/bnw/;")))
             )))))))

(define bnw-service-type
  (service-type
    (name 'bnw)
    (extensions
      (list
        (service-extension activation-service-type
                           bnw-activation)
        (service-extension nginx-service-type
                           bnw-nginx-config)
        ;; Make sure BNW doesn't get garbage collected.
        (service-extension profile-service-type
                           (compose list bnw-configuration-package))
        ;; Make sure php-fpm is instantiated.
        (service-extension php-fpm-service-type
                           (const #t))))
    (default-value (bnw-configuration))
    (description
     "Run a Bayesian Network Webserver.")))

(operating-system
  (host-name "bnw")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs.
  (firmware '())
  ;; We don't need any packages inside the container.
  (packages '())

  (services (list (service dhcp-client-service-type)
                  (service bnw-service-type
                           ;; The following is for testing:
                           ;(bnw-configuration
                           ;  (port '("8888")))
                           ))))
