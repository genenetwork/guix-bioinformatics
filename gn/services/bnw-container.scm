(define-module (gn services bnw-container))

(use-modules (gnu)
             (gn packages bnw))
(use-service-modules base networking web)

(operating-system
  (host-name "bnw")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs
  (firmware '())

  (services (list (service dhcp-client-service-type)
                  (service php-fpm-service-type)
                  (service nginx-service-type
                           (nginx-configuration
                             (server-blocks
                               (list
                                 (nginx-server-configuration
                                   (server-name '("Bayesian Network"))
                                   (listen '("8888"))
                                   (root bnw)
                                   (locations
                                     (list
                                       (nginx-php-location)
                                       (nginx-location-configuration
                                         (uri "/sourcecodes/data/")
                                         (body (list "alias /tmp/bnw/;")))
                                       ))
                                   ))))))))
