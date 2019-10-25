(use-modules (gnu))
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
                                   (root "/srv/http/bnw")
                                   (locations
                                     (list (nginx-php-location)))
                                   (listen '("8880"))
                                   (ssl-certificate #f)
                                   (ssl-certificate-key #f)))))))))
