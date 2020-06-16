(define-module (gn services rn6-assembly-container))

(use-modules (gnu)
             (gn packages bioinformatics)
             (gn services rshiny))

(operating-system
  (host-name "rn6-assembly")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs.
  (firmware '())
  ;; We only need a few packages inside the container.
  (packages '())

  (services (list (service rshiny-service-type
                           (rshiny-configuration
                             (package rn6-assembly-error-app)
                             (binary "rn6-assembly-error-app"))))))
