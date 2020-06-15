(define-module (gn services bxd-power-container))

(use-modules (gnu)
             (gn packages bioinformatics)
             (gn services rshiny))

(operating-system
  (host-name "bxd-power")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs.
  (firmware '())
  ;; We only need a few packages inside the container.
  (packages (list coreutils))

  (services (list (service rshiny-service-type
                           (rshiny-configuration
                             (package bxd-power-calculator-app)
                             (binary "bxd-power-calculator-app"))))))
