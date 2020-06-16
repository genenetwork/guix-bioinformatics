;; Not ready for deployment yet
;; Warning: Error in <Anonymous>: error in evaluating the argument 'object' in selecting a method for function 'summary':

(define-module (gn services singlecell-container))

(use-modules (gnu)
             (gn packages bioinformatics)
             (gn services rshiny))

(operating-system
  (host-name "singlecell")
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
                             (package singlecellrshiny)
                             (binary "singlecellrshiny"))))))
