;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu)
             (gnu packages web))
(use-service-modules networking ssh web)
(use-package-modules screen ruby)

(operating-system
  (host-name "komputilo")
  (timezone "Europe/Amsterdam")
  (locale "en_US.utf8")

  ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
  ;; target hard disk, and "my-root" is the label of the target
  ;; root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/sdX")))
  (file-systems (cons (file-system
                        (device (file-system-label "my-root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
                (name "pjotr")
                (password "$6$EoLVFsCpLywbQmy1$aJJ3dIrgIH4UtiTVynIZ0MiC667w4C5ybygGisUnUfusPrgxZ7ncz.Cjv67EJPA6VW3EPFbOaiadQzxFn2sLb.")
                (comment "Pjotr")
                (group "users")

                ;; Adding the account to the "wheel" group
                ;; makes it a sudoer.  Adding it to "audio"
                ;; and "video" allows the user to play sound
                ;; and access the webcam.
                (supplementary-groups '("wheel"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Globally-installed packages.
  ; (packages (cons screen %base-packages))

  (packages (append (list
                     screen nginx)
                    %base-packages))


  ;; Add services to the baseline: a DHCP client and
  ;; an SSH server with nginx
  (services (append (list (service dhcp-client-service-type)
                          (service openssh-service-type
                                   (openssh-configuration
                                    ; (authorized-keys
                                    ;  `(("pjotr" ,(local-file "/home/pjotr/.ssh/authorized_keys"))))
                                    (password-authentication? #t)
                                    (port-number 2222)))
                          (service nginx-service-type
                                   (nginx-configuration
                                    (server-blocks
                                     (list (nginx-server-configuration
                                            (listen '("8080")))))))
                                   )
                    %base-services)))
