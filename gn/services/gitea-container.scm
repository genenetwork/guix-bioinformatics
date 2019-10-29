(define-module (gn services gitea-container))

(use-modules (gnu)
             (gn packages gitea)
             (guix records)
             (ice-9 match))
(use-service-modules base networking shepherd)

(define %GITEA_WORK_DIR "/var/lib/gitea")
(define-record-type* <gitea-configuration>
  gitea-configuration
  make-gitea-configuration
  gitea-configuration?
  (package          gitea-configuration-package     ; package
                    (default gitea))
  (work-dir         gitea-configuration-work-dir    ; string
                    (default %GITEA_WORK_DIR))
  (port             gitea-configuration-port        ; number
                    (default 3001)))

(define gitea-activation
  (match-lambda
    (($ <gitea-configuration> package work-dir port)
     #~(begin
         (use-modules (guix build utils))
         (let ((%user (getpwnam "gitea")))
           ;; Prepare the environment for gitea:
           ;; https://docs.gitea.io/en-us/install-from-binary/
           (unless (directory-exists? #$work-dir)
             (mkdir-p #$work-dir)
             ;; These two are supposed to be recursive.
             (chown #$work-dir (passwd:uid %user) (passwd:gid %user))
             (chmod #$work-dir #o750)))))))

(define gitea-shepherd-service
  (match-lambda
    (($ <gitea-configuration> package work-dir port)
     (list (shepherd-service
             (documentation "Run the Gitea server.")
             (requirement '(networking))
             (provision '(gitea))
             (start #~(make-forkexec-constructor
                        (list
                          #$(file-append package "/bin/gitea")
                          "--port" #$(number->string port))
                        #:environment-variables
                        (list (string-append "GITEA_WORK_DIR=" #$work-dir)
                              (string-append "HOME=" #$work-dir))
                        #:user "gitea"
                        #:group "gitea"))
             (stop #~(make-kill-destructor)))))))

(define gitea-service-type
  (service-type
    (name 'gitea)
    (extensions
      (list (service-extension shepherd-root-service-type
                               gitea-shepherd-service)
            (service-extension activation-service-type
                               gitea-activation)))
    (description
     "Run a Gitea server.")
    (default-value (gitea-configuration))))


(operating-system
  (host-name "gitea")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems %base-file-systems)
  ;; No firmware for VMs or containers.
  (firmware '())

  ;; The user and group names aren't important, but the user uid and group id
  ;; NEED to match the directory outside of the container.
  (users (cons (user-account
                 (name "gitea")
                 (group "gitea")
                 (system? #t)
                 (uid 1009))
           %base-user-accounts))

  (groups (cons (user-group
                  (name "gitea")
                  (system? #t)
                  (id 1009))
                %base-groups))

  (services (list (service dhcp-client-service-type)
                  (service gitea-service-type))))
