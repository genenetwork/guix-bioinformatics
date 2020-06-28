(define-module (gn services bh20-seq-resource-container))

(use-modules (gnu)
             (gn packages bioinformatics)
             (guix modules)
             ((guix packages) #:select (package-source))
             (guix records)
             (ice-9 match))
(use-service-modules shepherd)
(use-package-modules compression python python-web)

(define-record-type* <covid19-pubseq-configuration>
  covid19-pubseq-configuration
  make-covid19-pubseq-configuration
  covid19-pubseq-configuration?
  (package          covid19-pubseq-configuration-package    ; package
                    (default bh20-seq-resource))
  (deploy-directory covid19-pubseq-deploy-directory         ; string
                    (default "/srv/http"))
  (port             covid19-pubseq-configuration-port       ; string
                    (default "5000")))

(define covid19-pubseq-activation-service
  (match-lambda
    (($ <covid19-pubseq-configuration> package deploy-directory port)
     #~(begin
         (let ((pkg-src #$(package-source package)))
           (when (file-exists? #$deploy-directory)
             (delete-file-recursively (mkdir-p #$deploy-directory)))
           (mkdir-p #$deploy-directory)
           (if (file-is-directory? pkg-src)
             (copy-recursively pkg-src #$deploy-directory)
             (begin
               (copy-file pkg-src #$(string-append deploy-directory
                                                   "/src.tar.xz"))
               (with-directory-excursion #$deploy-directory
                 (invoke #$(file-append xz "/bin/xz") "-d" "src.tar.xz")
                 (invoke #$(file-append tar "/bin/tar") "xvf" "src.tar"
                         "--strip-components=1")))))))))

(define covid19-pubseq-shepherd-service
  (match-lambda
    (($ <covid19-pubseq-configuration> package deploy-directory port)
     (with-imported-modules (source-module-closure
                              '((gnu build shepherd)
                                (gnu system file-systems)))
       (list (shepherd-service
               (provision '(covid19-pubseq))
               (requirement '(networking))
               (modules '((gnu build shepherd)
                          (gnu system file-systems)))
               ;(start #~(make-forkexec-constructor/container
               (start #~(make-forkexec-constructor
                          (list
                            #$(file-append gunicorn "/bin/gunicorn")
                            (string-append "-blocalhost:" #$port)
                            "bh20simplewebuploader.main:app")
                          #:directory #$deploy-directory
                          #:log-file "/var/log/covid19-pubseq.log"
                          #:environment-variables
                          '("TMPDIR=/export/tmp"
                            ;; TODO: Don't hardcode python version!
                            "PYTHONPATH=/run/current-system/profile/lib/python3.8/site-packages")
                          ;#:mappings
                          ;(list (file-system-mapping
                          ;        (source "/export/tmp")
                          ;        (target source)
                          ;        (writable? #t))
                          ;      (file-system-mapping
                          ;        ;; TODO: Don't hardcode python version!
                          ;        (source "/run/current-system/profile/lib/python3.8/site-packages")
                          ;        (target source)))
                          ))
               (stop  #~(make-kill-destructor))))))))

(define covid19-pubseq-service-type
  (service-type
    (name 'covid19-pubseq)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           covid19-pubseq-shepherd-service)
        (service-extension activation-service-type
                           covid19-pubseq-activation-service)
        (service-extension profile-service-type
                           (compose list
                                    covid19-pubseq-configuration-package))))
    (default-value (covid19-pubseq-configuration))
    (description
     "Run a COVID-19 PubSeq: Public SARS-CoV-2 Sequence Resource Webserver.")))

(operating-system
  (host-name "covid19-pubseq")
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

  (services (list (service covid19-pubseq-service-type))))
