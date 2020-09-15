(define-module (gn services ratspub-container))

(use-modules (gnu)
             (gn packages ratspub)
             (guix download)
             (guix modules)
             (guix packages)
             (guix records)
             (ice-9 match))
(use-service-modules shepherd)
(use-package-modules certs compression)

(define-record-type* <ratspub-configuration>
  ratspub-configuration
  make-ratspub-configuration
  ratspub-configuration?
  (package  ratspub-configuration-package   ; package
            (default ratspub)))

(define %punkt.zip
  (origin
    (method url-fetch)
    (uri "https://github.com/nltk/nltk_data/raw/b63a469d2f83a3cc9a2efcfe36915839d4e11d42/packages/tokenizers/punkt.zip")
    (sha256
     (base32 "0i01c5qzn1p8dxyrpx4hry2n6x6b8rgcq1sck091n0jp036f6x4s"))))

(define ratspub-activation
  (match-lambda
    (($ <ratspub-configuration> package)
     #~(begin
         (let ((nltk_data "/var/cache/nltk_data/tokenizers")
               (data_dir "/export/ratspub"))
           (unless (file-exists? "/export2/PubMed")
             (mkdir-p "/export2/PubMed"))
           (unless (file-exists? nltk_data)
             (begin
               ;; The correct way would be to use python-nltk to download the data
               ;; python3 -m nltk.downloader -d /var/cache/nltk_data punkt
               (mkdir-p nltk_data)
               (chdir nltk_data)
               (invoke #$(file-append unzip "/bin/unzip") "-q" #$%punkt.zip)))
           (unless (file-exists? (string-append data_dir "/userspub.sqlite"))
             (begin
               (install-file #$(file-append package "/userspub.sqlite") data_dir)
               (chmod (string-append data_dir "/userspub.sqlite") #o554))))))))

(define ratspub-shepherd-service
  (match-lambda
    (($ <ratspub-configuration> package)
     (with-imported-modules (source-module-closure
                              '((gnu build shepherd)
                                (gnu system file-systems)))
       (list (shepherd-service
               (provision '(ratspub))
               (requirement '(networking))
               (modules '((gnu build shepherd)
                          (gnu system file-systems)))
               (start #~(make-forkexec-constructor/container
                          (list #$(file-append package "/server.py"))
                          ;; Needs to run from the directory it is located in.
                          #:directory #$package
                          #:log-file "/var/log/ratspub.log"
                          ;; We don't need to set TMPDIR because we're inside a container.
                          #:environment-variables
                          '("EDIRECT_PUBMED_MASTER=/export2/PubMed"
                            "NLTK_DATA=/var/cache/nltk_data"
                            "PERL_LWP_SSL_CA_FILE=/etc/ssl/certs/ca-certificates.crt")
                          #:mappings (list (file-system-mapping
                                             (source "/export2/PubMed")
                                             (target source)
                                             (writable? #t))
                                           (file-system-mapping
                                             (source "/export/ratspub")
                                             (target source)
                                             (writable? #t))
                                           (file-system-mapping
                                             (source "/var/cache/nltk_data")
                                             (target source))
                                           (file-system-mapping
                                             (source "/etc/ssl/certs")
                                             (target source)))))
               (stop  #~(make-kill-destructor))))))))

(define ratspub-service-type
  (service-type
    (name 'ratspub)
    (extensions
      (list
        (service-extension shepherd-root-service-type
                           ratspub-shepherd-service)
        (service-extension activation-service-type
                           ratspub-activation)
        ;; Make sure we get all the dependencies of RatsPub.
        (service-extension profile-service-type
                           (compose list ratspub-configuration-package))))
    (default-value (ratspub-configuration))
    (description
     "Run a RatsPub Webserver.")))

(operating-system
  (host-name "ratspub")
  (timezone "Etc/UTC")
  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "does-not-matter")))
  (file-systems (list (file-system
                        (device "does-not-matter")
                        (mount-point "/")
                        (type "does-not-matter"))))
  ;; TODO: A more minimal kernel for use in a docker image
  ;; (kernel linux-libre-vm)
  ;; No firmware for VMs.
  (firmware '())
  (packages (list nss-certs))

  (services (list (service ratspub-service-type
                           (ratspub-configuration
                             ;; ratspub for docker, ratspub-with-tensorflow-native for architecture specific speed optimizations.
                             ;(package ratspub))))))
                             (package ratspub-with-tensorflow-native))))))

;; guix system container -L /path/to/guix-bioinformatics/ -L /path/to/guix-past/modules/ /path/to/guix-bioinformatics/gn/services/ratspub-container.scm --network --share=/export2/PubMed=/export2/PubMed --share=/export/ratspub=/export/ratspub
;; For docker it isn't necessary to list the shared folders at build time.
;; guix system docker-image -L /path/to/guix-bioinformatics/ -L /path/to/guix-past/modules/ /path/to/guix-bioinformatics/gn/services/ratspub-container.scm --network
;; Docker instructions:
;; docker load --input ratspub-docker-image.tar.gz
;; docker run -d --privileged --net=host --name ratspub --volume /path/to/PubMed:/export2/PubMed guix
