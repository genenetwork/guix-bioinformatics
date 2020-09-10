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

;; The correct way would be to use python-nltk to download the data
;; python3 -m nltk.downloader -d /var/cache/ratspub punkt
(define ratspub-activation
  (lambda _
    #~(begin
        (let ((nltk_data "/var/cache/ratspub/tokenizers"))
          (mkdir-p nltk_data)
          (chdir nltk_data)
          (invoke #$(file-append unzip "/bin/unzip") "-q" #$%punkt.zip)))))

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
                          #:environment-variables
                          '("EDIRECT_PUBMED_MASTER=/export2/PubMed"
                            "NLTK_DATA=/var/cache/ratspub"
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
                                             (source "/var/cache/ratspub")
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
        ;; Setup the NLTK_DATA data.
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
  (file-systems %base-file-systems)
  ;; No firmware for VMs.
  (firmware '())
  (packages (list nss-certs))

  (services (list (service ratspub-service-type
                           (ratspub-configuration
                             ;(package ratspub)
                             (package ratspub-with-tensorflow-native))))))

;; guix system container -L /path/to/guix-bioinformatics/ -L /path/to/guix-past/modules/ /path/to/guix-bioinformatics/gn/services/ratspub-container.scm --network --share=/export2/PubMed=/export2/PubMed --share=/export/ratspub=/export/ratspub
