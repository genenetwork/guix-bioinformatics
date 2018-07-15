(define-module (gn packages edash)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (gnu packages web)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages time)
  #:use-module (gnu packages check)
  #:use-module (gn packages web))

(define-public python-s3transfer-0.1.13
  (package
  (name "python-s3transfer")
  (version "0.1.13")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "s3transfer" version))
      (sha256
        (base32
          "1harvyn1s8v54n1w5h7c0lg4bgjh68aylhg28s8n174q53h1ip4h"))))
  (build-system python-build-system)
  (arguments
   `(#:tests? #f))
  (home-page "https://github.com/boto/s3transfer")
  (synopsis "Amazon S3 Transfer Manager")
  (description "S3transfer is a Python library for managing Amazon S3
transfers.")
  (license license:asl2.0)))

(define-public python-botocore-1.10.48
  (package
    (name "python-botocore")
    (version "1.10.48")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "botocore" version))
       (sha256
        (base32
	 "1432drc7482nwrppwkk1i6ars3wz9w2g9rsxkz5nlxmyf9qm260j"))))
    (build-system python-build-system)
    (arguments
    ;; FIXME: Many tests are failing.
    '(#:tests? #f))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-docutils" ,python-docutils)
       ("python-jmespath" ,python-jmespath)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("behave" ,behave)
       ("python-tox" ,python-tox)
       ("python-wheel" ,python-wheel)))
    (home-page "https://github.com/boto/botocore")
    (synopsis "Low-level interface to AWS")
    (description "Botocore is a Python library that provides a low-level
interface to the Amazon Web Services (AWS) API.")
    (license license:asl2.0)))

(define-public python-boto3
  (package
  (name "python-boto3")
  (version "1.7.48")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "boto3" version))
      (sha256
        (base32
	 "1pnm5pj70kaa8pbq39i2y70h17cazk8nkjqh12b8xqks7ywjr9f5"))))
  (build-system python-build-system)
  (arguments
   `(#:tests? #f))
  (propagated-inputs
    `(("python-botocore" ,python-botocore-1.10.48)
      ("python-jmespath" ,python-jmespath)
      ("python-s3transfer" ,python-s3transfer-0.1.13)))
  (home-page "https://github.com/boto/boto3")
  (synopsis "The AWS SDK for Python")
  (description "The AWS SDK for Python")
  (license license:asl2.0)))

(define-public edash
  (let ((md5 "93e745e9c"))
    (package
    (name "edash")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "http://biogems.info/genenetwork2-2.0-a8fcff4.svg") ; any old file
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "0rir1mcn3a8i1mbw3ppgnjl7wg71mapljik7n3v5i8j5ic95mqr5"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    (inputs `(("sassc" ,sassc)))
    (propagated-inputs
     `(("python" ,python)
       ("python-wrapper" ,python-wrapper)
       ("python-elasticsearch" ,python-elasticsearch)
       ("python-flask" ,python-flask)
       ("python-jinja2" ,python-jinja2)
       ("python-mako" ,python-mako)
       ("python-markdown" ,python-markdown)
       ("python-misaka" ,python-misaka)
       ("python-pygit2" ,python-pygit2)
       ("web-bootstrap" ,web-bootstrap)
       ("sassc" ,sassc)
       ("python-boto3" ,python-boto3)
       ("python-pytest" ,python-pytest)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share")))
             (write target)
             (mkdir-p target)
             ; (copy-recursively (assoc-ref %build-inputs "source") target)
             #t))))

    (home-page "https://gitlab.com/unless-emtec/live-service")
    (synopsis "EMTech's edash service")
    (description "EMTech's edash service package.")
    (license license:agpl3+))))
