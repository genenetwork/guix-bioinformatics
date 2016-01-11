(define-module (gn packages pfff)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake))

(define-public pfff-gn
  (package
    (name "pfff-gn")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pfff/pfff/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "00m553aa277iarxj6dalmklyb64r7ias49bfwzbacsfg8h3kar8m"))))
    (build-system cmake-build-system)
    (home-page "http://biit.cs.ut.ee/pfff/")
    (synopsis "Probabilistic fast file fingerprinting")
    (description
     "Calculate a probablistic fast finger print (pfff) which
functions as a compact digital fingerprint of a file by sampling
randomly from the file instead of reading it in full.  Consequently,
the computation has a flat performance characteristic, correlated with
data variation rather than file size.")
    (license license:bsd-3)))
