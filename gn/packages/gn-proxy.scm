(define-module (gn packages gn-proxy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages scheme))

(define-public gn-proxy
  (let ((commit "866bc432e1e5686f18a8e178e6bbde69fd5b83a2"))
  (package
    (name "gn-proxy")
    (version (string-append "0.2.1-gn2-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/gn-proxy.git")
                   (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qfz6wz1f977pgpq74vn60va9szdanc8dm37fi7g34vzcqyskmfa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("racket-minimal" ,racket-minimal)))
    (home-page "https://github.com/genenetwork/gn-proxy")
    (synopsis "GN proxy and access control")
    (description
     "GeneNetwork proxy manages authorization/access control")
    (license license:agpl3))))
