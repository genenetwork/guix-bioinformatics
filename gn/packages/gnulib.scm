(define-module (gn packages gnulib)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages texinfo))

(define-public gnulib
  (let ((commit "de255f87357354e0f2422d9321fe9701b776ead5")
        (revision "2877")) ; as reported from gnulib
    (package
      (name "gnulib")
      (version (git-version "0.1" revision commit)) ; Sept. 30, 2019
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://git.savannah.gnu.org/git/gnulib.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "00zjjldhj8ckr47fkqmb4d0lkpn6awwi8k8pngkfn2g30ccz7hji"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; the test suite doesn't help us
         #:make-flags '("info")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "doc/gnulib.info"
                             (string-append (assoc-ref outputs "out")
                                            "/share/info")))))))
      (native-inputs
       `(("texinfo" ,texinfo)))
      (home-page "https://www.gnu.org/software/gnulib/")
      (synopsis "Common GNU code")
      (description
       "Gnulib is a central location for common GNU code, intended to be shared
among GNU packages.")
      (license (list license:gpl2+
                     license:lgpl2.0+)))))
