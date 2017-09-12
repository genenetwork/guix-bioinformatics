(define-module (gn packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public web-bootstrap-css
  (let ((commit "2e48d7ee29d4063e3bd2024ff83ddc50a550c4dd"))
  (package
    (name "web-bootstrap-native-css")
    (version (string-append "4.0.0" "-beta-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/thednp/bootstrap.native.git")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "1hkyibyfby0mnkavr3xbmr20kb88wy6jw28b206pd236xnp2qkx0"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((out (assoc-ref %outputs "out"))
              (name "bootstrap-native")
              (targetdir (string-append out "/share/web/" name))
              )
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source"))
                   )
               (and
                    ; (mkdir-p targetdir)
                    (copy-recursively source targetdir)
                    ))))))
    (home-page "https://github.com/thednp/bootstrap.native")
    (synopsis "Bootstrap minimal")
    (description "Bootstrap native does not use jquery.")
    (license license:expat))))
