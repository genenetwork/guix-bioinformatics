(define-module (gn packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public javascript-twitter-post-fetcher
  (let ((commit "12d1693980ef836af4d5eee74a0aec5c65b8e6c3"))
  (package
    (name "javascript-twitter-post-fetcher")
    (version (string-append "17.0.2" "-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jasonmayes/Twitter-Post-Fetcher.git")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "1afs5dd3mbih016vganvm9bcmn3cg3cvwh34hhk5n04qvb2aphdm"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((out (assoc-ref %outputs "out"))
              (name "Twitter-Post-Fetcher")
              (targetdir (string-append out "/share/genenetwork2/javascript/" name))
              )
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source"))
                   )
               (and
                    ; (mkdir-p targetdir)
                    (copy-recursively source targetdir)
                    ))))))
    (home-page "http://jasonmayes.com/projects/twitterApi/")
    (synopsis "Twitter post fetching")
    (description "Twitter post fetching.")
    (license license:expat))))
