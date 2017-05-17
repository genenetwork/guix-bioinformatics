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
  (let ((commit "377c050def3a1076c15cb24fd20b71f801d0e78f"))
  (package
    (name "javascript-twitter-post-fetcher")
    (version (string-append "16.0.3" "-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/Twitter-Post-Fetcher.git")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "05w2wiqw84lb778nkivn421m1kk6h66xk4cjjqng1bri3d2kfzjv"))))
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
