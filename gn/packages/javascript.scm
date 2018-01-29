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
  (let ((commit "27440ffebd4c1ba7abc9aec92a581155715f2e4e"))
  (package
    (name "javascript-twitter-post-fetcher")
    (version (string-append "17.0.3" "-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jasonmayes/Twitter-Post-Fetcher.git")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "159z8izf510086d8sa79k4mml6sw1gycb1r1r9ri1kyw2k9knmqa"))))
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
