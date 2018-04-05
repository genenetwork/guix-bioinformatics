(define-module (gn packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
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

(define-public javascript-cytoscape
  (package
    (name "javascript-cytoscape")
    (version "2.7.8") ; ancient version
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cytoscape/cytoscape.js/archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08ks2nd7ccmdmahn151i180pvhn4vdzgpw99g4g4f2baz9pkz4w3"))))
    (build-system trivial-build-system)
    (native-inputs `(("gzip" ,gzip)
                     ("tar" ,tar)
                     ("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (tarcmd (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape"))
                (source (assoc-ref %build-inputs "source")))
           (setenv "PATH" (string-append
                           (assoc-ref %build-inputs "tar") "/bin" ":"
                           (assoc-ref %build-inputs "gzip") "/bin"))
           (invoke "tar" "xvf" (assoc-ref %build-inputs "source") "--strip-components=1")
           (mkdir-p targetdir)
           (copy-recursively "dist" targetdir)
           ))))

    (home-page "https://github.com/cytoscape/cytoscape.js")
    (synopsis "Cytoscape.js")
    (description "Cytoscape.")
    (license license:expat)))
