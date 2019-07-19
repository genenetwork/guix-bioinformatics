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

;; see color-make package for great example - also install-file and invoke

(define-public javascript-twitter-post-fetcher
  (let ((commit "8f9e667e917b3c581b100bf2ccc7157aadc5ff43") ; April 30, 2019
        (revision "1"))
    (package
      (name "javascript-twitter-post-fetcher")
      (version (git-version "18.0.2" revision commit)) ; April 3, 2018
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/jasonmayes/Twitter-Post-Fetcher.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0n935qmmd9gijkklrps8qimplbskcrijds3076zfd28afxrr96wp"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/" ,name))
                (source (assoc-ref %build-inputs "source")))
           (begin
             (use-modules (guix build utils))
             (copy-recursively source targetdir)
             (install-file (string-append source "/License.txt")
                           (string-append out "/share/doc/" ,name "-" ,version))))))
      (native-inputs `(("source" ,source)))
      (home-page "http://jasonmayes.com/projects/twitterApi/")
      (synopsis "Twitter post fetching")
      (description "Allows you to get your tweets displaying on your website
using JavaScript, without using new Twitter 1.1 API.")
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

(define-public javascript-cytoscape-panzoom
  (package
   ;; (inherit javascript-cytoscape)
   (name "javascript-cytoscape-panzoom")
   (version "2.5.2") ; ancient version
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/cytoscape/cytoscape.js-panzoom/archive/" version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "154xzi693gbv89y221gkpi03k84lccmr55v5z43mn1i1s1fdhm2b"))))
   (inputs `(("javascript-cytoscape" ,javascript-cytoscape)))
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
               (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-panzoom"))
               (source (assoc-ref %build-inputs "source")))
          (setenv "PATH" (string-append
                          (assoc-ref %build-inputs "tar") "/bin" ":"
                          (assoc-ref %build-inputs "gzip") "/bin"))
          (invoke "tar" "xvf" (assoc-ref %build-inputs "source") "--strip-components=1")
          (mkdir-p targetdir)
          (install-file "cytoscape-panzoom.js" targetdir)
          (install-file "cytoscape.js-panzoom.css" targetdir)
          ))))
    (home-page "https://github.com/cytoscape/cytoscape.js")
    (synopsis "Cytoscape.js")
    (description "Cytoscape.")
    (license license:expat)))

;; https://github.com/cytoscape/cytoscape.js-qtip/archive/2.7.1.tar.gz
(define-public javascript-cytoscape-qtip
  (package
   ;; (inherit javascript-cytoscape)
   (name "javascript-cytoscape-qtip")
   (version "2.7.1") ; ancient version
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/cytoscape/cytoscape.js-qtip/archive/" version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0f2qhxi9qx5r3rcmxqgdqnwd9apnjlb4s4hckwhaqgsp6rf8lzlb"))))
   (inputs `(("javascript-cytoscape" ,javascript-cytoscape)))
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
               (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-qtip"))
               (source (assoc-ref %build-inputs "source")))
          (setenv "PATH" (string-append
                          (assoc-ref %build-inputs "tar") "/bin" ":"
                          (assoc-ref %build-inputs "gzip") "/bin"))
          (invoke "tar" "xvf" (assoc-ref %build-inputs "source") "--strip-components=1")
          (mkdir-p targetdir)
          (install-file "cytoscape-qtip.js" targetdir)
          ))))
    (home-page "https://github.com/cytoscape/cytoscape.js")
    (synopsis "Cytoscape.js")
    (description "Cytoscape.")
    (license license:expat)))

;; https://github.com/DataTables/DataTables/archive/1.10.12.tar.gz
(define-public javascript-datatables
  (package
   (name "javascript-datatables")
   (version "1.10.12") ; ancient version
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/DataTables/DataTables/archive/" version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0blzsd2zqmvnqi6pl1xq0dr457kjrdaaa86d2cmdakls0j2mj92s"))))
   (inputs `(("javascript-cytoscape" ,javascript-cytoscape)))
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
               (targetdir (string-append out "/share/genenetwork2/javascript/DataTables"))
               (source (assoc-ref %build-inputs "source")))
          (setenv "PATH" (string-append
                          (assoc-ref %build-inputs "tar") "/bin" ":"
                          (assoc-ref %build-inputs "gzip") "/bin"))
          (invoke "tar" "xvf" (assoc-ref %build-inputs "source") "--strip-components=1")
          (mkdir-p targetdir)
           (copy-recursively "media" targetdir)
          ))))
    (home-page "https://github.com/DataTables/")
    (synopsis "Datatables")
    (description "Datatables.")
    (license license:expat)))
