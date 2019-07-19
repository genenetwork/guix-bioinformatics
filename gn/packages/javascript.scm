(define-module (gn packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gn packages web)
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
    (version "3.8.1") ; July 9, 2019
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cytoscape/cytoscape.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1dnwvmghwq21g9ag5cj49l0wnyfc54kbsrj0byii5wbwljjg9826"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://js.cytoscape.org/")
    (synopsis "Graph theory (network) library for visualisation and analysis")
    (description "Cytoscape.js is a fully featured graph theory library.  Do you
need to model and/or visualise relational data, like biological data or social
networks? If so, Cytoscape.js is just what you need.

Cytoscape.js contains a graph theory model and an optional renderer to display
interactive graphs.  This library was designed to make it as easy as possible
for programmers and scientists to use graph theory in their apps, whether it's
for server-side analysis in a Node.js app or for a rich user interface.")
    (license license:expat)))

(define-public javascript-cytoscape-2
  (package
    (inherit javascript-cytoscape)
    (name "javascript-cytoscape")
    (version "2.7.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cytoscape/cytoscape.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00y0h6kdkw2x6lyf9c16fxmg8iagfl77dz8nqb337v3ljifkb4z8"))))))

(define-public javascript-cytoscape-panzoom
  (package
    (name "javascript-cytoscape-panzoom")
    (version "2.5.3") ; August 21, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-panzoom")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lzcsip6q44h14g5l4jciv5sfc7ilvg1yrd14lcji8mnq6akx16n"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-panzoom"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/cytoscape-panzoom.js") targetdir)
           (install-file (string-append source "/cytoscape.js-panzoom.css") targetdir)))))
    (native-inputs `(("source" ,source)))
    ;; TODO: Add font-awsome?
    (propagated-inputs
     `(("javascript-cytoscape" ,javascript-cytoscape)
       ("jquery" ,web-jquery)))
    (home-page "https://github.com/cytoscape/cytoscape.js-panzoom/")
    (synopsis "Panzoom extension for Cytoscape.js")
    (description "This extension creates a widget that lets the user pan and
zoom about a Cytoscape.js graph.  This complements the built-in gesture support
for panning and zooming in Cytoscape.js by giving less savvy users a more
traditional UI -- similar to controls on map webapps.")
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
