(define-module (gn packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gn packages web)
  #:use-module (guix packages)
  #:use-module (guix git-download)
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

(define-public javascript-dagre
  (package
    (name "javascript-dagre")
    (version "0.8.4") ; Dec. 9, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dagrejs/dagre.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1svlvd66bnskicqb7kizx57s97z9lkxssh1g5sgymw7ddfdbhy5l"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/dagre"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://github.com/dagrejs/dagre")
    (synopsis "Directed graph layout for JavaScript")
    (description "Dagre is a JavaScript library that makes it easy to lay out
directed graphs on the client-side.")
    (license license:expat)))

;; Author recommends using cytoscape-popper with tippy.js since qtip2 is no longer maintained
(define-public javascript-cytoscape-qtip
  (package
    (name "javascript-cytoscape-qtip")
    (version "2.7.1") ; May 4, 2017
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-qtip")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1rdwdkx9j1lqzks8gi8ilkcbryswdx653569av4i74pv4j93v6zx"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-qtip"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/cytoscape-qtip.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (propagated-inputs ; TODO: Add qtip
     `(("javascript-cytoscape" ,javascript-cytoscape)
       ("jquery" ,web-jquery)))
    (home-page "https://github.com/cytoscape/cytoscape.js")
    (synopsis "Cytoscape.js extension that wraps the QTip jQuery library")
    (description "Cytoscape.")
    (license license:expat)))

(define-public javascript-cytoscape-popper
  (package
    (name "javascript-cytoscape-popper")
    (version "1.0.4") ; Feb 13, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-popper")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0nb59cimzp6zprk0czrfkc6id70ia2gg8drgmd55nf3yspn4b5rj"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-popper"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/cytoscape-popper.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (propagated-inputs `(("javascript-cytoscape" ,javascript-cytoscape)))
    (home-page "https://github.com/cytoscape/cytoscape.js-popper")
    (synopsis "Cytoscape.js extension for integrating Popper.js")
    (description "Popper.js allows you to dynamically align a div, e.g. a
tooltip, to another element in the page.  This extension allows you to use
Popper.js on Cytoscape elements.  This allows you to create DOM elements
positioned on or around Cytoscape elements.  It is useful for tooltips and
overlays, for example.")
    (license license:expat)))

(define-public javascript-qtip2
  (package
    (name "javascript-qtip2")
    (version "3.0.3") ; May 11, 2016
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/qTip2/qTip2.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06lmmy6mqbfdgbbyjm0v2bl1ifdv03rh6vqnsmmkn1s56kd2qr62"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/qtip2"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "http://qtip2.com/")
    (synopsis "Pretty powerful tooltips")
    (description "The second generation of the advanced qTip plugin for the
ever popular jQuery framework.")
    (license license:expat)))

(define-public javascript-datatables
  (package
    (name "javascript-datatables")
    (version "1.10.19") ; June 22, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/DataTables/DataTables.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0p85xzcvfjdrs4nwj7lhnlw2dmyky0hkwy8bzjw2fdabmsrdpwyg"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/DataTables"))
                (source (assoc-ref %build-inputs "source"))
                (media (string-append source "/media")))
           (copy-recursively media targetdir)))))
    (native-inputs `(("source" ,source)))
    (propagated-inputs `(("javascript-cytoscape" ,javascript-cytoscape)))
    (home-page "https://www.datatables.net/")
    (synopsis "Tables plug-in for jQuery")
    (description "DataTables is a table enhancing plug-in for the jQuery
Javascript library, adding sorting, paging and filtering abilities to plain HTML
tables with minimal effort.")
    (license license:expat)))
