(define-module (gn packages javascript)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gn packages web)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system minify)
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

(define-public javascript-cytoscape-dagre
  (package
    (name "javascript-cytoscape-dagre")
    (version "2.2.2") ; Sept. 26, 2018
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cytoscape/cytoscape.js-dagre")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0z0sh5q5cd0iirdyhlln83vmsvwn1sbh4zdmdh8k5hld075g4q64"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/cytoscape-dagre"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/cytoscape-dagre.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (propagated-inputs
     `(("javascript-cytoscape" ,javascript-cytoscape)
       ("javascript-dagre" ,javascript-dagre)))
    (home-page "https://github.com/cytoscape/cytoscape.js-dagre")
    (synopsis "Dagre layout for DAGs and trees for Cytoscape.js")
    (description "The dagre layout organises the graph using a @dfn{directed
acyclic graph} (DAG) system, written by Chris Pettitt.  It is especially
suitable for DAGs and trees.")
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

(define-public javascript-lodash
  (package
    (name "javascript-lodash")
    (version "4.17.15") ; July 17, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lodash/lodash")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1hp04cg3b59j3dpnvzixd5p6wpv34mj2pnq8wp60csv3p2s0qk4y"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/lodash"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://lodash.com")
    (synopsis "JavaScript utility library delivering modularity, performance & extras")
    (description "Lodash makes JavaScript easier by taking the hassle out of
working with arrays, numbers, objects, strings, etc. Lodash's modular methods
are great for:
@enumerate
@item Iterating arrays, objects, & strings
@item Manipulating & testing values
@item Creating composite functions
@end enumerate")
    (license license:expat)))

(define-public javascript-d3js
  (package
    (name "javascript-d3js")
    (version "5.9.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/d3/d3/releases/download/v"
                            version "/d3.zip"))
        (sha256
         (base32 "0vg1cfgg7p897bg0nhaprgr77kgvi8k38h5w6sl54nxvibvynkhc"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (name "d3js")
                (unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (targetdir (string-append out "/share/genenetwork/javascript/" name))
                (source (assoc-ref %build-inputs "source")))
           (invoke unzip source)
           (install-file "d3.js" targetdir)
           (install-file "d3.min.js" targetdir)
           (install-file "LICENSE" (string-append out "/share/doc/d3js-" ,version))))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "https://d3js.org/")
    (synopsis "JavaScript library for visualizing data")
    (description "D3.js is a JavaScript library for manipulating documents based
on data.  D3 helps you bring data to life using HTML, SVG, and CSS.  D3's
emphasis on web standards gives you the full capabilities of modern browsers
without tying yourself to a proprietary framework, combining powerful
visualization components and a data-driven approach to DOM manipulation.")
    (license license:bsd-3)))

(define-public javascript-d3js-4
  (package
    (inherit javascript-d3js)
    (version "4.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/d3/d3/releases/download/v"
                            version "/d3.zip"))
        (sha256
         (base32 "06yqgyvkpvh0lg7rsh4hjzq72fylkd8ziqcf7yhgy510x0mqla19"))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (name "d3js")
                (unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (targetdir (string-append out "/share/genenetwork/javascript/" name))
                (source (assoc-ref %build-inputs "source")))
           (invoke unzip source)
           (install-file "d3.js" targetdir)
           (install-file "d3.min.js" targetdir)
           (install-file "LICENSE" (string-append out "/share/doc/d3js-" ,version))))))))

(define-public javascript-d3js-multi
  (package
    (name "javascript-d3js-multi")
    (version "1.0.1") ; Feb, 21, 2017
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/d3/d3-selection-multi/releases"
                            "/download/v" version "/d3-selection-multi.zip"))
        (sha256
         (base32 "0k89n15ggpzsvf7qflmsagkrrhigk6nksdyks0ccx3453gizbb4q"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (name "d3js-multi")
                (unzip (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip"))
                (targetdir (string-append out "/share/genenetwork/javascript/" name))
                (source (assoc-ref %build-inputs "source")))
           (invoke unzip source)
           (install-file "d3-selection-multi.js" targetdir)
           (install-file "d3-selection-multi.min.js" targetdir)
           (install-file "LICENSE" (string-append out "/share/doc/" ,name "-" ,version))))))
    (native-inputs
     `(("source" ,source)
       ("unzip" ,unzip)))
    (home-page "https://d3js.org/")
    (synopsis "Multi-value syntax for d3-selection and d3-transition")
    (description "This module adds multi-value syntax to selections and
transitions, allowing you to set multiple attributes, styles or properties
simultaneously with more concise syntax.")
    (license license:bsd-3)))

(define-public javascript-canvas-toblob
  (let ((commit "f1a01896135ab378aa5c0118eadd81da55e698d8") ; May 26, 2016
        (revision "1"))
    (package
      (name "javascript-canvas-toblob")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/eligrey/canvas-toBlob.js")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "1ly7qnavf9h26sgynccf00hf2ybdwyn0kvnl7i3milz3big02qdm"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (assoc-ref %outputs "out"))
                  (name "canvas-toblob")
                  (targetdir (string-append out "/share/genenetwork/javascript/" name))
                  (source (assoc-ref %build-inputs "source")))
             (install-file (string-append source "/canvas-toBlob.js") targetdir)
             (install-file (string-append source "/LICENSE.md")
                           (string-append out "/share/doc/" ,name "-" ,version))))))
      (native-inputs
       `(("source" ,source)))
      (home-page "https://github.com/eligrey/canvas-toBlob.js/")
      (synopsis "canvas.toBlob() implementation")
      (description "canvas-toBlob.js implements the standard HTML5
@code{canvas.toBlob()} and @code{canvas.toBlobHD()} methods in browsers that do
not natively support it.")
      (license license:expat))))

(define-public javascript-filesaver
  (package
    (name "javascript-filesaver")
    (version "2.0.2") ; May 14, 2019
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/eligrey/FileSaver.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ij3vmv8n2ia9kbyih3g479rj68xrsiq7l9s29vv1bdmmk41lpf3"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/filesaver"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://github.com/eligrey/FileSaver.js")
    (synopsis "HTML5 saveAs() FileSaver implementation")
    (description "FileSaver.js is the solution to saving files on the
client-side, and is perfect for web apps that generates files on the client.")
    (license license:expat)))

(define-public javascript-underscore
  (package
    (name "javascript-underscore")
    (version "1.9.1") ; June 1, 2018
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jashkenas/underscore.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1f75wrln5kv5ihkbb9zwhyjqd9imwil801abhv36w09dkkabpjy5"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/underscore"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/underscore.js") targetdir)
           (install-file (string-append source "/underscore-min.js") targetdir)
           (install-file (string-append source "/underscore-min.js.map") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://underscorejs.org")
    (synopsis "Utility-belt library for JavaScript")
    (description
     "Underscore is a JavaScript library that provides a whole mess of useful
functional programming helpers without extending any built-in objects.")
    (license license:expat)))

(define-public js-underscore
  (package
    (inherit javascript-underscore)
    (name "js-underscore")
    (arguments
     `(#:javascript-files '("underscore.js")))
    (build-system minify-build-system)))

(define-public javascript-smart-time-ago
  (let ((commit "055c3858997b12d44bf06c0fb9eb5847002cf973")
        (revision "1"))
    (package
      (name "javascript-smart-time-ago")
      (version (git-version "0.1.5" revision commit)) ; Feb, 21, 2014
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/pragmaticly/smart-time-ago.git")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0fj5vf3s3rj7ywvx1s4gh6z0yljn9ax75y2ikf1d41c0lzaxdpyd"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (assoc-ref %outputs "out"))
                  (targetdir (string-append out "/share/genenetwork2/javascript/smart-time-ago"))
                  (source (assoc-ref %build-inputs "source")))
             (install-file (string-append source "/lib/timeago.js") targetdir)))))
      (native-inputs `(("source" ,source)))
      (home-page "http://pragmaticly.github.com/smart-time-ago/")
      (synopsis "jQuery library to update the relative timestamps")
      (description
       "Smart Time Ago is a little jQuery library to update the relative
timestamps in your document intelligently.  (e.g \"3 hours ago\").")
      (license license:expat))))

(define-public js-smart-time-ago
  (package
    (inherit javascript-smart-time-ago)
    (name "js-smart-time-ago")
    (arguments
     `(#:javascript-files '("lib/timeago.js")))
    (build-system minify-build-system)))

(define-public javascript-colorbox
  (package
    (name "javascript-colorbox")
    (version "1.4.36") ; Feb. 11, 2014
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jackmoore/colorbox.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yjikn0mc1cmhcl3wbd5pjspi6n75swazsahm616xlra73qpagfn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/colorbox"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/jquery.colorbox.js") targetdir)
           (install-file (string-append source "/jquery.colorbox-min.js") targetdir)
           ))))
    (native-inputs `(("source" ,source)))
    (home-page "http://www.jacklmoore.com/colorbox/")
    (synopsis "Lightweight customizable lightbox plugin for jQuery")
    (description
     "Colorbox is a lightweight customizable lightbox plugin for jQuery.")
    (license license:expat)))

(define-public js-colorbox
  (package
    (inherit javascript-colorbox)
    (name "js-colorbox")
    (arguments
     `(#:javascript-files '("jquery.colorbox.js")))
    (build-system minify-build-system)))

(define-public javascript-nouislider
  (package
    (name "javascript-nouislider")
    (version "8.0.2") ; July 6, 2015
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/leongersen/noUiSlider.git")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ngfll2hr9w2q4869n0prfn66lcfyjshvhq4pgi0lb63xla8asfp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/nouislider"))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/distribute")))
           (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://refreshless.com/nouislider/")
    (synopsis "Javascript range slider")
    (description
     "Nouislider is a lightweight JavaScript range slider with full touch support.")
    (license license:expat)))

(define-public js-nouislider
  (package
    (inherit javascript-nouislider)
    (name "js-nouislider")
    (arguments
     `(#:javascript-files '("distribute/nouislider.js")))
    (build-system minify-build-system)))

(define-public javascript-chroma
  (package
    (name "javascript-chroma")
    (version "1.1.1") ; Aug. 15, 2015
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gka/chroma.js")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "184bd2cddd9b5ynj2ygr5p7xkrrxnmnjyq5ljyw6g4aqqk4pb0mr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (targetdir (string-append out "/share/genenetwork2/javascript/chroma"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/chroma.js") targetdir)
           (install-file (string-append source "/chroma.min.js") targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://vis4.net/chromajs/")
    (synopsis "Javascript library for color conversions")
    (description
     "chroma.js is a small-ish zero-dependency JavaScript library for all kinds
of color conversions and color scales.  chroma.js can also help you generate
nice colors using various methods, for instance to be used in color palette for
maps or data visualization.")
    (license (list license:bsd-3 license:asl2.0))))

(define-public js-chroma
  (package
    (inherit javascript-chroma)
    (name "js-chroma")
    (arguments
     `(#:javascript-files '("chroma.js")))
    (build-system minify-build-system)))
