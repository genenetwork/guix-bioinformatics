(define-module (gn packages bnw)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gcc)
  #:use-module (gn packages graphviz)
  #:use-module (gn packages javascript)
  #:use-module (gn packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gn packages web))

(define-public bnw
  (let ((commit "eb6b002b924694808384f1a8d7c6d1121806ae04")
        (revision "1"))
    (package
      (name "bnw")
      (version (git-version "1.22" revision commit)) ; June 28, 2019
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/ziejd2/BNW.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "10qwykp2zcyxih6a52icvy30ps69qk4v3jgirmdpw1l15zi4p2wq"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'patch-source-shebangs 'patch--more-shebangs
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((bash     (assoc-ref inputs "bash"))
                      (graphviz (assoc-ref inputs "graphviz"))
                      (octave   (assoc-ref inputs "octave"))
                      (python   (assoc-ref inputs "python"))
                      (rmath    (assoc-ref inputs "rmath"))
                      (jquery (assoc-ref inputs "jquery"))
                      (awesome (assoc-ref inputs "awesome"))
                      (cyto (assoc-ref inputs "cytoscape"))
                      (cyto2 (assoc-ref inputs "cytoscape-2"))
                      (cs-dagre (assoc-ref inputs "cyto-dagre"))
                      (d3js (assoc-ref inputs "d3js"))
                      (d3js-multi (assoc-ref inputs "d3js-multi"))
                      (dagre (assoc-ref inputs "dagre"))
                      (lodash (assoc-ref inputs "lodash"))
                      (canvas-toblob (assoc-ref inputs "canvas-toblob"))
                      (filesaver (assoc-ref inputs "filesaver"))
                      (panzoom (assoc-ref inputs "panzoom"))
                      (js-path "/share/genenetwork2/javascript/"))
                 (for-each (lambda (file)
                   (patch-shebang file
                     (list (string-append bash "/bin")
                           (string-append octave "/bin")
                           (string-append python "/bin"))))
                   (find-files "." ".*"))
                 (substitute*
                   (append '("home.php")
                           (find-files "sourcecodes" ".php")
                           (find-files "sourcecodes/run_scripts" ".*"))
                   (("/usr/bin/dot") (string-append graphviz "/bin/dot")))
                 (substitute* "sourcecodes/build.sh"
                   (("./localscore/libRmath.so") (string-append rmath "/lib/libRmath.so")))
                 (substitute* "sourcecodes/layout_cyto.php"
                   (("https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.7.1/cytoscape.min.js")
                    (string-append cyto js-path "cytoscape/cytoscape.min.js"))
                   (("https://cdnjs.cloudflare.com/ajax/libs/cytoscape/2.7.29/cytoscape.min.js")
                    (string-append cyto2 js-path "cytoscape/cytoscape.min.js"))
                   (("http://spades.bioinf.spbau.ru/~alla/graph_viewer/js/cytoscape-dagre.js")
                    (string-append cs-dagre js-path "cytoscape-dagre/cytoscape-dagre.js"))
                   (("https://unpkg.com/dagre@0.7.4/dist/dagre.js")
                    (string-append dagre js-path "dagre/dagre.js"))
                   (("https://cdnjs.cloudflare.com/ajax/libs/cytoscape-panzoom/2.5.3/cytoscape.js-panzoom.css")
                    (string-append panzoom js-path "cytoscape-panzoom/cytoscape.js-panzoom.css"))
                   (("https://cdnjs.cloudflare.com/ajax/libs/cytoscape-panzoom/2.5.3/cytoscape-panzoom.js")
                    (string-append panzoom js-path "cytoscape-panzoom/cytoscape-panzoom.js"))
                   (("https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.0.3/css/font-awesome.css")
                    (string-append awesome "/share/web/font-awesome/css/font-awesome.css"))
                   (("https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js")
                    (string-append jquery "/share/web/jquery/jquery.min.js"))
                   (("https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.10/lodash.js")
                    (string-append lodash js-path "lodash/lodash.js")))
                 (substitute* '("sourcecodes/layout_svg_wt.php"
                                "sourcecodes/layout_svg_no.php")
                   (("http://d3js.org/d3.v4.min.js")
                    (string-append d3js js-path "d3js/d3.min.js"))
                   (("http://d3js.org/d3-selection-multi.v1.js")
                    (string-append d3js-multi js-path "d3js-multi/d3-selection-multi.js"))
                   (("https://cdn.rawgit.com/eligrey/canvas-toBlob.js/f1a01896135ab378aa5c0118eadd81da55e698d8/canvas-toBlob.js")
                    (string-append canvas-toblob js-path "canvas-toBlob/canvas-toBlob.js"))
                   (("https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js")
                    (string-append filesaver js-path "filesaver/filesaver.js"))))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (delete-file "sourcecodes/localscore/libRmath.so")
                 (copy-recursively "." out))
               #t))
           (replace 'build
             (lambda _
               (with-directory-excursion "sourcecodes"
                 (substitute* "build.sh"
                   (("./localscore") "localscore"))
                 (chmod "k-best/src/buildk_poster.sh" #o755)
                 (invoke "sh" "build.sh")))))))
      (inputs
       `(("graphviz" ,graphviz-2.26)
         ("octave" ,octave-3.4.3)
         ("python" ,python-2)
         ("rmath" ,rmath-standalone) ; Should this be r-minimal? Which version?
         ("jquery" ,web-jquery)
         ("awesome" ,web-font-awesome)
         ("cytoscape" ,javascript-cytoscape)
         ("cytoscape-2" ,javascript-cytoscape-2)
         ("d3js" ,javascript-d3js-4)
         ("d3js-multi" ,javascript-d3js-multi)
         ("dagre" ,javascript-dagre)
         ("lodash" ,javascript-lodash)
         ("canvas-toblob" ,javascript-canvas-toblob)
         ("filesaver" ,javascript-filesaver)
         ("cyto-dagre" ,javascript-cytoscape-dagre)
         ("panzoom" ,javascript-cytoscape-panzoom)))
      (native-inputs
       `(("gcc" ,gcc-5)))
      (home-page "http://compbio.uthsc.edu/BNW/")
      (synopsis "Bayesian Network Webserver")
      (description "This contains the code for the @dfn{Bayesian Network Webserver} (BNW).")
      (license (list license:gpl2
                     license:gpl2+
                     license:lgpl2.1+)))))
