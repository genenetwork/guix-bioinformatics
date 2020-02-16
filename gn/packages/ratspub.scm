(define-module (gn packages ratspub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages admin)
  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages javascript)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gn packages web))

(define-public ratspub
  (package
    (name "ratspub")
    (version "0.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/chen42/ratspub.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0cm9g38fxpa52458mdmhzhghj5c7b8l3k1b764zs9hdrki5s7wi7"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out       (assoc-ref outputs "out"))
                   (inetutils (assoc-ref inputs "inetutils")))
               (substitute* "templates/cytoscape.html"
                 (("script src=.*")
                  "script src=\"/static/cytoscape.min.js\"></script>\n"))
               (substitute* "templates/layout.html"
                 (("https://stackpath.bootstrapcdn.com/bootstrap/.*")
                  "/static/bootstrap.min.css\">\n"))
               (substitute* "ratspub.py"
                 (("hostname") (string-append inetutils "/bin/hostname"))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." out))
             #t))
         (add-after 'install 'install-javascript
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out       (assoc-ref outputs "out"))
                   (cytoscape (assoc-ref inputs "cytoscape"))
                   (bootstrap (assoc-ref inputs "bootstrap")))
               (symlink (string-append cytoscape
                                       "/share/genenetwork2/javascript/cytoscape/cytoscape.min.js")
                        (string-append out "/static/cytoscape.min.js"))
               (symlink (string-append bootstrap
                                       "/share/web/bootstrap/css/bootstrap.min.css")
                        (string-append out "/static/bootstrap.min.css")))
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (path (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/server.py")
                 `("PATH" ":" prefix (,(dirname (which "edirect.pl"))
                                      ,(dirname (which "dirname"))
                                      ,(dirname (which "grep"))
                                      ,(dirname (which "sed"))))
                 `("PYTHONPATH" ":" prefix (,path))))
             #t)))))
    (inputs
     `(("edirect" ,edirect-gn)
       ("inetutils" ,inetutils)
       ("python-flask" ,python-flask)
       ("python-nltk" ,python-nltk)))
    (native-inputs
     `(("cytoscape" ,javascript-cytoscape)
       ("bootstrap" ,web-bootstrap)))
    (home-page "http://rats.pub/")
    (synopsis "Relationship with Addiction Through Searches of PubMed")
    (description
     "RatsPub searches PubMed to find sentences that contain the query terms
(i.e., gene symbols) and drug addiction-related keywords.  These gene-keyword
relationships are presented as an interactive graph that can efficiently answer
the question \"What do we know about these genes and addiction?\".  Data from
@acronym{EBI GWAS, European Bioinformatics Institute Genome-Wide Association
Studies} catalog are also included in the search to better answer this
question.")
    (license #f)))
