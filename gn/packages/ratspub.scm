(define-module (gn packages ratspub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gn packages javascript)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gn packages web))

(define-public ratspub
  (package
    (name "ratspub")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/chen42/ratspub.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q111fsch69kjv96d0pld9agqpwazh9p47vsvig63hmmjp72wa84"))))
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
               (substitute* '("templates/cytoscape.html"
                              "templates/tableview.html"
                              "templates/tableview0.html"
                              "templates/userarchive.html")
                 (("script src=.*")
                  "script src=\"/static/cytoscape.min.js\"></script>\n"))
               (substitute* "templates/layout.html"
                 (("https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css.*")
                  "/static/bootstrap.min.css\">\n")
                 (("https://.*.bootstrapcdn.com/bootstrap/4.*/js/bootstrap.min.js.*")
                  "/static/bootstrap.min.js\"></script>\n")
                 (("https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
                  "/static/font-awesome.min.css")
                 (("https://code.jquery.com/jquery-3.2.1.slim.min.js.*")
                  "/static/jquery.slim.min.js\"></script>\n")
                 ;(("https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js.*")
                 ; "/static/popper.min.js\"></script>\n")
                 )
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
                   (awesome   (assoc-ref inputs "font-awesome"))
                   (bootstrap (assoc-ref inputs "bootstrap"))
                   (cytoscape (assoc-ref inputs "cytoscape"))
                   (jquery    (assoc-ref inputs "jquery"))
                   ;(js-popper (assoc-ref inputs "js-popper"))
                   )
               (symlink (string-append awesome
                                       "/share/web/font-awesomecss/font-awesome.min.css")
                        (string-append out "/static/font-awesome.min.css"))
               (symlink (string-append bootstrap
                                       "/share/web/bootstrap/css/bootstrap.min.css")
                        (string-append out "/static/bootstrap.min.css"))
               (symlink (string-append bootstrap
                                       "/share/web/bootstrap/js/bootstrap.min.js")
                        (string-append out "/static/bootstrap.min.js"))
               (symlink (string-append cytoscape
                                       "/share/genenetwork2/javascript/cytoscape/cytoscape.min.js")
                        (string-append out "/static/cytoscape.min.js"))
               (symlink (string-append jquery
                                       "/share/web/jquery/jquery.slim.min.js")
                        (string-append out "/static/jquery.slim.min.js"))
               ;(symlink (string-append js-popper
               ;                        "/share/web/popper/popper.min.js")
               ;         (string-append out "/static/popper.min.js"))
               )
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
     `(("edirect" ,edirect)
       ("inetutils" ,inetutils)
       ("python-bcrypt" ,python-bcrypt)
       ("python-flask" ,python-flask)
       ("python-flask-sqlalchemy" ,python-flask-sqlalchemy)
       ("python-nltk" ,python-nltk)
       ("python-pytz" ,python-pytz)))
    (native-inputs
     `(("bootstrap" ,web-bootstrap)
       ("cytoscape" ,javascript-cytoscape)
       ("font-awesome" ,web-font-awesome)
       ("jquery" ,web-jquery)
       ;("js-popper" ,js-popper)
       ))
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
    (license license:expat)))
