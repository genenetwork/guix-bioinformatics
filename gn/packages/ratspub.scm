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
  (let ((commit "88ab6a3a21fcc2715c9a7528d8b4d15bb7ea85ed")
        (revision "2"))
    (package
      (name "ratspub")
      (version (git-version "0.0.0" revision commit)) ; Aug 30, 2019
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/chen42/ratspub.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "0gg2wikajbz248qgg7vyclrmjhdw01wb721zvzgil9qd2gm0g4c6"))))
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
                     (cytoscape (assoc-ref inputs "cytoscape"))
                     (bootstrap (assoc-ref inputs "bootstrap"))
                     (inetutils (assoc-ref inputs "inetutils")))
                 ;; Source cannot find the substituted css currently.
                 ;(substitute* "templates/cytoscape.html"
                 ;  (("script src=.*")
                 ;   (string-append "script src=\"" cytoscape
                 ;                  "/share/genenetwork2/javascript/cytoscape/cytoscape.min.js\"></script>\n")))
                 ;(substitute* "templates/layout.html"
                 ;  (("https://stackpath.bootstrapcdn.com/bootstrap/.*")
                 ;   (string-append bootstrap "/share/web/bootstrap/css/bootstrap.min.css\">\n")))
                 (substitute* "ratspub.py"
                   (("hostname") (string-append inetutils "/bin/hostname"))))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "." out))
               #t))
           (add-after 'install 'wrap-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out  (assoc-ref outputs "out"))
                     (path (getenv "PYTHONPATH")))
                 (wrap-program (string-append out "/server.py")
                   `("PATH" ":" prefix (,(dirname (which "edirect.pl"))))
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
      (license #f))))
