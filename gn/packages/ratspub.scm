(define-module (gn packages ratspub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gn packages javascript)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gn packages web))

(define-public ratspub
  (let ((commit "19bc3078c06bef3c10c55dc6e69018e595d07d17")
        (revision "1"))
    (package
      (name "ratspub")
      (version (git-version "0.0.0" revision commit)) ; June 1, 2019
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/chen42/ratspub.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1v05rrjmiwrjmjspn487hd39403vb452hmk2l1l55rkm26781hwk"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (add-after 'unpack 'patch-javascript-references
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((cytoscape (assoc-ref inputs "cytoscape"))
                     (bootstrap (assoc-ref inputs "bootstrap")))
                 (substitute* "templates/cytoscape.html"
                   (("script src=.*")
                    (string-append "script src=\"" cytoscape
                                   "/share/genenetwork2/javascript/cytoscape/cytoscape.min.js\"></script>\n")))
                 (substitute* "templates/layout.html"
                   (("https://stackpath.bootstrapcdn.com/bootstrap/.*")
                    (string-append bootstrap "/share/web/bootstrap/css/bootstrap.min.css\">\n"))))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "static" (string-append out "/static"))
                 (copy-recursively "templates" (string-append out "/templates"))
                 (install-file "server.py" out))
               #t))
           (add-after 'install 'wrap-executable
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out  (assoc-ref outputs "out"))
                     (path (getenv "PYTHONPATH")))
                 (wrap-program (string-append out "/server.py")
                               `("PYTHONPATH" ":" prefix (,path))))
               #t)))))
      (inputs
       `(("python-flask" ,python-flask)
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
