(define-module (gn packages ratspub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gn packages javascript)
  #:use-module (gn packages machine-learning)
  #:use-module (gn packages web))

(define-public ratspub
  (package
    (name "ratspub")
    (version "0.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/chen42/ratspub")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              ;; Keep the service running on port 4200.
              (modules '((guix build utils)))
              (snippet
               '(begin (substitute* "server.py"
                         (("4201") "4200"))
                       #t))
              (sha256
               (base32
                "03v1nk58yhi7rsy6b23hlc1xd5i6lvqla25r9l4b1jcmzraaci8l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'patch-datadir
           (lambda _
             (substitute* "server.py"
               (("^datadir.*") "datadir = \"/export/ratspub/\"\n"))
             #t))
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out       (assoc-ref outputs "out"))
                   (inetutils (assoc-ref inputs "inetutils")))
               (substitute* '("templates/cytoscape.html"
                              "templates/tableview.html"
                              "templates/tableview0.html"
                              "templates/userarchive.html")
                 (("https.*FileSaver.js.*\\\">") "/static/FileSaver.js\">")
                 (("https.*cytoscape-svg.js.*\\\">") "/static/cytoscape-svg.js\">")
                 (("https.*cytoscape.min.js.*\\\">") "/static/cytoscape.min.js\">"))
               (substitute* "templates/layout.html"
                 (("https.*bootstrap.min.css.*\\\">") "/static/bootstrap.min.css\">")
                 (("https.*4.*bootstrap.min.js.*\\\">") "/static/bootstrap.min.js\">")
                 (("https.*4.7.0/css/font-awesome.min.css") "/static/font-awesome.min.css")
                 (("https.*jquery-3.2.1.slim.min.js.*\\\">") "/static/jquery.slim.min.js\">")
                 (("https.*1.12.9/umd/popper.min.js.*\\\">") "/static/popper.min.js\">"))
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
                   (cytoscape-svg (assoc-ref inputs "cytoscape-svg"))
                   (jquery    (assoc-ref inputs "jquery"))
                   (js-filesaver (assoc-ref inputs "js-filesaver"))
                   (js-popper (assoc-ref inputs "js-popper")))
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
               (symlink (string-append cytoscape-svg
                                       "/share/javascript/cytoscape-svg.js")
                        (string-append out "/static/cytoscape-svg.js"))
               (symlink (string-append jquery
                                       "/share/web/jquery/jquery.slim.min.js")
                        (string-append out "/static/jquery.slim.min.js"))
               (symlink (string-append js-filesaver
                                       "/share/javascript/FileSaver.js")
                        (string-append out "/static/FileSaver.js"))
               (symlink (string-append js-popper
                                       "/share/javascript/popper.min.js")
                        (string-append out "/static/popper.min.js")))
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
       ("python-flask-sqlalchemy" ,python-flask-sqlalchemy)
       ("python-keras" ,python-keras-for-ratspub)
       ("python-nltk" ,python-nltk)
       ("tensorflow" ,tensorflow)))
    (native-inputs
     `(("bootstrap" ,web-bootstrap)
       ("cytoscape" ,javascript-cytoscape)
       ;("cytoscape-svg" ,js-cytoscape-svg-0.3.1)   ; TODO
       ("cytoscape-svg" ,js-cytoscape-svg-vendor-0.3.1)
       ("font-awesome" ,web-font-awesome)
       ("jquery" ,web-jquery)
       ("js-filesaver" ,js-filesaver-1.3.2)
       ("js-popper" ,js-popper-1.12.9)))
    (home-page "http://rats.pub/")
    (synopsis "Relationship with Addiction Through Searches of PubMed")
    (description
     "RatsPub is a tool to efficiently and comprehensively answer the question
\"What do we know about these genes and addiction?\".  RatsPub answers this
question by searching PubMed to find sentences containing the query terms (i.e.,
gene symbols) and over 300 drug addiction-related keywords that are organized
into six categories.  Data from @url{https://www.ebi.ac.uk/gwas/,
@acronym{NHGRI-EBI GWAS, European Bioinformatics Institute Genome-Wide
Association Studies}} catalog are also included in the search.  These
gene-keyword relationships are presented as an interactive graph and a table.")
    (license license:expat)))

(define-public ratspub-with-tensorflow-native
  (package
    (inherit
      (tensowflow-native-instead-of-tensorflow ratspub))
    (name "ratspub-with-tensorflow-native")))

;; We want a copy of python-keras with the AUC optimizer backported.
;; We skip the tests because we "test in production".
;; That's a lie. The test suite just takes a long time to run.
(define-public python-keras-for-ratspub
  (hidden-package
    (package
      (inherit python-keras)
      (source
        (origin
          (inherit (package-source python-keras))
          (patches (search-patches "keras-auc-optimizer.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments python-keras)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'check))))))))

(define-public hrdp-project
  (package
    (name "hrdp-project")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/noderboarder/hrdp-project")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ag7jm43p35yh0cqcn53wg4iw7sgfypw10mxq5klxvhgj3r6cf7i"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'patch-sources
           (lambda _
             (substitute* "./app/templates/layout.html"
               (("https://.*.bootstrapcdn.com/bootstrap/4.*/css/bootstrap.min.css.*")
                "/static/bootstrap.min.css\">\n")
               (("https://.*.bootstrapcdn.com/bootstrap/4.*/js/bootstrap.min.js.*")
                "/static/bootstrap.min.js\"></script>\n")
               (("https://code.jquery.com/jquery-3.*.slim.min.js.*")
                "/static/jquery.slim.min.js\"></script>\n")
               ;(("https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js.*")
               ; "/static/popper.min.js\"></script>\n")
               )
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (python (assoc-ref inputs "python")))
               (delete-file "main.py")
               (with-output-to-file "main.py"
                 (lambda ()
                   (format #t "#!~a/bin/python
from app import create_app

app = create_app()

if __name__ == '__main__':
    app.run(debug=True, port=4222)~%"
                   python)))
               (chmod "main.py" #o555)
               (copy-recursively "." out))
             #t))
         (add-after 'install 'install-javascript
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out       (assoc-ref outputs "out"))
                   (bootstrap (assoc-ref inputs "bootstrap"))
                   (jquery    (assoc-ref inputs "jquery"))
                   ;(js-popper (assoc-ref inputs "js-popper"))
                   )
               (symlink (string-append bootstrap
                                       "/share/web/bootstrap/css/bootstrap.min.css")
                        (string-append out "/app/static/bootstrap.min.css"))
               (symlink (string-append bootstrap
                                       "/share/web/bootstrap/js/bootstrap.min.js")
                        (string-append out "/app/static/bootstrap.min.js"))
               (symlink (string-append jquery
                                       "/share/web/jquery/jquery.slim.min.js")
                        (string-append out "/app/static/jquery.slim.min.js"))
               ;(symlink (string-append js-popper
               ;                        "/share/web/popper/popper.min.js")
               ;         (string-append out "/static/popper.min.js"))
               )
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (path (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/main.py")
                `("PYTHONPATH" ":" prefix (,path))))
             #t)))))
    (inputs
     `(("python" ,python)
       ("python-flask-sqlalchemy" ,python-flask-sqlalchemy)))
    (native-inputs
     `(("bootstrap" ,web-bootstrap)
       ("jquery" ,web-jquery)
       ;("js-popper" ,js-popper)    ; empty output
       ))
    (home-page "https://github.com/noderboarder/hrdp-project")
    (synopsis "")
    (description "")
    (license license:expat)))
