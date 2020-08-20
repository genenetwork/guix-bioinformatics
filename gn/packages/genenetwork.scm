;; Bioinformatics module

(define-module (gn packages genenetwork)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml)
  #:use-module (past packages graphviz)
  #:use-module (past packages python)
  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages twint)
  #:use-module (gn packages crates-io)
  #:use-module (gn packages elixir)
  #:use-module (gn packages gemma)
  #:use-module (gn packages javascript)
  #:use-module (gn packages phewas)
  #:use-module (gn packages python)
  #:use-module (gn packages python24)
  #:use-module (gn packages statistics)
  #:use-module (gn packages web)
  #:use-module (srfi srfi-1))



(define-public my-deploy
  (package
    (name "my-deploy")
    (version "0.0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (let* ((out  (assoc-ref %outputs "out"))
              (bash (assoc-ref %build-inputs "bash"))
              (foo  (string-append out "/foo")))
         (begin
           (use-modules (guix build utils))
           (mkdir out)
           (call-with-output-file foo
             (lambda (p)
               (format p
                       "#!~a~%echo \"${GUIX_FOO} ${GUIX_BAR}\"~%"
                       bash)))
           (chmod foo #o777)
           ;; wrap-program uses `which' to find bash for the wrapper
           ;; shebang, but it can't know about the bootstrap bash in
           ;; the store, since it's not named "bash".  Help it out a
           ;; bit by providing a symlink to this package's output.
           (symlink bash (string-append out "/bash"))
           (setenv "PATH" out)
           (wrap-program foo `("GUIX_FOO" prefix ("hello")))
           (wrap-program foo `("GUIX_BAR" prefix ("world")))
           #t))))
    (inputs `(("bash" ,(bootstrap-executable "bash"
                                             (%current-system)))))

    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public gn-server
  (let ((md5 "93e745e9c"))
    (package
    (name "gn-server")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "http://files.genenetwork.org/raw_database/md5sum.txt") ; any old file
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "1cnkiwid4h0nnf93rm647ji9vhfzjl23arp1xj374la7mmic9jqs"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    (inputs `(("sassc" ,sassc)))
    (propagated-inputs
     `(("python" ,python)
       ("elixir" ,elixir)
       ("mariadb" ,mariadb)
       ("gnu-make" ,gnu-make) ; needed for mysqlex
       ))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share")))
             (write target)
             (mkdir-p target)
             ; (copy-recursively (assoc-ref %build-inputs "source") target)
             #t))))

    (home-page "http://github.com/genenetwork/gn_server/")
    (synopsis "GN REST server API")
    (description "REST Server")
    (license license:agpl3+))))

(define-public python2-qtlreaper
  (let ((commit "442c217b90393380a8634ff8636b44992f5c53dd"))
  (package
    (name "python2-qtlreaper")
    (version (string-append "1.11-gn2-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://github.com/pjotrp/QTLreaper.git")
                   (commit commit)))
             (file-name (string-append name "-" (string-take commit 7)))
             (sha256
              (base32
               "1rrbm1ap2zzyjxmrs9aa1d18sgiba5dhj1fmkl7wmab06jv3j1hm"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://qtlreaper.sourceforge.net/")
    (synopsis "Scan expression data for QTLs")
    (description
     "Batch-oriented version of WebQTL. It requires, as input,
expression data from members of a set of recombinant inbred lines and
genotype information for the same lines.  It searches for an
association between each expression trait and all genotypes and
evaluates that association by a permutation test.  For the permutation
test, it performs only as many permutations as are necessary to define
the empirical P-value to a reasonable precision. It also performs
bootstrap resampling to estimate the confidence region for the
location of a putative QTL.")
    (license license:gpl2+))))

(define-public python24-qtlreaper
  (let ((commit "dd9c7fb2a9d5fa40b4054e1bcb7c57905d98d5f8"))
  (package
    (name "python24-qtlreaper")
    (version (string-append "1.1-gn2-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://github.com/pjotrp/QTLreaper.git")
                   (commit commit)))
             (file-name (string-append name "-" (string-take commit 7)))
             (sha256
              (base32
               "1ldcvyk8y8w6f4ci04hzx85sknd5a3h424p5bfi4fz32sm2p7fja"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:tests? #f))   ; no 'setup.py test' really!
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (home-page "http://qtlreaper.sourceforge.net/")
    (synopsis "Scan expression data for QTLs")
    (description
     "Batch-oriented version of WebQTL. It requires, as input,
expression data from members of a set of recombinant inbred lines and
genotype information for the same lines.  It searches for an
association between each expression trait and all genotypes and
evaluates that association by a permutation test.  For the permutation
test, it performs only as many permutations as are necessary to define
the empirical P-value to a reasonable precision. It also performs
bootstrap resampling to estimate the confidence region for the
location of a putative QTL.")
    (license license:gpl2+))))

;; Reintroduced python2-gunicorn because we are running GN with python2
;; right now. Please keep it until we migrate to Python3 fully!

(define-public python-gunicorn-gn
  (package
    (name "python-gunicorn-gn")
    (version "19.9.0")
    (source 
      (origin
        (method url-fetch)
        (uri (pypi-uri "gunicorn" version))
        (sha256
         (base32
          "1wzlf4xmn6qjirh5w81l6i6kqjnab1n1qqkh7zsj1yb6gh4n49ps"))))
    (build-system python-build-system)
    (inputs
     `(("python-mock" ,python-mock)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-verion-restrictions
           (lambda _
             (substitute* "requirements_test.txt"
               (("coverage.*") "coverage\n")
               (("pytest.*") "pytest\n")
               (("pytest-cov.*") "pytest-cov\n"))
             #t)))))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://gunicorn.org")
    (synopsis "WSGI HTTP Server for UNIX")
    (description "Gunicorn 'Green Unicorn' is a Python WSGI HTTP Server for
UNIX.  It's a pre-fork worker model ported from Ruby's Unicorn project.  The
Gunicorn server is broadly compatible with various web frameworks, simply
implemented, light on server resource usage, and fairly speedy.")
    (license license:expat)))


(define-public python2-gunicorn-gn
  (package-with-python2 python-gunicorn-gn))
  

(define-public rust-qtlreaper
  (let ((commit "2e7fed6d45b0b602d80fa2a55835f96ef1cba9e3")
        (revision "1"))
    (package
      (name "rust-qtlreaper")
      (version "0.1.4")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/chfi/rust-qtlreaper.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0h70aalsplmc6xn1w7ha102n3bsi3gqkbnbrjvjm2za37c07gv0g"))))
      (build-system cargo-build-system)
      (arguments
       `(#:cargo-inputs
         (("rust-rand" ,rust-rand-0.6)
          ("rust-structopt" ,rust-structopt-0.2)
          ("rust-rayon" ,rust-rayon-1)
          ("rust-serde" ,rust-serde-1)
          ("rust-serde-json" ,rust-serde-json-1)
          ("rust-ndarray" ,rust-ndarray-0.12))
         #:phases
         (modify-phases %standard-phases
           ;; Test results vary based on the machine running them.
           (replace 'check
             (lambda _
               (or (assoc-ref %standard-phases 'check)
                   (begin
                     (substitute* "src/geneobject.rs"
                       ;; array![Genotype::Unk, Genotype::Unk, Genotype::Pat]
                       (("0.3421367343627405") "0.3421367343627406")
                       ;; array![Genotype::Unk, Genotype::Unk, Genotype::Unk]
                       (("-0.3223330030526561") "-0.32233300305265566"))
                     (assoc-ref %standard-phases 'check)))
               #t)))))
      (home-page "https://github.com/chfi/rust-qtlreaper")
      (synopsis "Reimplementation of genenetwork/QTLReaper in Rust")
      (description "Reimplementation of genenetwork/QTLReaper in Rust")
      (license #f))))

(define-public gfautil
  (package
    (name "gfautil")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfautil" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0cgiis9v1nd4m7zxvgsz8jf8ijv4d8fa5wb7cpnjshksb8z7xh69"))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-1.42
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-gfa" ,rust-gfa-0.6)
        ("rust-handlegraph" ,rust-handlegraph-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-structopt" ,rust-structopt-0.3))))
    (home-page "https://github.com/chfi/rs-gfa-utils")
    (synopsis "Command line tools for working with GFA files")
    (description
     "This package provides command line tools for working with @acronym{GFA,
Graphical Fragment Assembly} files and related formats.")
    (license license:expat)))

(define-public genenetwork2
  (let ((commit "1538ffd33af19e6ac922b4ee85fe701408968dfd"))
  (package
    (name "genenetwork2")
    (version (string-append "2.11-guix-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://pjotrp@gitlab.com/genenetwork/gn2_diet.git")
                   (commit commit)))
             (file-name (string-append name "-" version))
             (sha256
              (base32
               "0ji929xgzypyhchcfy9xa1sz04w322ibs2khc8s3qiddxjqdglrz"))))
    (propagated-inputs
     `(;; propagated for development purposes
       ("python" ,python-2) ;; probably superfluous
       ("coreutils" ,coreutils)
       ("git" ,git)
       ("vim" ,vim)
       ("which" ,which)
       ("grep" ,grep)
       ("r" ,r)
       ("r-ctl" ,r-ctl)
       ; ("r-phewas" ,r-phewas)
       ("r-qtl" ,r-qtl)
       ("r-wgcna" ,r-wgcna)
       ("redis" ,redis)
       ("mariadb" ,mariadb)
       ("gemma" ,gemma-gn2)
       ("gemma-wrapper" ,gemma-wrapper)
       ; ("genenetwork2-files-small" ,genenetwork2-files-small)
       ("plink-ng-gn" ,plink-ng-gn)
       ; ("pylmm-gn2" ,pylmm-gn2)
       ("rust-qtlreaper" ,rust-qtlreaper)
       ("nginx" ,nginx)
       ("python-twint" ,python-twint)
       ("python2-coverage" ,python2-coverage)
       ("python2-flask" ,python2-flask)
       ("python2-gunicorn-gn" ,python2-gunicorn-gn)
       ("python2-pillow" ,python2-pillow) ;;  - for later!
       ("python2-pil1" ,python2-pil1-gn)
       ("python2-piddle-gn" ,python2-piddle-gn)
       ("python2-cssselect" ,python2-cssselect)
       ("python2-elasticsearch" ,python2-elasticsearch)
       ; ("python2-htmlgen" ,python2-htmlgen)
       ("python2-htmlgen-gn" ,python2-htmlgen-gn)
       ("python2-jinja2" ,python2-jinja2)
       ("python2-sqlalchemy" ,python2-sqlalchemy)
       ("python2-flask-sqlalchemy" ,python2-flask-sqlalchemy)
       ("python2-setuptools" ,python2-setuptools)
       ("python2-scipy" ,python2-scipy)
       ("python2-lxml" ,python2-lxml)
       ("python2-mechanize" ,python2-mechanize)
       ("python2-mock" ,python2-mock)
       ("python2-mysqlclient" ,python2-mysqlclient)
       ("python2-nose" ,python-nose2)
       ("python2-numarray" ,python2-numarray)
       ("python2-numpy" ,python2-numpy)
       ("python2-pandas" ,python2-pandas)
       ("python2-parallel" ,python2-parallel)
       ("python2-parameterized" ,python2-parameterized)
       ("python2-passlib" ,python2-passlib)
       ("python2-redis" ,python2-redis)
       ("python2-requests" ,python2-requests)
       ("python2-rpy2" ,python2-rpy2)
       ("python2-simplejson" ,python2-simplejson)
       ("python2-pyyaml" ,python2-pyyaml)
       ("python-unittest2" ,python-unittest2)
       ("python2-xlsxwriter" ,python2-xlsxwriter)
       ("python2-qtlreaper" ,python2-qtlreaper)
       ;; All the external js dependencies
       ("javascript-twitter-post-fetcher" ,javascript-twitter-post-fetcher)
       ("javascript-cytoscape" ,javascript-cytoscape)
       ("javascript-panzoom" ,javascript-cytoscape-panzoom)
       ("javascript-qtip" ,javascript-cytoscape-qtip)
       ("javascript-chroma" ,javascript-chroma)
       ("javascript-d3-tip" ,javascript-d3-tip)
       ("javascript-jscolor" ,javascript-jscolor)
       ("javascript-colorbox" ,javascript-colorbox)
       ("javascript-jszip" ,javascript-jszip)
       ("js-jstat" ,js-jstat)
       ("js-md5" ,js-md5)
       ("js-parsley" ,js-parsley)
       ("javascript-plotly" ,javascript-plotly)
       ("javascript-typeahead" ,javascript-typeahead)
       ("js-underscore" ,js-underscore)
       ("js-smart-time-ago" ,js-smart-time-ago)
       ("javascript-nouislider" ,javascript-nouislider)
       ("javascript-purescript-genome-browser" ,javascript-purescript-genome-browser)
       ("javascript-datatables" ,javascript-datatables)
       ("javascript-datatables-scroller" ,javascript-datatables-scroller)
       ("javascript-datatables-buttons" ,javascript-datatables-buttons)
       ("javascript-datatables-buttons-bootstrap" ,javascript-datatables-buttons-bootstrap)
       ("javascript-datatables-plugins" ,javascript-datatables-plugins)
       ("javascript-datatables-col-reorder" ,javascript-datatables-col-reorder)
       ("javascript-datatables-col-resize" ,javascript-datatables-col-resize)
       ("javascript-datatables-buttons-styles" ,javascript-datatables-buttons-styles)
       ("javascript-shapiro-wilk" ,javascript-shapiro-wilk)
       ("javascript-underscore-string" ,javascript-underscore-string)
       ("javascript-qtip2" ,javascript-qtip2)
       ("javascript-d3js" ,javascript-d3js)
       ("javascript-nvd3" ,javascript-nvd3)
       ("javascript-bootstrap" ,javascript-bootstrap)
       ("javascript-jquery" ,javascript-jquery)
       ("javascript-zxcvbn-async" ,javascript-zxcvbn-async)
       ("javascript-jquery-ui" ,javascript-jquery-ui)
       ("javascript-jquery-cookie" ,javascript-jquery-cookie)
       ))
    (inputs
     `(("javascript-colorbox" ,(package-source javascript-colorbox))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
         (modify-phases %standard-phases
           (delete 'reset-gzip-timestamps)
           (add-after 'unpack 'fix-paths-scripts
             (lambda _
               (substitute* "bin/genenetwork2"
                 (("/usr/bin/env") (which "env"))
                 (("python ") (string-append (which "python2") " "))
                 (("readlink") (which "readlink"))
                 (("dirname") (which "dirname"))
                 (("basename") (which "basename"))
                 (("cat") (which "cat"))
                 (("echo") (which "echo"))
                 (("redis-server") (which "redis-server"))
                 (("git") (which "git"))
                 (("grep") (which "grep"))
                 (("rm") (which "rm"))
                 (("which") (which "which")) ; three whiches in a row!
                 )
               #t))
           (add-after 'unpack 'patch-javascript
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((colorbox (assoc-ref inputs "javascript-colorbox"))
                     (gn2 "/share/genenetwork2/javascript/"))
                 (delete-file-recursively "wqflask/wqflask/static/packages/colorbox")
                 (copy-recursively colorbox "wqflask/wqflask/static/packages/colorbox")
                 #t)))
           (add-before 'install 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let* (
                      ; (datafiles (string-append (assoc-ref inputs "genenetwork2-files-small") "/share/genenetwork2"))
                      ; (pylmmcmd (string-append (assoc-ref inputs "pylmm-gn2") "/bin/pylmm_redis"))
                      (plink2cmd (string-append (assoc-ref inputs "plink-ng-gn") "/bin/plink2"))
                      (gemmacmd (string-append (assoc-ref inputs "gemma") "/bin/gemma"))
                      )

                 (substitute* '("etc/default_settings.py")
                   ; (("^GENENETWORK_FILES +=.*") (string-append "GENENETWORK_FILES = \"" datafiles "\"\n" ))
                   ; (("^PYLMM_COMMAND =.*") (string-append "PYLMM_COMMAND = \"" pylmmcmd "\"\n" ))
                   (("^PLINK_COMMAND =.*") (string-append "PLINK_COMMAND = \"" plink2cmd "\"\n" ))
                   (("^GEMMA_COMMAND =.*") (string-append "GEMMA_COMMAND = \"" gemmacmd "\"\n" ))
                   )
                 ))))
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://genenetwork.org/")
    (synopsis "Full genenetwork services")
    (description "Genenetwork installation sumo.")
    (license license:agpl3+))))

(define-public python3-genenetwork2
  (let ((commit "84cbf35adbb15c79638372d108308edb05f12683"))
    (package
      (inherit genenetwork2)
      (name "python3-genenetwork2")
      (version (string-append "3.11-guix-" (string-take commit 7) ))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/genenetwork/genenetwork2.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "1402g129ghfh0xwfxjj1i7gbib2yl9rahf55caj7b1psy24ys87x"))))
      (propagated-inputs
       (let ((inputs (package-propagated-inputs genenetwork2)))
         `(,@(fold
              alist-delete inputs
              (map car
                   (filter (lambda (x)
                             (let ((name (car x)))
                               (or (string-prefix? "python2" name)
                                   (string-prefix? "python-2" name)
                                   (string=? "python" name))))
                           inputs)))
           ("python" ,python-wrapper)
           ("python-pillow" ,python-pillow)
           ("python-coverage" ,python-coverage)
           ("python-flask" ,python-flask)
           ("gunicorn" ,gunicorn)
           ("python-cssselect" ,python-cssselect)
           ("python-elasticsearch" ,python-elasticsearch)
           ("python-htmlgen" ,python-htmlgen)
           ("python-jinja2" ,python-jinja2)
           ("python-sqlalchemy" ,python-sqlalchemy)
           ("python-flask-sqlalchemy" ,python-flask-sqlalchemy)
           ("python-setuptools" ,python-setuptools)
           ("python-scipy" ,python-scipy)
           ("python-lxml" ,python-lxml)
           ("python-mechanize" ,python-mechanize)
           ("python-mysqlclient" ,python-mysqlclient)
           ("python-numpy" ,python-numpy)
           ("python-pandas" ,python-pandas)
           ("python-parameterized" ,python-parameterized)
           ("python-passlib" ,python-passlib)
           ("python-redis" ,python-redis)
           ("python-requests" ,python-requests)
           ("python-simplejson" ,python-simplejson)
           ("python-pyyaml" ,python-pyyaml)
           ("python-rpy2" ,python-rpy2)
           ("python-xlsxwriter" ,python-xlsxwriter))))
      (arguments
       (let ((python (specification->package "python-wrapper"))
             (args (package-arguments genenetwork2)))
         (substitute-keyword-arguments args
           ((#:python _) python)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'unpack 'fix-paths-scripts
                 (lambda _
                   (substitute* "bin/genenetwork2"
                     (("/usr/bin/env") (which "env"))
                     (("python ") (string-append (which "python3") " "))
                     (("readlink") (which "readlink"))
                     (("dirname") (which "dirname"))
                     (("basename") (which "basename"))
                     (("cat") (which "cat"))
                     (("echo") (which "echo"))
                     (("redis-server") (which "redis-server"))
                     (("git") (which "git"))
                     (("grep") (which "grep"))
                     (("rm") (which "rm"))
                     (("which") (which "which")))
                   #t))))
           ))
       ))))

;; ./pre-inst-env guix download http://files.genenetwork.org/raw_database/db_webqtl_s.zip
;; 0sscjh0wml2lx0mb43vf4chg9gpbfi7abpjxb34n3kyny9ll557x

(define-public genenetwork2-files-small
  (let ((pfff "xx"))
    (package
    (name "genenetwork2-files-small")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri "http://files.genenetwork.org/data_files/gn2_data_s-20160303-C9E672ECED1F51B915DE419B5B2C524E.tar.lz4")
       (file-name (string-append name "-" pfff))
       (sha256
        (base32 "058ymx3af6abdhdxyxj0i9qfvb6v7j091frjpp6jh4ahks7r23lj"))))
    (build-system trivial-build-system)
    (native-inputs `(("lz4" ,lz4)
                     ("tar" ,tar)
                     ("source" ,source)))

    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((out (assoc-ref %outputs "out"))
              (name "gn2_data_s")
              (tarfn (string-append name ".tar"))
              (targetdir (string-append out "/share/genenetwork2/")))
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source"))
                   (lz4unpack (string-append (assoc-ref %build-inputs "lz4") "/bin/lz4"))
                   (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar")))
               (and
                 (zero? (system* lz4unpack source "-d" tarfn))
                 (zero? (system* tar "xf" tarfn))
                 (mkdir-p targetdir)
                 (copy-recursively name targetdir)))))))
    (home-page "http://genenetwork.org/")
    (synopsis "Small file archive to run on genenetwork")
    (description "Genenetwork genotype and mapping files.")
    (license license:agpl3+))))

(define-public genenetwork2-database-small
  (let ((md5 "93e745e9c"))
    (package
    (name "genenetwork2-database-small")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri "http://files.genenetwork.org/raw_database/db_webqtl_s.zip")
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "0sscjh0wml2lx0mb43vf4chg9gpbfi7abpjxb34n3kyny9ll557x"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))

    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
                   (and (mkdir "db")
                        (zero? (system* unzip source "-d" "db"))
                        (chdir "db"))))))
    (home-page "http://genenetwork.org/")
    (synopsis "Small database to run on genenetwork")
    (description "Genenetwork installation + database.")
    (license license:agpl3+))))



(define-public python-reaper
  (let ((commit "63391333a6619771277bfffa9bd9d33811fa0d28"))
    (package
     (name "python-reaper")
     (version (string-append "0.0.1-"
                             (string-take commit 7)))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                      (url "https://github.com/fredmanglis/reaper.git")
                      (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1rq2qn0vrqd8k676yy8drm0zxzkj065ywhxjl0j1n2r25zifay7r"))))
     (build-system python-build-system)
     (arguments
      `(#:tests? #f))
     (home-page "https://github.com/fredmanglis/reaper")
     (synopsis "Parser for .geno files")
     (description "Parser for .geno files.  It replaces the Python2 library
written in C")
     (license license:agpl3+))))

(define-public genenetwork3
  (let (;; (commit "1538ffd33af19e6ac922b4ee85fe701408968dfd")
        (commit "5bff4f49dffb4ac982d36cd0d39e0a9ec6bc66e9"))
    (package
     (name "genenetwork3")
     (version (string-append "2.10rc5-" (string-take commit 7) ))
     (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fredmanglis/genenetwork2.git")
               ;; (url "https://pjotrp@gitlab.com/genenetwork/gn2_diet.git")
               ;; (url "https://github.com/genenetwork/genenetwork2_diet.git")
               (commit commit)))
        (file-name (string-append name "-" version))
        (sha256
         (base32
          "0klgjra2qisfzs8mk0s8vzdr190l4n56xcm66dk0asqs7zswi8di"
          ;; "0ji929xgzypyhchcfy9xa1sz04w322ibs2khc8s3qiddxjqdglrz"
          ))))
     (propagated-inputs ;; propagated for development purposes
      `(;; Agnostic to Python
        ("r" ,r)
        ("git" ,git)
        ("vim" ,vim)
        ("grep" ,grep)
        ("which" ,which)
        ("r-ctl" ,r-ctl)
        ("r-qtl" ,r-qtl)
        ("redis" ,redis)
        ("mariadb" ,mariadb)
        ("nginx" ,nginx)
        ("r-wgcna" ,r-wgcna)
        ;; ("r-phewas" ,r-phewas)
        ("coreutils" ,coreutils)
        ("gemma" ,gemma-gn2-git)
        ("plink-ng-gn" ,plink-ng-gn)
        ("python-lxml" ,python-lxml) ;; used for the tests
        ("gemma-wrapper" ,gemma-wrapper)
        ("python-unittest" ,python-unittest2) ;; used for the tests
        ("python-parameterized" ,python-parameterized) ;; used for the tests
        ("genenetwork2-files-small" ,genenetwork2-files-small)
        ("javascript-twitter-post-fetcher" ,javascript-twitter-post-fetcher)
        ("javascript-cytoscape" ,javascript-cytoscape)
        ("javascript-panzoom" ,javascript-cytoscape-panzoom)
        ("javascript-qtip" ,javascript-cytoscape-qtip)
        ("javascript-chroma" ,javascript-chroma)
        ;; With Python3 support
        ("gunicorn" ,gunicorn)
        ("python-rpy2" ,python-rpy2)
        ("python-flask" ,python-flask)
        ("python-scipy" ,python-scipy)
        ("python-numpy" ,python-numpy)
        ("python-redis" ,python-redis)
        ("python-scipy" ,python-scipy)
        ("python-pillow" ,python-pillow)
        ("python-reaper" ,python-reaper)
        ("python-pyyaml" ,python-pyyaml)
        ("python-jinja2" ,python-jinja2)
        ("python-pandas" ,python-pandas)
        ("python-htmlgen" ,python-htmlgen)
        ("python-passlib" ,python-passlib)
        ("python-wrapper" ,python-wrapper)
        ("python-requests" ,python-requests)
        ("python-cssselect" ,python-cssselect)
        ("python-sqlalchemy" ,python-sqlalchemy)
        ("python-setuptools" ,python-setuptools)
        ("python-simplejson" ,python-simplejson)
        ("python-xlsxwriter" ,python-xlsxwriter)
        ("python-mysqlclient" ,python-mysqlclient)
        ("python-elasticsearch" ,python-elasticsearch)
        ("python-flask-sqlalchemy" ,python-flask-sqlalchemy)

        ;; Without Python3 support
        ;; ("python-qtlreaper" ,python-qtlreaper) ;; Run as an external program
        ;; ("pylmm-gn2" ,pylmm-gn2) ;; To be run as an external python2 program
        ;; ("python2-numarray" ,python2-numarray) ;; Update gn2 code and drop this (IMPORTANT)
        ;; ("python2-htmlgen-gn" ,python2-htmlgen-gn) ;; pjotrp and zsloan to give directions
        ))
     (build-system python-build-system)
     (arguments
      `(#:phases
        (modify-phases
          %standard-phases
          (delete 'reset-gzip-timestamps)
          (add-after
            'unpack 'fix-paths-scripts
            (lambda _
              (substitute* "bin/genenetwork2"
                (("/usr/bin/env") (which "env"))
                (("python ") (string-append (which "python") " "))
                (("readlink") (which "readlink"))
                (("dirname") (which "dirname"))
                (("basename") (which "basename"))
                (("cat") (which "cat"))
                (("echo") (which "echo"))
                (("redis-server") (which "redis-server"))
                (("git") (which "git"))
                (("grep") (which "grep"))
                (("rm") (which "rm"))
                (("which") (which "which")))
              #t))
          (add-before
            'install 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((datafiles
                       (string-append
                         (assoc-ref inputs "genenetwork2-files-small")
                         "/share/genenetwork2" ))
                     ;; (pylmmcmd
                     ;;  (string-append
                     ;;   (assoc-ref inputs "pylmm-gn2") "/bin/pylmm_redis"))
                     (plink2cmd
                       (string-append
                         (assoc-ref inputs "plink-ng-gn") "/bin/plink2"))
                     (gemmacmd
                       (string-append (assoc-ref inputs "gemma") "/bin/gemma")))

                (substitute*
                  '("etc/default_settings.py")
                  (("^GENENETWORK_FILES +=.*")
                   (string-append "GENENETWORK_FILES = \"" datafiles "\"\n" ))
                  (("^PYLMM_COMMAND =.*")
                   (string-append "PYLMM_COMMAND = \"" pylmmcmd "\"\n" ))
                  (("^PLINK_COMMAND =.*")
                   (string-append "PLINK_COMMAND = \"" plink2cmd "\"\n" ))
                  (("^GEMMA_COMMAND =.*")
                   (string-append "GEMMA_COMMAND = \"" gemmacmd "\"\n")))))))
        #:tests? #f))   ; no 'setup.py test'
     (home-page "http://genenetwork.org/")
     (synopsis "Full genenetwork services")
     (description "Genenetwork installation sumo.")
     (license license:agpl3+))))

(define-public genenetwork1
  (let ((commit "acf65ac9ae4be395c07c1629758f7408bf4eab5f") ; June 3, 2020
        (revision "2"))
    (package
      (name "genenetwork1")
      (version (git-version "0.0.0" revision commit))
      (source (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/genenetwork/genenetwork1.git")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0xmmmjyvh80yd8b0cjrwpdmxl8k9zj5ly65r2g9aygx74njsp4fi"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("ghostscript" ,ghostscript)
         ("graphviz" ,graphviz-2.26)
         ("python24" ,python-2.4)
         ("python-piddle" ,python24-piddle)
         ("wget" ,wget)))
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (add-after 'patch-generated-file-shebangs 'patch-more-files
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((piddle (assoc-ref inputs "python-piddle")))
                 (substitute* "web/webqtl/networkGraph/networkGraphUtils.py"
                   (("/usr/local/bin/neato") (which "neato"))
                   (("/usr/local/bin/circo") (which "circo"))
                   (("/usr/local/bin/twopi") (which "twopi"))
                   (("/usr/local/bin/fdp") (which "fdp"))
                   (("ps2pdf") (which "ps2pdf")))
                 (substitute* "web/webqtl/maintainance/addRif.py"
                   (("rm ") (string-append (which "rm") " "))
                   (("wget ") (string-append (which "wget") " "))
                   (("gunzip") (which "gunzip")))
                 (substitute* "web/webqtl/misc/editHtmlPage.py"
                   (("/bin/cp") (which "cp")))
                 (substitute* "web/webqtl/geneWiki/AddGeneRIFPage.py"
                   (("touch") (which "touch")))
                 (substitute* '("web/webqtl/maintainance/addRif.py"
                                "web/webqtl/networkGraph/networkGraphPage.py"
                                "web/webqtl/utility/svg.py")
                   (("/usr/bin/python") (which "python"))
                   (("/usr/bin/env python") (which "python")))
                 (substitute* "web/webqtl/base/webqtlConfigLocal.py"
                   (("PythonPath.*")
                    (string-append "PythonPath = '" (which "python") "'\n"))
                   (("PIDDLE_FONT_PATH.*/lib")
                    (string-append "PIDDLE_FONT_PATH = '" piddle "/lib"))))
               #t))
           (add-after 'patch-generated-file-shebangs 'changes-for-deployed-service
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out    (assoc-ref outputs "out")))
                 (substitute* "web/webqtl/base/webqtlConfigLocal.py"
                   ;; Where GN1 is located:
                   (("/gnshare/gn") out)
                   ;; Where the database is located:
                   (("tux01") "localhost"))
                 ;; This directory is expected to be writable
                 (symlink "/tmp" "web/tmp")
                 #t)))
           (add-after 'unpack 'use-local-links
             (lambda _
               (substitute* '("web/javascript/menu_items.js"
                              "web/webqtl/base/webqtlConfig.py"
                              "web/webqtl/maintainance/updateMenuJS.py")
                 (("http://(www|gn1).genenetwork.org") ""))

               ;; Move this file out of the way while patching files.
               (rename-file "web/infoshare/manager/MDB-Free/index.html"
                            "web/infoshare/manager/MDB-Free/index.htm")
               (substitute* (cons*
                              "web/webqtl/base/indexBody.py"
                              "web/webqtl/submitTrait/BatchSubmitPage.py"
                              (find-files "web" "\\.html"))
                 ((".*base href.*") "")
                 (("(HREF|href)=\\\"http://(www.)?genenetwork.org")
                  "href=\""))
               ;; Move this file back to its original location.
               (rename-file "web/infoshare/manager/MDB-Free/index.htm"
                            "web/infoshare/manager/MDB-Free/index.html")

               (substitute* (cons*
                              "web/humanCross.html"
                              "web/webqtl/base/indexBody.py"
                              "web/whats_new.html"
                              (find-files "web/dbdoc" "\\.html"))
                 (("src=\\\"http://www.genenetwork.org") "src=\""))
               #t))
           (add-before 'install 'replace-htaccess-file
             (lambda _
               (delete-file "web/webqtl/.htaccess")
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "." (assoc-ref outputs "out"))
               #t)))))
      (home-page "http://www.genenetwork.org/webqtl/main.py")
      (synopsis
       "Combined database and data analysis software resource for systems genetics")
      (description "GeneNetwork is a group of linked data sets and tools used to
study complex networks of genes, molecules, and higher order gene function and
phenotypes.  GeneNetwork combines more than 25 years of legacy data generated by
hundreds of scientists together with sequence data (SNPs) and massive
transcriptome data sets (expression genetic or eQTL data sets).  The
@dfn{quantitative trait locus} (QTL) mapping module that is built into GN is
optimized for fast on-line analysis of traits that are controlled by
combinations of gene
variants and environmental factors.  GeneNetwork can be used to study humans,
mice (BXD, AXB, LXS, etc.), rats (HXB), Drosophila, and plant species (barley
and Arabidopsis).  Most of these population data sets are linked with dense
genetic maps (genotypes) that can be used to locate the genetic modifiers that
cause differences in expression and phenotypes, including disease susceptibility.")
      (license license:agpl3+))))
