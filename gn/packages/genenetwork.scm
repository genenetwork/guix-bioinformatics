;; Bioinformatics module

(define-module (gn packages genenetwork)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  ;; #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cran)
  #:use-module (gn packages crates-io)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages cpio)
  #:use-module (gn packages elixir)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages version-control)
  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages gemma)
  #:use-module (gn packages javascript)
  #:use-module (gn packages phewas)
  #:use-module (gn packages python)
  #:use-module (gn packages python24)
  #:use-module (gn packages statistics)
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
           ;; bit by providing a symlink it this package's output.
           (symlink bash (string-append out "/bash"))
           (setenv "PATH" out)
           (wrap-program foo `("GUIX_FOO" prefix ("hello")))
           (wrap-program foo `("GUIX_BAR" prefix ("world")))
           #t))))
    (inputs `(("bash" ,(search-bootstrap-binary "bash"
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
       ("mysql" ,mysql)
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
  (let ((commit "dd9c7fb2a9d5fa40b4054e1bcb7c57905d98d5f8"))
  (package
    (name "python2-qtlreaper")
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

(define-public python-qtlreaper
  (let ((commit "dd9c7fb2a9d5fa40b4054e1bcb7c57905d98d5f8"))
  (package
    (name "python-qtlreaper")
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
     `(
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'fix-tests
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* '("./test/example1.py" "./test/example2.py")
              (("/usr/bin/python") (which "python3"))))))
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

(define-public rust-qtlreaper
  (let ((commit "eacf6ff1c3d1cd16084a70a7a425bd77080c15de")
        (revision "1"))
    (package
      (name "rust-qtlreaper")
      (version (git-version "0.1.3" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/chfi/rust-qtlreaper.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0gr2z54i11zz94ra4w06fhfnwnmmhl5xyc8qhlk0v2qq18yfi7ji"))))
      (build-system cargo-build-system)
      (arguments
       `(#:cargo-inputs
         (("rust-rand" ,rust-rand-0.6)
          ("rust-structopt" ,rust-structopt)
          ("rust-rayon" ,rust-rayon-1.0)
          ("rust-serde" ,rust-serde)
          ("rust-serde-json" ,rust-serde-json))
         #:phases
         (modify-phases %standard-phases
           (add-after 'configure 'regenerate-cargo-lock
             (lambda _
               (delete-file "Cargo.lock")
               (invoke "cargo" "generate-lockfile")))
           (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
             (lambda _
               (use-modules (guix build cargo-utils))
                 (for-each
                   (lambda (filename)
                     (use-modules (guix build cargo-utils))
                     (delete-file filename)
                     (let* ((dir (dirname filename)))
                       (display (string-append
                                  "patch-cargo-checksums: generate-checksums for "
                                  dir "\n"))
                       (generate-checksums dir)))
                   (find-files "guix-vendor" ".cargo-checksum.json"))
                 #t)))))
      (home-page "https://github.com/chfi/rust-qtlreaper")
      (synopsis "Reimplementation of genenetwork/QTLReaper in Rust")
      (description "Reimplementation of genenetwork/QTLReaper in Rust")
      (license #f))))

(define-public genenetwork2
  (let ((commit "1538ffd33af19e6ac922b4ee85fe701408968dfd"))
  (package
    (name "genenetwork2")
    (version (string-append "2.11-guix-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://pjotrp@gitlab.com/genenetwork/gn2_diet.git")
                   ;; (url "https://github.com/genenetwork/genenetwork2_diet.git")
                   (commit commit)))
             (file-name (string-append name "-" version))
             (sha256
              (base32
               "0ji929xgzypyhchcfy9xa1sz04w322ibs2khc8s3qiddxjqdglrz"))))
    (propagated-inputs `(  ;; propagated for development purposes
              ("python" ,python-2) ;; probably superfluous
              ("coreutils" ,coreutils)
              ("git" ,git)
              ("vim" ,vim)
              ("which" ,which)
              ("grep" ,grep)
              ("r" ,r)
              ("r-ctl" ,r-ctl)
              ("r-phewas" ,r-phewas)
              ("r-qtl" ,r-qtl)
              ("r-wgcna" ,r-wgcna)
              ("redis" ,redis)
              ("mysql" ,mysql)
              ("gemma" ,gemma-gn2-git)
              ("gemma-wrapper" ,gemma-wrapper)
              ("genenetwork2-files-small" ,genenetwork2-files-small)
              ("plink-ng-gn" ,plink-ng-gn)
              ("pylmm-gn2" ,pylmm-gn2)
              ("nginx" ,nginx)
              ("python2-flask" ,python2-flask)
              ("gunicorn" ,gunicorn)
	      ("python2-pillow" ,python2-pillow)
              ("python2-cssselect" ,python2-cssselect)
              ("python2-elasticsearch" ,python2-elasticsearch)
              ("python2-htmlgen" ,python2-htmlgen)
              ("python2-jinja2" ,python2-jinja2)
              ("python2-sqlalchemy" ,python2-sqlalchemy)
              ("python2-flask-sqlalchemy" ,python2-flask-sqlalchemy)
              ("python2-setuptools" ,python2-setuptools)
              ("python2-scipy" ,python2-scipy)
              ("python2-lxml" ,python2-lxml)
              ("python2-mechanize" ,python2-mechanize)
              ("python2-mysqlclient" ,python2-mysqlclient)
              ("python2-numarray" ,python2-numarray)
              ("python2-numpy" ,python2-numpy)
              ("python2-pandas" ,python2-pandas)
              ("python2-parallel" ,python2-parallel)
              ("python2-parameterized" ,python2-parameterized)
              ("python2-passlib" ,python2-passlib)
              ("python2-redis" ,python2-redis)
              ("python2-requests" ,python2-requests)
              ("python2-requests" ,python2-requests)
              ("python2-rpy2" ,python2-rpy2)
              ("python2-scipy" ,python2-scipy)
              ("python2-simplejson" ,python2-simplejson)
              ("python2-pyyaml" ,python2-pyyaml)
              ("python2-unittest2" ,python2-unittest2)
              ("python2-xlsxwriter" ,python2-xlsxwriter)
              ("python2-qtlreaper" ,python2-qtlreaper)
	            ("javascript-twitter-post-fetcher" ,javascript-twitter-post-fetcher)
	            ("javascript-cytoscape" ,javascript-cytoscape)
	            ("javascript-panzoom" ,javascript-cytoscape-panzoom)
	            ("javascript-qtip" ,javascript-cytoscape-qtip)

              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
         (modify-phases %standard-phases
           (delete 'reset-gzip-timestamps)
           (add-after 'unpack 'fix-paths-scripts
             (lambda* _
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
                            (("which") (which "which")) ; three wiches in a row!
                            )#t))
           (add-before 'install 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
                      (let* (
                             (datafiles (string-append (assoc-ref inputs "genenetwork2-files-small") "/share/genenetwork2" ))
                             (pylmmcmd (string-append (assoc-ref inputs "pylmm-gn2") "/bin/pylmm_redis"))
                             (plink2cmd (string-append (assoc-ref inputs "plink-ng-gn") "/bin/plink2"))
                             (gemmacmd (string-append (assoc-ref inputs "gemma") "/bin/gemma"))
                             )

               (substitute* '("etc/default_settings.py")
                            (("^GENENETWORK_FILES +=.*") (string-append "GENENETWORK_FILES = \"" datafiles "\"\n" ))
                            (("^PYLMM_COMMAND =.*") (string-append "PYLMM_COMMAND = \"" pylmmcmd "\"\n" ))
                            (("^PLINK_COMMAND =.*") (string-append "PLINK_COMMAND = \"" plink2cmd "\"\n" ))
                            (("^GEMMA_COMMAND =.*") (string-append "GEMMA_COMMAND = \"" gemmacmd "\"\n" ))
                            )
               ))))
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://genenetwork.org/")
    (synopsis "Full genenetwork services")
    (description "Genenetwork installation sumo.")
    (license license:agpl3+))))

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
              (targetdir (string-append out "/share/genenetwork2/"))
              )
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source"))
                   (lz4unpack (string-append (assoc-ref %build-inputs "lz4") "/bin/lz4"))
                   (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                   )
               (and
                    (zero? (system* lz4unpack source "-d" tarfn))
                    (zero? (system* tar "xf" tarfn))
                    (mkdir-p targetdir)
                    (copy-recursively name targetdir)
                    ))))))
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
                         (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
                         )
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
      `( ;; Agnostic to Python
	("r" ,r)
	("git" ,git)
	("vim" ,vim)
	("grep" ,grep)
	("which" ,which)
	("r-ctl" ,r-ctl)
	("r-qtl" ,r-qtl)
	("redis" ,redis)
	("mysql" ,mysql)
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
	  (lambda* _
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
