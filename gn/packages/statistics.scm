(define-module (gn packages statistics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages texinfo)
  #:use-module (srfi srfi-1))

(define-public r-hmisc-3
  (package
    (name "r-hmisc")
    (version "3.17-2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "Hmisc" version))
        (sha256
         (base32
          "110w5hbrl10isslqs0iq6w2ll0dafqyqznb50cdcallnlnvbvxrg"))))
    (properties `((upstream-name . "Hmisc")))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (arguments
     `(#:tests? #f))   ; no 'setup.py test'
    (propagated-inputs
     `(("r-acepack" ,r-acepack)
       ("r-cluster" ,r-cluster)
       ("r-foreign" ,r-foreign)
       ("r-formula" ,r-formula)
       ("r-ggplot2" ,r-ggplot2)
       ; ("r-grid" ,r-grid)
       ("r-gridextra" ,r-gridextra)
       ("r-gtable" ,r-gtable)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ; ("r-methods" ,r-methods)
       ("r-nnet" ,r-nnet)
       ("r-rpart" ,r-rpart)
       ("r-survival" ,r-survival)
       ))
    (home-page "http://biostat.mc.vanderbilt.edu/Hmisc")
    (synopsis "Harrell Miscellaneous")
    (description
     "Contains many functions useful for data analysis, high-level graphics, utility operations, functions for computing sample size and power, importing and annotating datasets, imputing missing values, advanced table making, variable clustering, character string manipulation, conversion of R objects to LaTeX code, and recoding variables.")
    (license license:gpl2+)))

(define-public r-graphics
  (package
    (name "r-graphics")
    (version "2.0-13")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "RGraphics" version))
        (sha256
         (base32
          "10c6wiqh074bmbg2gwdscwp5kj8afs152ipv0byyqw5n2r8fw0w1"))))
    (properties `((upstream-name . "RGraphics")))
    (build-system r-build-system)
    (propagated-inputs
     `(;; ("r-datasets" ,r-datasets)
       ("r-ggplot2" ,r-ggplot2)
       ;; ("r-graphics" ,r-graphics)
       ;; ("r-grdevices" ,r-grdevices)
       ;; ("r-grid" ,r-grid)
       ("r-lattice" ,r-lattice)
       ;; ("r-methods" ,r-methods)
       ;; ("r-stats" ,r-stats)
       ))
    (home-page "http://www.stat.auckland.ac.nz/~paul/RG2e/index.html")
    (synopsis
     "Data and Functions from the Book R Graphics, Second Edition")
    (description
     "Data and Functions from the book R Graphics, Second Edition.  There is a function to produce each figure in the book, plus several functions, classes, and methods defined in Chapter 8.")
    (license license:gpl2+)))

(define-public pylmm-gn2
  (let ((commit "3c6d1cac8"))
    (package
      (name "pylmm-gn2")
      (version (string-append "1.0-" commit ))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/genenetwork/pylmm.git")
                       (commit commit)))
                (file-name (string-append name "-" commit))
                (sha256
                 (base32
                  "0wryaadb36i275p9d2i1kzflahvbl9kj5wlk8jlbvjij8gpqg964"))))
      (build-system python-build-system)
      (inputs `(
                ("python2-setuptools" ,python2-setuptools)
                ("python2-scipy" ,python2-scipy)
                ("python2-numpy" ,python2-numpy)
                ))
      (arguments
       `(#:python ,python-2
         #:tests? #f        ; no 'setup.py test'
         #:phases
         (modify-phases %standard-phases
           (add-before
             'build 'change-paths
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "scripts/pylmm_redis"
                   (("/usr/bin/python") (which "python"))
                   (("\\$PACKAGEDIR") (string-append out "/lib/python2.7/site-packages")))
                 ))))))

      (home-page "http://genenetwork.org/")
      (synopsis "LMM resolver")
      (description "Fast and lightweight linear mixed-model (LMM) solver
for use in genome-wide association studies (GWAS).")
      (license license:agpl3+))))

(define-public r-ctl ; guix: ready
  (package
    (name "r-ctl")
    (version "1.0.0-4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cran/src/contrib/ctl_"
                            version ".tar.gz"))
        (sha256
         (base32
          "027cij4vdsq3xhi38izzn07q6xwja7n8v95l7kk1pp92y9qkvqv3"))))
    (build-system r-build-system)
    (inputs `(
              ("r-qtl" ,r-qtl)
              ("r-mass" ,r-mass)
              ))
    (home-page "https://github.com/DannyArends/CTLmapping")
    (synopsis "R package for analysis of genetical genomic data to identify genetic loci associated with correlation changes in quantitative traits (CTL)")
    (description "Analysis of experimental crosses to identify genetic
markers associated with correlation changes in quantitative
traits (CTL).  The additional correlation information obtained can be
combined with QTL information to perform de novo reconstruction of
interaction networks.")
    (license license:gpl3)))

;; r-with-tests is private so we inherit from r-minimal.
(define r-with-tests-2
  (package
    (inherit r-minimal)
    (name "r-with-tests")
    (version "2.15.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cran/src/base"
                            "/R-" (version-major version)
                            "/R-" version ".tar.gz"))
        (sha256
         (base32
          "1mjmq95s5nrwppbzic6lzanjq65j3sxg85l1q09c0fxdin7s70y5"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file "NEWS.pdf")
            #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments r-minimal)
       ((#:tests? _ #t) #t)
       ((#:configure-flags cf)
        `(cons* "--with-system-zlib"
                "--with-system-bzlib"
                "--with-system-pcre"
                (delete "--without-recommended-packages" ,cf)))
       ((#:phases phases)
        `(modify-phases ,phases
           ;; We can only use some of 'build-reproducibly with this older version.
           (replace 'build-reproducibly
             (lambda _
               ;; The documentation contains time stamps to demonstrate
               ;; documentation generation in different phases.
               (substitute* "src/library/tools/man/Rd2HTML.Rd"
                 (("\\\\%Y-\\\\%m-\\\\%d at \\\\%H:\\\\%M:\\\\%S")
                  "(removed for reproducibility)"))

               ;; Remove timestamp from tracing environment.  This fixes
               ;; reproducibility of "methods.rd{b,x}".
               (substitute* "src/library/methods/R/trace.R"
                 (("dateCreated = Sys.time\\(\\)")
                  "dateCreated = as.POSIXct(\"1970-1-1 00:00:00\", tz = \"UTC\")"))

               ;; Ensure that gzipped files are reproducible.
               (substitute* '("src/library/grDevices/Makefile.in"
                              "doc/manual/Makefile.in")
                 (("R_GZIPCMD\\)" line)
                  (string-append line " -n")))

               ;; The "srcfile" procedure in "src/library/base/R/srcfile.R"
               ;; queries the mtime of a given file and records it in an object.
               ;; This is acceptable at runtime to detect stale source files,
               ;; but it destroys reproducibility at build time.
 
               ;; Similarly, the "srcfilecopy" procedure records the current
               ;; time.  We change both of them to respect SOURCE_DATE_EPOCH.
;               (substitute* "src/library/base/R/srcfile.R"
;                 (("timestamp <- (timestamp.*|file.mtime.*)" _ time)
;                  (string-append "timestamp <- \
;as.POSIXct(if (\"\" != Sys.getenv(\"SOURCE_DATE_EPOCH\")) {\
;  as.numeric(Sys.getenv(\"SOURCE_DATE_EPOCH\"))\
;} else { " time "}, origin=\"1970-01-01\")\n")))

               ;; This library is installed using "install_package_description",
               ;; so we need to pass the "builtStamp" argument.
               ;(substitute* "src/library/tools/Makefile.in"
               ;  (("(install_package_description\\(.*\"')\\)\"" line prefix)
               ;   (string-append prefix ", builtStamp='1970-01-01')\"")))

               (substitute* "src/library/Recommended/Makefile.in"
                 (("INSTALL_OPTS =" m)
                  (string-append m " --built-timestamp=1970-01-01" m)))

               ;; R bundles an older version of help2man, which does not respect
               ;; SOURCE_DATE_EPOCH.  We cannot just use the latest help2man,
               ;; because that breaks a test.
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* "tools/help2man.pl"
                   (("my \\$date = strftime \"%B %Y\", localtime" line)
                    (string-append line " 1"))))
               #t))))))
    (native-inputs
     `(("texinfo" ,texinfo-4)
       ,@(alist-delete "texinfo" (package-native-inputs r-minimal))))
    (properties '((release-date "2013-03-01")))))

(define-public r-minimal-2
  (package
    (inherit r-with-tests-2)
    (name "r-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments r-with-tests-2)
       ((#:tests? _ #f) #f)
       ((#:configure-flags flags)
        ;; Do not build the recommended packages.  The build system creates
        ;; random temporary directories and embeds their names in some
        ;; package files.  We build these packages with the r-build-system
        ;; instead.
        `(cons* "--without-recommended-packages" ,flags))))))

(define-public r-2-lattice
  (package
    (inherit r-lattice)
    (name "r-lattice")
    (version "0.20-31")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "lattice" version))
        (sha256
         (base32
          "1b3m3rg1zd8ssk5jjswk5y93js89vh6939kfajh6i6wphndxigb1"))))
    (build-system r-build-system)
    (arguments
     `(#:r ,r-minimal-2))))

(define-public r-2-matrix
  (package
    (inherit r-matrix)
    (name "r-matrix")
    (version "1.2-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Matrix" version))
       (sha256
        (base32
         "0ywz213p6cpwnklxd81hzdyxjzagaj6cn32ycc5rcnhxy30d7kk5"))))
    (arguments
     `(#:r ,r-minimal-2))
    (propagated-inputs
     `(("r-lattice" ,r-2-lattice)))))

(define-public r-2-survival
  (package
    (inherit r-survival)
    (name "r-survival")
    (version "2.41-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "survival" version))
       (sha256
        (base32
         "07cnr0hnki6ybbjll54l4s5lllhk19vni5f8m0mvsfp99ls7qygk"))))
    (arguments
     `(#:r ,r-minimal-2))
    (propagated-inputs
     `(("r-matrix" ,r-2-matrix)))))
