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
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics))

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
