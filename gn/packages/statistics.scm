(define-module (gn packages statistics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1))

(define-public r-acepack
(package
  (name "r-acepack")
  (version "1.3-3.3")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "acepack" version))
      (sha256
        (base32
          "13ry3vyys12iplb14jfhmkrl9g5fxg3iijiggq4s4zb5m5436b1y"))))
  (build-system r-build-system)
  (inputs
   `(("gfortran" ,gfortran)))
  (home-page
    "http://cran.r-project.org/web/packages/acepack")
  (synopsis
    "ace() and avas() for selecting regression transformations")
  (description
    "ACE and AVAS methods for choosing regression transformations.")
  (license license:x11)))

(define-public r-latticeextra
(package
  (name "r-latticeextra")
  (version "0.6-28")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "latticeExtra" version))
      (sha256
        (base32
          "1hkyqsa7klk5glj9y1hg3rxr5qilqw8h0017zc4c3nps7lr9a1kq"))))
  (properties `((upstream-name . "latticeExtra")))
  (build-system r-build-system)
  (propagated-inputs
    `( ;;("r-grdevices" ,r-grdevices)
      ("r-gridbase" ,r-gridbase)
      ("r-lattice" ,r-lattice)
      ("r-rcolorbrewer" ,r-rcolorbrewer)
      ))
  (home-page
    "http://latticeextra.r-forge.r-project.org/")
  (synopsis
    "Extra Graphical Utilities Based on Lattice")
  (description
    "Building on the infrastructure provided by the lattice package, this package provides several new high-level functions and methods, as well as additional utilities such as panel and axis annotation functions.")
  (license license:gpl2+)))

(define-public r-formula
  (package
  (name "r-formula")
  (version "1.2-1")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "Formula" version))
      (sha256
        (base32
          "02in5325zzrqbhlygx6s0dinj6ymw845q70y56frqacv25ayzcax"))))
  (properties `((upstream-name . "Formula")))
  (build-system r-build-system)
  (home-page
    "http://cran.r-project.org/web/packages/Formula")
  (synopsis "Extended Model Formulas")
  (description
    "Infrastructure for extended formulas with multiple parts on the right-hand side and/or multiple responses on the left-hand side.")
  (license #f))
)

(define-public r-hmisc
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
      ; ("r-cluster" ,r-cluster)
      ; ("r-foreign" ,r-foreign)
      ("r-formula" ,r-formula)
      ("r-ggplot2" ,r-ggplot2)
      ; ("r-grid" ,r-grid)
      ("r-gridextra" ,r-gridextra)
      ("r-gtable" ,r-gtable)
      ("r-lattice" ,r-lattice)
      ("r-latticeextra" ,r-latticeextra)
      ; ("r-methods" ,r-methods)
      ; ("r-nnet" ,r-nnet)
      ; ("r-rpart" ,r-rpart)
                                        ; ("r-survival" ,r-survival)))
      ))
  (home-page
    "http://biostat.mc.vanderbilt.edu/Hmisc")
  (synopsis "Harrell Miscellaneous")
  (description
    "Contains many functions useful for data analysis, high-level graphics, utility operations, functions for computing sample size and power, importing and annotating datasets, imputing missing values, advanced table making, variable clustering, character string manipulation, conversion of R objects to LaTeX code, and recoding variables.")
  (license license:gpl2+)))

(define-public r-iterators
(package
  (name "r-iterators")
  (version "1.0.8")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "iterators" version))
      (sha256
        (base32
          "1f057pabs7ss9h1n244can26qsi5n2k3salrdk0b0vkphlrs4kmf"))))
  (build-system r-build-system)
  ;; (propagated-inputs `(("r-utils" ,r-utils)))
  (home-page
    "http://cran.r-project.org/web/packages/iterators")
  (synopsis "Provides Iterator Construct for R")
  (description
    "Support for iterators, which allow a programmer to traverse through all the elements of a vector, list, or other collection of data.")
  (license #f)))

(define-public r-fastcluster
(package
  (name "r-fastcluster")
  (version "1.1.16")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "fastcluster" version))
      (sha256
        (base32
          "0x2prrsnqi5iqx23ki6y2agndjq8058ph6s703i4avrqi1q1w1q8"))))
  (build-system r-build-system)
  (home-page
    "http://danifold.net/fastcluster.html")
  (synopsis
    "Fast Hierarchical Clustering Routines for R and Python")
  (description
    "This is a two-in-one package which provides interfaces to both R and Python.  It implements fast hierarchical, agglomerative clustering routines.  Part of the functionality is designed as drop-in replacement for existing routines: \"linkage\" in the SciPy package \"scipy.cluster.hierarchy\", \"hclust\" in R's \"stats\" package, and the \"flashClust\" package.  It provides the same functionality with the benefit of a much faster implementation.  Moreover, there are memory-saving routines for clustering of vector data, which go beyond what the existing packages provide.  For information on how to install the Python files, see the file INSTALL in the source distribution.")
  (license #f)))

(define-public r-dynamictreecut
(package
  (name "r-dynamictreecut")
  (version "1.62")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "dynamicTreeCut" version))
      (sha256
        (base32
          "1y11gg6k32wpsyb10kdv176ivczx2jlizs1xsrjrs6iwbncwzrkp"))))
  (properties
    `((upstream-name . "dynamicTreeCut")))
  (build-system r-build-system)
  ; (propagated-inputs `(("r-stats" ,r-stats)))
  (home-page
    "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/BranchCutting/")
  (synopsis
    "Methods for detection of clusters in hierarchical clustering dendrograms.")
  (description
    "Contains methods for detection of clusters in hierarchical clustering dendrograms.")
  (license license:gpl2+)))

(define-public r-rcppeigen
(package
  (name "r-rcppeigen")
  (version "0.3.2.5.1")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "RcppEigen" version))
      (sha256
        (base32
          "1j41kyr2xsq0ha3dhd0iz62kghkvhnf8zp15qb4kgj6www086b4s"))))
  (properties `((upstream-name . "RcppEigen")))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-matrix" ,r-matrix)
      ("r-rcpp" ,r-rcpp)
      ;; ("r-stats" ,r-stats)
      ;; ("r-utils" ,r-utils)
      ))
  (home-page "http://eigen.tuxfamily.org")
  (synopsis
    "'Rcpp' Integration for the 'Eigen' Templated Linear Algebra Library")
  (description
    "R and 'Eigen' integration using 'Rcpp'. 'Eigen' is a C++ template library for linear algebra: matrices, vectors, numerical solvers and related algorithms.  It supports dense and sparse matrices on integer, floating point and complex numbers, decompositions of such matrices, and solutions of linear systems.  Its performance on many algorithms is comparable with some of the best implementations based on 'Lapack' and level-3 'BLAS'. .  The 'RcppEigen' package includes the header files from the 'Eigen' C++ template library (currently version 3.2.5).  Thus users do not need to install 'Eigen' itself in order to use 'RcppEigen'. .  Since version 3.1.1, 'Eigen' is licensed under the Mozilla Public License (version 2); earlier version were licensed under the GNU LGPL version 3 or later. 'RcppEigen' (the 'Rcpp' bindings/bridge to 'Eigen') is licensed under the GNU GPL version 2 or later, as is the rest of 'Rcpp'.")
  (license #f)))


(define-public r-matrix
(package
  (name "r-matrix")
  (version "1.2-3")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "Matrix" version))
      (sha256
        (base32
          "11zi02hj083jh20lnxsiimnx4brksavbv7dmkp659w33cfzsnnwg"))))
  (properties `((upstream-name . "Matrix")))
  (build-system r-build-system)
  (propagated-inputs
    `(("r-graphics" ,r-graphics)
      ;; ("r-grid" ,r-grid)
      ("r-lattice" ,r-lattice)
      ;; ("r-methods" ,r-methods)
      ;; ("r-stats" ,r-stats)
      ;;("r-utils" ,r-utils)
      ))
  (home-page
    "http://Matrix.R-forge.R-project.org/")
  (synopsis
    "Sparse and Dense Matrix Classes and Methods")
  (description
    "Classes and methods for dense and sparse matrices and operations on them using 'LAPACK' and 'SuiteSparse'.")
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
  (home-page
    "http://www.stat.auckland.ac.nz/~paul/RG2e/index.html")
  (synopsis
    "Data and Functions from the Book R Graphics, Second Edition")
  (description
    "Data and Functions from the book R Graphics, Second Edition.  There is a function to produce each figure in the book, plus several functions, classes, and methods defined in Chapter 8.")
  (license license:gpl2+)))

(define-public r-lmmlite
  (package
    (name "r-lmmlite")
    (version "0.1-9")
    (source (origin
              ;; We use the git reference, because there's no CRAN package (yet)
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kbroman/lmmlite.git")
                    (commit "5b833d5")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0v4z4qxa8ki9hlmdwlgslchvg21nqkkq6135nx6w63xikjffxcba"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcppeigen" ,r-rcppeigen)))
    (synopsis "R/lmmlite")
    (description
     "R/lmmlite")
    (home-page "https://github.com/kbroman/")
    (license license:asl2.0)))

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
  (version "1.0.0-0")
  (source
   (origin
    (method url-fetch)
    (uri (string-append "mirror://cran/src/contrib/ctl_"
                        version ".tar.gz"))
    (sha256
     (base32
      "12hrrql9wz43s1d3sfyzlqzx7ajrl3hvf96776ik6icbm8by8h6j"))))
  (build-system r-build-system)
  (inputs `(
            ("r-qtl" ,r-qtl)
            ))
  (home-page "https://github.com/DannyArends/CTLmapping")
  (synopsis "R package for analysis of genetical genomic data to identify genetic loci associated with correlation changes in quantitative traits (CTL)")
  (description "Analysis of experimental crosses to identify genetic
markers associated with correlation changes in quantitative
traits (CTL).  The additional correlation information obtained can be
combined with QTL information to perform de novo reconstruction of
interaction networks.")
  (license license:gpl3)))
