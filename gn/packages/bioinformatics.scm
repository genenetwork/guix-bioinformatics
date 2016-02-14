;; Bioinformatics module

(define-module (gn packages bioinformatics)
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
  #:use-module (gnu packages databases)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
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
  #:use-module (gn packages python)
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

(define-public r-wgcna
(package
  (name "r-wgcna")
  (version "1.48")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "WGCNA" version))
      (sha256
        (base32
          "18yl2v3s279saq318vd5hlwnqfm89rxmjjji778d2d26vviaf6bn"))))
  (properties `((upstream-name . "WGCNA")))
  (build-system r-build-system)
  ;; (propagated-inputs
    ;; `( ;; ("r-annotationdbi" ,r-annotationdbi)
       ;; ("r-doparallel" ,r-doparallel)
       ;; ("r-dynamictreecut" ,r-dynamictreecut)
       ;; ("r-fastcluster" ,r-fastcluster)
       ;; ("r-foreach" ,r-foreach)
       ;; ("r-go.db" ,r-go.db)
       ;; ("r-grdevices" ,r-grdevices)
       ;; ("r-hmisc" ,r-hmisc)
       ;; ("r-impute" ,r-impute)
       ;; ("r-matrixstats" ,r-matrixstats)
       ;; ("r-parallel" ,r-parallel)
       ;; ("r-preprocesscore" ,r-preprocesscore)
       ;; ("r-splines" ,r-splines)
       ;; ("r-stats" ,r-stats)
       ;; ("r-survival" ,r-survival)
       ;; ("r-utils" ,r-utils)))
  (home-page
    "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
  (synopsis
    "Weighted Correlation Network Analysis")
  (description
    "Functions necessary to perform Weighted Correlation Network Analysis on high-dimensional data.  Includes functions for rudimentary data cleaning, construction of correlation networks, module identification, summarization, and relating of variables and modules to sample traits.  Also includes a number of utility functions for data manipulation and visualization.")
  (license license:gpl2+)))

(define-public qtlreaper
  (package
    (name "qtlreaper")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/qtlreaper/qtlreaper-" version ".tar.gz"
             ;; "http://downloads.sourceforge.net/project/qtlreaper/qtlreaper/1.1.1/qtlreaper-1.1.1.tar.gz?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fqtlreaper%2Ffiles%2Flatest%2Fdownload&ts=1358975786&use_mirror=iweb"))
             ))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rbf030940nbbbkggdq2dxiy3c0jv8l4y3vvyfxhqimgj0qv3l1x"))))
    (build-system python-build-system)
    ;; (native-inputs
    ;; `(("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://qtlreaper.sourceforge.net/")
    (synopsis "Tool for scanning expression data for QTLs")
    (description
     "It is essentially the batch-oriented version of WebQTL. It
requires, as input, expression data from members of a set of
recombinant inbred lines and genotype information for the same
lines.  It searches for an association between each expression trait
and all genotypes and evaluates that association by a permutation
test.  For the permutation test, it performs only as many permutations
as are necessary to define the empirical P-value to a reasonable
precision. It also performs bootstrap resampling to estimate the
confidence region for the location of a putative QTL.")
    (license license:gpl2)))

(define-public plink2
  (package
    (name "plink2")
    (version "1.90b3")
    (source
     (origin
      (method url-fetch)
      ;; https://github.com/chrchang/plink-ng/archive/v1.90b3.tar.gz
       (uri (string-append
             "https://github.com/chrchang/plink-ng/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "03fzib1al5qkr9vxv63wxmv6y2pfb1rmir0h8jpi72r87hczqjig"))
       (patches (list (search-patch "plink-ng-Makefile-zlib.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (zero? (system* "make" "-f" "Makefile.std"))
                   ))                 
        (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink2" bin)
                      #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("openblas" ,openblas)
       ("atlas" ,atlas)
       ("lapack" ,lapack)
       ("gfortran" ,gfortran)
       ))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.cog-genomics.org/plink2")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    ;; Code is released under GPLv2, except for fisher.h, which is under
    ;; LGPLv2.1+
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public plink-ng
  (let ((commit "516d730f9"))
  (package
    (name "plink-ng")
    (version (string-append "1.90b3-" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/chrchang/plink-ng.git")
                   (commit commit)))
             (file-name (string-append name "-" commit)) 
             (sha256
              (base32
               "0cv824wkdml9h9imsc30s2x3l8g65j44cpjbr1ydkk49g5qmf580"))
    (patches (list (search-patch "plink-ng-Makefile-zlib-git.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (zero? (system* "make" "-f" "Makefile.std"))
                   ))                 
        (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink2" bin)
                      #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("openblas" ,openblas)
       ("atlas" ,atlas)
       ("lapack" ,lapack)
       ("gfortran" ,gfortran)
       ))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.cog-genomics.org/plink2")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    (license license:gpl3+))))

(define-public gemma-git
  (let ((commit "2de4bfab3"))
  (package
    (name "gemma-git")
    (version (string-append "0.9.5-" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/GEMMA.git")
                   (commit commit)))
             (file-name (string-append name "-" commit)) 
             (sha256
              (base32
               "1drffdgwbzgiw9sf55ghl3zjv58f8i9kfz0zys5mp6n06syp4ira"))))
    (inputs `(
              ("gsl" ,gsl)
              ("lapack" ,lapack)
              ("zlib" ,zlib)
              ))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '(" FORCE_DYNAMIC=1")
       #:phases
        (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'bin-mkdir
                     (lambda _
                       (mkdir-p "bin")
                       ))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                           (let ((out (assoc-ref outputs "out")))
                             (install-file "bin/gemma" (string-append out "/bin"))))))
       #:tests? #f))
    (home-page "")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "GEMMA is the software implementing the Genome-wide
Efficient Mixed Model Association algorithm for a standard linear
mixed model and some of its close relatives for genome-wide
association studies (GWAS).")
    (license license:gpl3))))


(define-public genenetwork1
  (let ((commit "d622c803b"))
  (package
    (name "genenetwork1")
    (version (string-append "1.0-" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/genenetwork.git")
                   ;; (url "https://github.com/pjotrp/genenetwork.git")
                   (commit commit)))
             (file-name (string-append name "-" commit)) 
             (sha256
              (base32
               "14fzfcm4vl20mlhxjslfa01i1nmxpk8lbxmfvpq6dyfc22ir62py"))))
    (propagated-inputs `(
              ("python" ,python-2) ;; probably superfluous
              ("r" ,r) 
    ))
    (inputs `(
              ;; http://spring211.uthsc.edu/gn/thirdparty.tbz
              ;; graphviz-2.22.2  htmlgen  json  numarray-1.5.2  piddle  PIL  pp-1.5.7  pyx  pyXLWriter  svg
              ("mysql" ,mysql)
              ("nginx" ,nginx)
              ("graphviz" ,graphviz)
              ; ("python2-jinja2" ,python2-jinja2)
              ; ("python2-sqlalchemy" ,python2-sqlalchemy)
              ; ("python2-setuptools" ,python2-setuptools)
              ; ("python2-scipy" ,python2-scipy)
              ;; looks like python-numarray is not needed
              ; ("python2-numpy" ,python2-numpy)
              ; ("python2-pandas" ,python2-pandas)
              ; ("python2-passlib" ,python2-passlib)
              ; ("python2-redis" ,python2-redis)
              ; ("python2-requests" ,python2-requests)
              ; ("python2-simplejson" ,python2-simplejson)
              ; ("python2-pyyaml" ,python2-pyyaml)
              ;; python-yolk is not needed
              ("python2-pil" ,python2-pil)
              ("python2-numarray" ,python2-numarray)
              ("plink" ,plink) ;; gn1
              ; ("r-qtl" ,r-qtl)
              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://genenetwork.org/")
    (synopsis "Full genenetwork services")
    (description "Genenetwork installation sumo.")
    (license license:agpl3+))))

(define-public genenetwork2
  (let ((commit "9e9475053"))
  (package
    (name "genenetwork2")
    (version (string-append "2.0-" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://github.com/pjotrp/genenetwork2.git")
                   (commit commit)))
             (file-name (string-append name "-" commit)) 
             (sha256
              (base32
               "09hvy9mf4dnmkb8qg49viffzrxk53m2kr4r955m84dxaa5pdrjhd"))))
    (propagated-inputs `(
              ("python" ,python-2) ;; probably superfluous
              ("r" ,r) 
    ))
    (inputs `(
              ("mysql" ,mysql)
              ("gemma" ,gemma-git)
              ("plink2" ,plink-ng)
              ("nginx" ,nginx)
              ("python2-flask" ,python2-flask)
              ("python2-htmlgen-gn" ,python2-htmlgen-gn)
              ("python2-jinja2" ,python2-jinja2)
              ("python2-sqlalchemy" ,python2-sqlalchemy)
              ("python2-flask-sqlalchemy" ,python2-flask-sqlalchemy)
              ("python2-setuptools" ,python2-setuptools)
              ("python2-scipy" ,python2-scipy)
              ;; looks like python-numarray is not needed
              ("python2-mysqlclient" ,python2-mysqlclient)
              ("python2-numarray" ,python2-numarray)
              ("python2-numpy" ,python2-numpy)
              ("python2-pandas" ,python2-pandas)
              ("python2-parallel" ,python2-parallel)
              ("python2-passlib" ,python2-passlib)
              ("python2-piddle" ,python2-piddle)
              ("python2-redis" ,python2-redis)
              ("python2-requests" ,python2-requests)
              ("python2-rpy2" ,python2-rpy2)
              ("python2-scipy" ,python2-scipy)
              ("python2-simplejson" ,python2-simplejson)
              ("python2-pyyaml" ,python2-pyyaml)
              ("python-xlsxwriter" ,python-xlsxwriter)
              ;; python-yolk is not needed
              ("plink" ,plink) 
              ("qtlreaper" ,qtlreaper) 
              ("r-qtl" ,r-qtl)
              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://genenetwork.org/")
    (synopsis "Full genenetwork services")
    (description "Genenetwork installation sumo.")
    (license license:agpl3+))))
