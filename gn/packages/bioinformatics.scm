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
  ;; #:use-module (guix build-system ruby)
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
  #:use-module (gnu packages ldc)
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
  #:use-module (gn packages statistics)
  #:use-module (srfi srfi-1))


(define-public r-biocpreprocesscore
  (package
    (name "r-biocpreprocesscore")
    (version "1.32.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "preprocessCore" version))
              (sha256
               (base32
                "07isghjkqm91rg37l1fzpjrbq36b7w4pbsi95wwh6a8qq7r69z1n"))))
    (properties
     `((upstream-name . "BiocpreprocessCore")
       (r-repository . bioconductor)))
    (build-system r-build-system)
    (home-page "http://bioconductor.org/packages/preprocessCore")
    (synopsis "Preprocess functions for Bioconductor")
    (description
     "A library of core preprocessing routines.")
    (license license:lgpl2.0+)))


(define-public r-wgcna
  (let ((commit "425bc170cc0873ddbd414675ac40f6d4d724c7cb"))
(package
  (name "r-wgcna")
  (version (string-append "1.49-" commit))
  (source (origin
           (method git-fetch)
           (uri (git-reference
                 ;; (url "https://github.com/genenetwork/WGCNA.git")
                 (url "https://github.com/pjotrp/WGCNA.git")
                 (commit commit)))
           (file-name (string-append name "-" commit))
           (sha256
            (base32
             "1zqnsb8s3065rq1y2y3l79zi8wmdwjkcjls96ypycrb7pmdil58j"))))
  (properties `((upstream-name . "WGCNA")))
  (build-system r-build-system)
  (propagated-inputs
   `( ;; ("r-annotationdbi" ,r-annotationdbi)
     ; ("r-biocparallel" ,r-biocparallel)
     ("r-dynamictreecut" ,r-dynamictreecut)
     ("r-doparallel" ,r-doparallel)
     ("r-fastcluster" ,r-fastcluster)
     ("r-foreach" ,r-foreach)
     ("r-go-db" ,r-go-db)
     ; ("r-grdevices" ,r-grdevices)
     ("r-hmisc" ,r-hmisc)
     ("r-impute" ,r-impute)
     ("r-matrixstats" ,r-matrixstats)
     ; ("r-parallel" ,r-parallel)
     ("r-biocpreprocesscore" ,r-biocpreprocesscore)
     ; ("r-splines" ,r-splines)
     ; ("r-stats" ,r-stats)
     ; ("r-survival" ,r-survival)
     ; ("r-utils" ,r-utils)
     ))
    (arguments
     `(
       #:tests? #f))   ; no 'setup.py test'
  (home-page
    "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
  (synopsis
    "Weighted gene correlation network analysis (wgcna)")
  (description
    "Functions necessary to perform Weighted Correlation Network
Analysis on high-dimensional data.  Includes functions for rudimentary
data cleaning, construction of correlation networks, module
identification, summarization, and relating of variables and modules
to sample traits.  Also includes a number of utility functions for
data manipulation and visualization.")
  (license license:gpl2+))))

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
    (license license:gpl2+)))

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

(define-public sambamba
  (let ((commit "2ca5a2dbac5ab90c3b4c588519edc3edcb71df84"))
    (package
      (name "sambamba")
      (version (string-append "0.5.9-1." (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference                
              (url "https://github.com/pjotrp/sambamba.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "1f14wn9aaxwjkmla6pzq3s28741carbr2v0fd2v2mm1dcpwnrqz5"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("ldc" ,ldc)
         ("lz4" ,lz4)
         ("rdmd" ,rdmd)
         ("zlib" ,zlib)
         ("perl" ,perl) ; Needed for htslib tests?
         ("ruby" ,ruby) ; Needed for htslib tests?
         ("python" ,python) ; Needed for htslib tests?
         ("gcc" ,gcc)
         ("htslib-src"
          ,(origin
             (method url-fetch)
             (uri "https://github.com/samtools/htslib/archive/1.3.tar.gz")
             (file-name "htslib-1.3.tar.gz")
             (sha256
              (base32 "1bqkif7yrqmiqak5yb74kgpb2lsdlg7y344qa1xkdg7k1l4m86i9"))
             (patches (list (search-patch "htslib-add-cram_to_bam.patch")))))
         ("biod-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/BioD.git")
                   (commit "7efdb8a2f7fdcd71c9ad9596be48d1262bb1bd5b")))
             (file-name "biod-src")
             (sha256
              (base32 "09icc2bjsg9y4hxjim4ql275izadf0kh1nnmapg8manyz6bc8svf"))))))
      (arguments
       `(#:tests? #f
         #:make-flags (list "-f" "Makefile.guix")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-after 'unpack 'unpack-htslib-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The current build compiles htslib statically into the
               ;; executable.  On top of that, we need to patch the latest
               ;; version of htslib to have it working with Sambamba.
               (and (with-directory-excursion "htslib"
                      (zero? (system* "tar" "xvf" (assoc-ref inputs "htslib-src")
                                      "--strip-components=1")))
                    (zero? (system* "rm" "-r" "BioD"))
                    (zero? (system* "ln" "--symbolic" "--no-target-directory"
                                    (assoc-ref inputs "biod-src") "BioD")))))
           (replace
            'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (zero? (system* "make" "-f" "Makefile.guix"
                              (string-append "LDC_LIB_PATH="
                                             (assoc-ref inputs "ldc")
                                             "/lib")))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "build/sambamba" bin)))))))
      (home-page "https://github.com/lomereiter/sambamba")
      (synopsis "A tool for working with SAM and BAM files written in D.")
      (description
       "Sambamba is a high performance modern robust and fast tool (and
library), written in the D programming language, for working with SAM
and BAM files.  Current parallelised functionality is an important
subset of samtools functionality, including view, index, sort,
markdup, and depth.")
      (license license:gpl2+))))

(define-public picard
  (package
    (name "picard")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/broadinstitute/picard/archive/"
             version ".tar.gz"))
       (sha256
        (base32 ""))))
    (build-system gnu-build-system)
    (home-page "http://broadinstitute.github.io/picard/")
    (synopsis "A set of Java command line tools for manipulating high-throughput
sequencing data (HTS) data and formats")
    (description "Picard comprises Java-based command-line utilities that
manipulate SAM files, and a Java API (HTSJDK) for creating new programs that
read and write SAM files. Both SAM text format and SAM binary (BAM) format are
supported.")
    ;; The license is MIT.
    (license license:expat)
))

(define-public fastqc
  (package
    (name "fastqc")
    (version "0.11.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/fastqc_v"
             version "_source.zip"))
       (sha256
        (base32 ""))))
    (build-system gnu-build-system)
    (arguments
     `(("perl" ,perl) ; Needed to run the java command.
       ("jdk" ,icedtea "jdk")))
    (native-inputs
     `(("ant" ,ant) ; TODO: Most Java packages need Ant, but in this case, IDK..
       ("jdk" ,icedtea "jdk")
       ;;("htsjdk" ,htsjdk) ; It is based on htsjdk, but it ships its own copy.
       ("unzip" ,unzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/fastqc/")
    (synopsis "A quality control tool for high throughput sequence data")
    (description
     "FastQC aims to provide a QC report which can spot problems which originate either in the sequencer or in the starting library material. It can either run as a stand alone interactive application for the immediate analysis of small numbers of FastQ files, or it can be run in a non-interactive mode where it would be suitable for integrating into a larger analysis pipeline for the systematic processing of large numbers of files.")
    (license license:gpl3+)))
