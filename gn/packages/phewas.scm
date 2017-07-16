(define-module (gn packages phewas)
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
  #:use-module (gn packages lmm)
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
  #:use-module (srfi srfi-1))

(define-public r-intermediate
  (let ((commit "1e4ec77a92889f7548da766cb28abab18df952dc"))
  (package
    (name "r-intermediate")
    (version "0.2-4")
    (source (origin
              ;; We use the git reference, because there's no CRAN package (yet)
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/simecek/intermediate.git")
                    (commit commit)))
              (file-name (string-append name "-" (string-take commit 7) "-checkout" ))
              (sha256
               (base32
                "0l7acxmizw8az37365z4gr9wc6ny9d4zx3q42dgvp04yyzwafbr8"))))
    (build-system r-build-system)
    ; (propagated-inputs
                                        ; `(("r-emma" ,r-emma)))
    (inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-magrittr" ,r-magrittr)
       ("r-plotly" ,r-plotly)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-testthat" ,r-testthat)))
    (synopsis "R/intermediate")
    (description
     "R package for eQTl/pQTL mediation analysis.")
    (home-page "https://github.com/simecek/intermediate")
    (license license:gpl3))))


(define-public r-mlmm
  (let ((commit "9fec9805573e97b44ee121f3651ddb79eafc8f8d"))
  (package
    (name "r-mlmm")
    (version "0.1.0")
    (source (origin
              ;; We use the git reference, because there's no CRAN package (yet)
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Gregor-Mendel-Institute/mlmm.git")
                    (commit commit)))
              (file-name (string-append name "-" (string-take commit 7) "-checkout" ))
              (sha256
               (base32
                "0dr77bf6i7qpi6kvay84p4lgib20l552ph8mp56v3qk0fbsamh8j"))))
    (build-system r-build-system)
    (propagated-inputs
    `(("r-emma" ,r-emma)))
    (synopsis "R/mlmm")
    (description
     "Implements an efficient multi-locus mixed-model approach for
genome-wide association studies in structured populations.")
    (home-page "https://github.com/Gregor-Mendel-Institute/mlmm")
    (license license:gpl3))))

(define-public r-fastmatch
(package
  (name "r-fastmatch")
  (version "1.1-0")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "fastmatch" version))
      (sha256
        (base32
          "0z80jxkygmzn11sq0c2iz357s9bpki548lg926g85gldhfj1md90"))))
  (build-system r-build-system)
  (home-page "http://www.rforge.net/fastmatch")
  (synopsis "Fast match() function")
  (description
    "Package providing a fast match() replacement for cases that require repeated look-ups.  It is slightly faster that R's built-in match() function on first match against a table, but extremely fast on any subsequent lookup as it keeps the hash table in memory.")
  (license license:gpl2)))

(define-public r-fgsea
  (package
    (name "r-fgsea")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "fgsea" version))
       (sha256
        (base32
         "1yq4s4g4xxzcpkv9dpbg29444iy38vfgwj4wgr47rjjq8njfblfx"))))
    (build-system r-build-system)
    ; (native-inputs
                                        ;  `(("gfortran" ,gfortran)))
    ; ‘Rcpp’, ‘data.table’, ‘BiocParallel’, ‘ggplot2’, ‘gridExtra’, ‘fastmatch
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-data-table" ,r-data-table)
       ("r-biocparallel" ,r-biocparallel)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-fastmatch" ,r-fastmatch)))
    (home-page "http://bioconductor.org/packages/fgsea")
    (synopsis "")
    (description
     ".")
    (license license:expat)))

(define-public r-qvalue
  (package
    (name "r-qvalue")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "qvalue" version))
       (sha256
        (base32
         "1dijh11v1kr29gnikq09pkdvm3qwmp1a406ahx9l4j6mgn8hlsfq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-reshape2" ,r-reshape2)
       ("r-ggplot2" ,r-ggplot2)))
    (home-page "http://bioconductor.org/packages/qvalue")
    (synopsis "")
    (description
     ".")
    (license license:expat)))

(define-public r-phewas ; GN2
  (package
   (name "r-phewas")
   (version "0.0.0-1")
   (source #f)
   (build-system trivial-build-system)
   (propagated-inputs
    `(
      ("r" ,r)
      ("r-data-table" ,r-data-table)
      ; ("r-emma" ,r-emma)
      ("r-fgsea" ,r-fgsea)
      ("r-intermediate" ,r-intermediate)
      ("r-limma" ,r-limma)
      ("r-mlmm" ,r-mlmm)
      ("r-qvalue" ,r-qvalue)
    ))
    (arguments
     `(; #:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
         (let* ((out  (assoc-ref %outputs "out"))
                (bash (assoc-ref %build-inputs "bash"))
                (foo  (string-append out "/foo")))
           (begin
             (use-modules (guix build utils))
             (mkdir out)
             #t))))

   (home-page
    "None")
   (synopsis
    "None")
   (description
    "None.")
   (license license:gpl2+)))
