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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
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
  #:use-module (srfi srfi-1))

(define-public r-qtl
  (package
    (name "r-qtl")
    (version "1.37-11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "mirror://cran/src/contrib/qtl_"
              version ".tar.gz"))
        (sha256
          (base32
           "0h20d36mww7ljp51pfs66xq33yq4b4fwq9nsh02dpmfhlaxgx1xi"))))
    (build-system r-build-system)
    (home-page "http://www.rqtl.org")
    (synopsis "Tools for Analyzing QTL Experiments")
    (description "Bio-statistics library for analysis of experimental
crosses to identify genes (called quantitative trait loci, QTLs)
contributing to variation in quantitative traits.")
    (license license:gpl3+)))
