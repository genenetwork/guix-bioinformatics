(define-module (gn packages lmm)
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

(define-public r-emma ; guix potential candidate (not in CRAN)
  (package
   (name "r-emma")
   (version "1.1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://mouse.cs.ucla.edu/emma/emma_"
                  version ".tar.gz"))
            (sha256
             (base32
              "0m9lhjawfqxw7hlcghxc1bs5bh0645wq9gr1lz7gagxlphjlcj2p"))))
   (build-system r-build-system)
   ; (propagated-inputs
   ;  `(("r-clustersim" ,r-clustersim)
   ;    ("r-earth" ,r-earth)))
   (home-page
    "http://mouse.cs.ucla.edu/emma/")
   (synopsis
    "Evolutionary model-based multiresponse approach")
   (description
    "Non CRAN update of the evolutionary model-based multiresponse
approach (EMMA) is a novel methodology to process optimisation and
product improvement.  The approach is suitable to contexts in which
the experimental cost and/or time limit the number of implementable
trials.")
   (license license:gpl2+)))