(define-module (gn packages gemma)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (gn packages statistics)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages check)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages bootstrap)
  #:use-module (gn packages ldc)
  #:use-module (gn packages ldc)
  #:use-module (gn packages shell)
  #:use-module (srfi srfi-1))

(define-public gsl1
  (package
   (name "gsl1")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gsl/gsl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "26yfs5n444s03np1naj6yp1fsysd42kdscxzkg0k2yvfjixx0ijd"))
              ))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gsl/")
    (synopsis "Numerical library for C and C++")
    (description
     "The GNU Scientific Library is a library for numerical analysis in C
and C++.  It includes a wide range of mathematical routines, with over 1000
functions in total.  Subject areas covered by the library include:
differential equations, linear algebra, Fast Fourier Transforms and random
numbers.")
    (license license:gpl3+)))


(define-public gemma-git-gn2 ; guix candidate
  (let ((commit "ffc99d62013b1a5025b21cc385dd9892360ebe49"))
  (package
    (name "gemma-git-gn2")
    (version (string-append "0.97.2-gn2-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genetics-statistics/GEMMA")
                   (commit commit)))
             (file-name (string-append name "-" version))
             (sha256
              (base32
               "0v68p469a5x8m0sdh408zbxc8gqb5ysqpp5hvh8h51sjf8lwyxv6"))))
    (inputs `(
              ("gsl" ,gsl)
              ("eigen" ,eigen)
              ("shunit2" ,shunit2)
              ("lapack" ,lapack)
              ("openblas" ,openblas)
              ("zlib" ,zlib)
              ))
    (native-inputs ; for running tests
     `(("perl" ,perl)
       ("which" ,which)
       ))

    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        (string-append "EIGEN_INCLUDE_PATH="
                       (assoc-ref %build-inputs "eigen")
                       "/include/eigen3/")
        "FORCE_DYNAMIC=1"
        "DEBUG=1"
        "WITH_OPENBLAS=1")
       #:phases
        ; "/include/eigen3/"
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
       #:parallel-tests? #f))
    (home-page "http://www.xzlab.org/software.html")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "GEMMA is the software implementing the Genome-wide
Efficient Mixed Model Association algorithm for a standard linear
mixed model and some of its close relatives for genome-wide
association studies (GWAS).")
    (license license:gpl3))))

(define-public gemma-gn2
  (package
   (name "gemma")
   (version "0.96")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/xiangzhou/GEMMA/archive/v"
                                version ".tar.gz"))
            (sha256
             (base32
              "055ynn16gd12pf78n4vr2a9jlwsbwzajpdnf2y2yilg1krfff222"))))
   (inputs `(("gsl" ,gsl)
             ("lapack" ,lapack)
             ("zlib" ,zlib)))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags '("FORCE_DYNAMIC=1")
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
      #:tests? #f)) ; no tests included
   (home-page "")
   (synopsis "Tool for genome-wide efficient mixed model association")
   (description "GEMMA is software implementing the Genome-wide
Efficient Mixed Model Association algorithm for a standard linear
mixed model and some of its close relatives for genome-wide
association studies (GWAS).")
   (license license:gpl3)))

(define-public gemma-wrapper
  (package
    (name "gemma-wrapper")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-gemma-wrapper" version))
       (sha256
        (base32
         "16csqx5y63i5z0zkk1nq671n0vba482faskgsp1x1h75majqjdql"))))
    (build-system ruby-build-system)
    (propagated-inputs `(("gemma-git-gn2" ,gemma-git-gn2)))
    (arguments
     `(#:tests? #f))
    (synopsis
     "Gemma wrapper for LOCO and caching")
    (description "Gemma wrapper")
    (home-page "https://rubygems.org/gems/bio-gemma-wrapper")
    (license license:gpl3)))
