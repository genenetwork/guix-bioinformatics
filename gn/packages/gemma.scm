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

(define-public openblas-haswell
  (package
    (name "openblas-haswell")
    (version "0.2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/xianyi/OpenBLAS/tarball/v"
                           version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bd03c5xni0bla0wg1wba841b36b0sg13sjja955kn5xzvy4i61a"))))
    (build-system gnu-build-system)
    (arguments

     `(
       #:tests? #f  ;no "check" target

       ;; DYNAMIC_ARCH is only supported on x86.  When it is disabled and no
       ;; TARGET is specified, OpenBLAS will tune itself to the build host, so
       ;; we need to disable substitutions.
       #:substitutable?
        ,(let ((system (or (%current-target-system) (%current-system))))
           (or (string-prefix? "x86_64" system)
               (string-prefix? "i686" system)
               (string-prefix? "mips" system)
               (string-prefix? "aarch64" system)))
       #:make-flags
       (list (string-append 
             "PREFIX=" (assoc-ref %outputs "out"))
             "SHELL=bash"
             "NUM_THREADS=64"
             "BINARY=64"
             "NO_WARMUP=0"
             ; "GEMM_MULTITHREAD_THRESHOLD=4"
             "USE_THREAD=1"
             ; "NO_AFFINITY=0"
             "NO_LAPACK=0"
             "COMMON_PROF="  ; disable profiling
             "DEBUG=0"

             ;; Build the library for all supported CPUs.  This allows
             ;; switching CPU targets at runtime with the environment variable
             ;; OPENBLAS_CORETYPE=<type>, where "type" is a supported CPU type.
             ;; Unfortunately, this is not supported on non-x86 architectures,
             ;; where it leads to failed builds.
             ,@(let ((system (or (%current-target-system) (%current-system))))
                 (cond
                  ; ((or (string-prefix? "x86_64" system)
                  ;     (string-prefix? "i686" system))
                  ((string-prefix? "x86_64" system)
                   '("TARGET=HASWELL"))
                  ;; On MIPS we force the "SICORTEX" TARGET, as for the other
                  ;; two available MIPS targets special extended instructions
                  ;; for Loongson cores are used.
                  ((string-prefix? "mips" system)
                   '("TARGET=SICORTEX"))
                  ;; On aarch64 force the generic 'armv8-a' target
                  ((string-prefix? "aarch64" system)
                   '("TARGET=ARMV8"))
                  (else '()))))
       ;; no configure script
       #:phases (alist-delete 'configure %standard-phases)))
    (inputs
     `(("gfortran" ,gfortran)
       ("gfortran:lib" ,gfortran "lib")))
    (native-inputs
     `(("cunit" ,cunit)
       ("perl" ,perl)))
    (home-page "http://www.openblas.net/")
    (synopsis "Platform optimized BLAS library based on GotoBLAS")
    (description
     "OpenBLAS is a BLAS library forked from the GotoBLAS2-1.13 BSD version.")
    (license license:bsd-3)))


(define-public gsl1 ; supporting older GSL tests
  (package
   (name "gsl1")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gsl/gsl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lrgipi0z6559jqh82yx8n4xgnxkhzj46v96dl77hahdp58jzg3k"))
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
  (let ((commit "89690967e33bcde4fd8736fe048d0e9dce5c6266"))
  (package
    (name "gemma-git-gn2")
    (version (string-append "0.97-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/GEMMA")
                   (commit commit)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "1kj1vsqdwnszyd2523snfiamgqgjdal65p1m9454p2ck72gxf7kn"))))
    (inputs `(
              ("gsl" ,gsl)
              ("eigen" ,eigen)
              ("shunit2" ,shunit2)
              ; ("lapack" ,lapack) - included in openblas-haswell
              ("openblas-haswell" ,openblas-haswell)
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
        "fast-check"
        )
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
       #:tests? #f
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
    (version "0.92.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-gemma-wrapper" version))
       (sha256
        (base32
         "08apz0imsxzwhzv2iicq2g5zx1iq1vlfrhk7khsfaydshbq5g741"))))
    (build-system ruby-build-system)
    (inputs `(("gemma-git-gn2" ,gemma-git-gn2)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'build 'set-gemma-path
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
                     (substitute* "bin/gemma-wrapper"
                      ; (("gemma_command = ENV['GEMMA_COMMAND']")
                      (("gemma_command = ENV.*")
                       (string-append "gemma_command = '" (which "gemma") "'")))
                     ))))))
    (synopsis
     "Gemma wrapper for LOCO and caching")
    (description "Gemma wrapper")
    (home-page "https://rubygems.org/gems/bio-gemma-wrapper")
    (license license:gpl3)))
