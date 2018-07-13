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
  #:use-module (gnu packages ldc)
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
  ; #:use-module (gn packages ldc)
  #:use-module (gn packages ldc)
  #:use-module (gn packages shell)
  #:use-module (srfi srfi-1))

(define-public faster-lmm-d ; incomplete, just creates build environment
  (let ((commit "dea6abf61b633ed55e6a2997068f2d54abe5376b"))
    (package
     (name "faster-lmm-d")
     (version (string-append "0.0.1-" (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pjotrp/faster_lmm_d.git")
                    (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1syrn32g1q06bsnrk9wl7q2m5mvvglys2zl92rx0bi49kp2i0ngv"))))
     (build-system trivial-build-system)
     (propagated-inputs
      `(("openblas" ,openblas)
        ("gsl" ,gsl)
        ("lapack" ,lapack)
        ("dub" ,dub)
        ("gcc" ,gcc)
        ("glibc" ,glibc)
        ("binutils" ,binutils) ; for ld linker
        ("gfortran:lib" ,gfortran "lib")
        ; ("make" ,make)
        ))
     (native-inputs
      `(("ldc" ,ldc)
        ("shunit2" ,shunit2)
        ("which" ,which)
        ))
     (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let ((target (string-append (assoc-ref %outputs "out")
                                       "/share")))
            (write target)
            (mkdir-p target)
            (copy-recursively (assoc-ref %build-inputs "source") target)
            #t))))
     (home-page "https://github.com/pjotrp")
     (synopsis "Parallel LMM")
     (description
      ".")
     (license license:gpl3))))

(define-public openblas-git
  (let ((commit "893bd14e924fa72a4ed345a75d64c637f1b1c550"))
  ; (let ((commit "36aea5ce2d5565e006a027a23e805cd9ff0e2eee"))
    (package
    (name "openblas-git")
    (version (string-append "0.2.20-git-" (string-take commit 7)))
    ; (version (string-append "0.3.1-git-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/xianyi/OpenBLAS.git")
                   (commit commit)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "0qv03c2yq46p9sajc3a3f56ijfifyv6f4n51a81wc2hihy4ilcap"))))
               ; "1rva61y4lwi6zkja5349r59fxichdhqaqk608kklw5bwk04fjh86"))))
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
               ; BINARY=64 NO_WARMUP=0 GEMM_MULTITHREAD_THRESHOLD=4 USE_THREAD=1 NO_AFFINITY=0 NO_LAPACK=0 NUM_THREADS=64
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "SHELL=bash"
             "NUM_THREADS=64"
             "BINARY=64"
             "NO_WARMUP=0"
             "GEMM_MULTITHREAD_THRESHOLD=4"
             "USE_THREAD=1"
             "NO_AFFINITY=0"
             "NO_LAPACK=0"    ; use OpenBlas LAPACK
             "COMMON_PROF=0"  ; disable profiling
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
                   '("DYNAMIC_ARCH=1"))
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
    (license license:bsd-3))))


(define-public gemma-gn2 ; guix candidate - generic openblas version
  (let ((commit "c760aa09c2aa91ca6270b5f898c27e9aed376a73"))
  (package
    (name "gemma-gn2")
    (version (string-append "0.97-" (string-take commit 7)))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/GEMMA")
                   (commit commit)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "0s61w8av70kqvkb00znrqnr59axygh9gnlxar8rfyasr5g6xvl8w"))))
    (inputs `(
              ("gfortran:lib" ,gfortran "lib")
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
                       "WITH_LAPACK=1"
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
       ; #:tests? #f
       #:parallel-tests? #f))
    (home-page "http://www.xzlab.org/software.html")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "GEMMA is the software implementing the Genome-wide
Efficient Mixed Model Association algorithm for a standard linear
mixed model and some of its close relatives for genome-wide
association studies (GWAS).")
    (license license:gpl3))))

(define-public gemma-gn2-git ; openblas optimized
  (package
   (inherit gemma-gn2)
   (name "gemma-gn2-git")
   (inputs `(
             ("gfortran:lib" ,gfortran "lib")
             ("gsl" ,gsl)
             ("eigen" ,eigen)
             ("shunit2" ,shunit2)
             ("openblas" ,openblas-git)
             ("zlib" ,zlib)
             ))
    (arguments
     `(#:make-flags
       (list
        (string-append "EIGEN_INCLUDE_PATH="
                       (assoc-ref %build-inputs "eigen")
                       "/include/eigen3/")
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
       ; #:tests? #f
       #:parallel-tests? #f))
   ))

(define-public gemma-wrapper
  (package
    (name "gemma-wrapper")
    (version "0.97")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-gemma-wrapper" version))
       (sha256
        (base32
         "0nypw1fnq03ssm73rij7xcgz3fqgx5rdzksbpggylbnvn7fz1q17"))))
    (build-system ruby-build-system)
    (inputs `(("gemma-gn2" ,gemma-gn2-git)))
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

(define-public gemma-dev-env
  (let ((md5 "93e745e9c"))
    (package
    (name "gemma-dev-env")
    (version "0.98")
    (source
     (origin
       (method url-fetch)
       (uri "http://biogems.info/genenetwork2-2.0-a8fcff4.svg") ; any old file
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "0rir1mcn3a8i1mbw3ppgnjl7wg71mapljik7n3v5i8j5ic95mqr5"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    (inputs `(("sassc" ,sassc)))
    (propagated-inputs
     `(("gemma-wrapper" ,gemma-wrapper)
        ;; ("gcc" ,gcc-8)
       ))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share")))
             (write target)
             (mkdir-p target)
             ; (copy-recursively (assoc-ref %build-inputs "source") target)
             #t))))

    (home-page "http://github.com/genetics-statistics/")
    (synopsis "GEMMA development environment imports build tools, gemma-wrapper and faster-lmm-d")
    (description "Gemma-development")
    (license license:gpl3))))
