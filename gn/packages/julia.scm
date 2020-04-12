(define-module (gn packages julia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system julia)
  #:use-module (gn packages cran)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics)
  #:use-module (ice-9 match))

(define-public julia-lmgpu
  (let ((commit "e9e95b5fa46f1905ca1ff32a3684a2616a7e482c")
        (revision "1"))
    (package
      (name "julia-lmgpu")
      (version (git-version "0.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ChelseaTrotter/LMGPU.jl")
                       (commit commit)))
                ;(file-name (git-file-name name version))
                (file-name "LMGPU")
                (sha256
                 (base32
                  "1ddx2np1lakw1l2dclpcaihxd0fcj6bjxsvaxr6g5brxjqk5j7b1"))))
      (build-system julia-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; This is a super ugly hack. Some JULIA environment variable should
           ;; be tuned so it can find the artifact directory.
           (add-after 'unpack 'symlink-zlib-into-artifact-directory
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((julia-dir (string-append (assoc-ref outputs "out")
                                               "/share/julia")))
                 (mkdir-p julia-dir)
                 (symlink
                   (string-append (assoc-ref inputs "julia-zlib-jll")
                                  "/share/julia/artifacts")
                   (string-append julia-dir "/artifacts")))
               #t))
           (add-after 'precompile 'check
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (builddir (string-append out "/share/julia/")))
                 (setenv "JULIA_LOAD_PATH"
                         (string-append builddir "packages/" ":"
                                        (or (getenv "JULIA_LOAD_PATH")
                                            "")))
                 (setenv "HOME" (getcwd))
                 (invoke "julia" "test/runtests.jl")))))))
      (native-inputs
       `(("r" ,r-minimal)
         ("r-mice" ,r-mice)
         ("r-qtl2" ,r-qtl2)
         ("r-tictoc" ,r-tictoc)
         ("r-tidyverse" ,r-tidyverse)))
      (propagated-inputs
       `(("julia-zipfile" ,julia-zipfile)))
      (home-page "https://github.com/ChelseaTrotter/LMGPU.jl")
      (synopsis "")
      (description "")
      (license license:expat))))

(define-public julia-lmgpu-myapp
  (package
    (inherit julia-lmgpu)
    (name "julia-lmgpu-myapp")
    (source
      (origin (inherit (package-source julia-lmgpu))
              (file-name "MyApp")))
    (arguments
     (substitute-keyword-arguments (package-arguments julia-lmgpu)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'change-directory
             (lambda _
               (chdir "bin/MyApp") #t))))))
    (propagated-inputs
     `(("julia-lmgpu" ,julia-lmgpu)
       ,@(package-propagated-inputs julia-lmgpu)))
    (native-inputs
     `(("julia-packagecompiler" ,julia-packagecompiler)))))

(define-public julia-zipfile
  (package
    (name "julia-zipfile")
    (version "0.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fhs/ZipFile.jl")
               (commit (string-append "v" version))))
        ;(file-name (git-file-name name version))
        (file-name "ZipFile")
        (sha256
         (base32
          "1fpvlhfqg5kgq5vchlf8dyc73r6dzki0dz7plddc3bnr0ld00rlw"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This is a super ugly hack. Some JULIA environment variable should
         ;; be tuned so it can find the artifact directory.
         (add-after 'unpack 'symlink-zlib-into-artifact-directory
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((julia-dir (string-append (assoc-ref outputs "out")
                                             "/share/julia")))
               (mkdir-p julia-dir)
               (symlink
                 (string-append (assoc-ref inputs "julia-zlib-jll")
                                "/share/julia/artifacts")
                 (string-append julia-dir "/artifacts")))
             #t)))))
    (propagated-inputs
     `(("julia-zlib-jll" ,julia-zlib-jll)))
    (home-page "https://github.com/fhs/ZipFile.jl")
    (synopsis "Read/Write ZIP archives in Julia")
    (description "This module provides support for reading and writing ZIP
archives in Julia.")
    (license license:expat)))

(define-public julia-zlib-jll
  (package
    (name "julia-zlib-jll")
    (version "1.2.11+9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaBinaryWrappers/Zlib_jll.jl")
               (commit (string-append "Zlib-v" version))))
        ;(file-name (git-file-name name version))
        (file-name "Zlib_jll")
        (sha256
         (base32
          "0m9n8dp4bwhkyjag1szmhz02k0bxzm4ka2ia2jh8crnd1qi8w9dz"))))
    (build-system julia-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'symlink-zlib-into-artifact-directory
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((artifacts (string-append (assoc-ref outputs "out")
                                             "/share/julia/artifacts")))
               (mkdir-p artifacts)
               (symlink
                 (assoc-ref inputs "zlib")
                 ;; from git-tree-sha1 in Artifacts.toml
                 (string-append
                   artifacts
                   ,(match (%current-system)
                      ("x86_64-linux" "/7846a2956a213715c2c76632f3461cef87d9d545")
                      ("i686-linux" "/c8456cbd00982236828623bbc63f21b9b7b03821")
                      ("armhf-linux" "/748c38025b5596a5005a87ac2b9476603cf8615b")
                      ("aarch64-linux" "/3dd0c7cd5424c8746a1a32034ba1b10458f20b3b")
                      (_ "/UNSUPPORTED")))))
             #t)))))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "https://github.com/JuliaBinaryWrappers/Zlib_jll.jl")
    (synopsis "Autogenerated package constructed using BinaryBuilder.jl")
    (description "This is an autogenerated package constructed using
@url{https://github.com/JuliaPackaging/BinaryBuilder.jl, BinaryBuilder.jl}.")
    (license license:expat)))

(define-public julia-packagecompiler
  (package
    (name "julia-packagecompiler")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/JuliaLang/PackageCompiler.jl")
               (commit (string-append "v" version))))
        ;(file-name (git-file-name name version))
        (file-name "PackageCompiler")
        (sha256
         (base32
          "1s9xc17i308fdpyvkz1w6qb1h7yncdr2jgk1szfvygxd6yzkv1b4"))))
    (build-system julia-build-system)
    (home-page "https://github.com/JuliaLang/PackageCompiler.jl")
    (synopsis "Compile your Julia Package")
    (description "PackageCompiler is a Julia package with two main purposes:
@itemize
@item Creating custom sysimages for reduced latency when working locally with
packages that has a high startup time.
@item Creating \"apps\" which are a bundle of files including an executable that
can be sent and run on other machines without Julia being installed on that machine.
@end itemize")
    (license license:expat)))
