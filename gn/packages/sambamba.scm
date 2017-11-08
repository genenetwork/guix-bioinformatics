;; Sambamba

(define-module (gn packages sambamba)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics) ; for samtools in sambamba
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages ldc)
  #:use-module (gn packages shell)
  #:use-module (srfi srfi-1))

(define-public sambamba
  (let ((commit "7cff06533b539a99b4e0db681fb573214d63aae2"))
    (package
      (name "sambamba")
      (version (string-append "0.6.7-pre1-" (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pjotrp/sambamba.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "11k2xqgmbvxwki569439kvzf2b0cqy2x21kbgjijwvpqk9j8czx4"))))
      (build-system gnu-build-system)
      (outputs '("out"     ; disable all checks for speed
                 "debug"))
      (inputs
       `(("samtools" ,samtools) ; for pileup
         ("bcftools" ,bcftools) ; for pileup
         ("lz4" ,lz4)
         ("zlib" ,zlib)
       ))
      (native-inputs
       `(("ldc" ,ldc)
         ("shunit2" ,shunit2)
         ("coreutils" ,coreutils) ; for env
         ("perl" ,perl) ; Needed for building htslib
         ("ruby" ,ruby) ; Needed for building htslib
         ("python" ,python-2) ; Needed for building htslib and sambamba
         ("gcc" ,gcc)
         ("which" ,which)
         ("htslib-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/pjotrp/htslib.git")
                   (commit "2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5")))
             (file-name (string-append "htslib-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "0g38g8s3npr0gjm9fahlbhiskyfws9l5i0x1ml3rakzj7az5l9c9"))))
         ("biod-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/BioD.git")
                   (commit "c778e4f2d8bacea7499283ce39f5577b232732c6")))
             (file-name (string-append "biod-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "1z90562hg47i63gx042wb3ak2vqjg5z7hwgn9bp2pdxfg3nxrw37"))))
         ("dlang-undeaD-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/dlang/undeaD.git")
                   (commit "92803d25c88657e945511f0976a0c79d8da46e89")))
             (file-name (string-append "dlang-undeaD-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "0vq6n81vzqvgphjw54lz2isc1j8lcxwjdbrhqz1h5gwrvw9w5138"))))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'patch-pileup-d
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "sambamba/pileup.d"
                             (("string samtoolsBin     = null;") (string-append "string samtoolsBin = \"" (which "samtools") "\";"))
                             (("string bcftoolsBin     = null;") (string-append "string bcftoolsBin = \"" (which "bcftools") "\";"))
                             (("    this_app = args[0];") (string-append "    this_app = \"" (which "sambamba") "\";")))))
           (add-after 'unpack 'unpack-htslib-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The current build compiles htslib statically into the
               ;; executable.  On top of that, we need to patch the latest
               ;; version of htslib to have it working with Sambamba.
               (and 
                    (copy-recursively (assoc-ref inputs "htslib-src") "htslib")
                    (copy-recursively (assoc-ref inputs "dlang-undeaD-src") "undeaD")
                    (copy-recursively (assoc-ref inputs "biod-src") "BioD"))))
           (replace
            'build
            (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
              (let* ((out        (assoc-ref outputs "out"))
                     (debug-out  (assoc-ref outputs "debug")))
                (zero? (system* "make" "-f" "Makefile.guix" "guix" "-j" "8"
                                (string-append "LDC_LIB_PATH="
                                               (assoc-ref inputs "ldc")
                                               "/lib"))))))
           (replace
            'check
            (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
              (let* ((out        (assoc-ref outputs "out"))
                     (debug-out  (assoc-ref outputs "debug")))
                (zero? (system* "make" "-f" "Makefile.guix" "check"
                                (string-append "LDC_LIB_PATH="
                                               (assoc-ref inputs "ldc")
                                               "/lib"))))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "build/sambamba" bin)))))))
      (home-page "https://github.com/lomereiter/sambamba")
      (synopsis "Fast tool for working with SAM, BAM and CRAM files written in D.")
      (description
       "Sambamba is a high performance modern robust and fast
tool (and library), written in the D programming language, for working
with SAM, BAM and CRAM files.  Current parallelised functionality is
an important subset of samtools functionality, including view, index,
sort, markdup, and depth.")
      (license license:gpl2+))))
