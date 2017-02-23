;; Sambamba

(define-module (gn packages sambamba)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gn packages ldc)
  #:use-module (gn packages shell)
  #:use-module (srfi srfi-1))

(define-public sambamba
  (let ((commit "6ae174bcb50d3a8f1b6dd10de9c68bbc4770e56a"))
    (package
      (name "sambamba")
      (version (string-append "0.6.6-pre3-" (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pjotrp/sambamba.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "1m26i8icllclynqia4yi2mp7zfs2zajllva6bzxn188fhlyjwzqr"))))
      (build-system gnu-build-system)
      (outputs '("out"
                 "debug"))
      (native-inputs
       `(("ldc" ,ldc)
         ("lz4" ,lz4)
         ("shunit2" ,shunit2)
         ("zlib" ,zlib)
         ("coreutils" ,coreutils) ; for env
         ("perl" ,perl) ; Needed for htslib
         ("ruby" ,ruby) ; Needed for htslib
         ("python" ,python-2) ; Needed for htslib
         ("gcc" ,gcc)
         ("which" ,which)
         ("htslib-src"
          ,(origin
             (method url-fetch)
             (uri "https://github.com/lomereiter/htslib/archive/2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5.tar.gz")
             ;;(uri "https://github.com/samtools/htslib/archive/1.3.tar.gz")
             (file-name "htslib-0.2.0-rc10-271-g2f3c3ea-dirty.tar.gz")
             (sha256
              (base32 "0bl6w856afnbgdsw8bybsxpqsyf2ba3f12rqh47hhpxvv866g08w"))))
              ;;(base32 "1bqkif7yrqmiqak5yb74kgpb2lsdlg7y344qa1xkdg7k1l4m86i9"))
             ;;(patches (list (search-patch "htslib-add-cram_to_bam.patch")))))
         ("biod-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/pjotrp/BioD.git")
                   (commit "b7f1db860d212ee5fb6f9adfb36c6e783aaeb6f5")))
             (file-name (string-append "biod-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "01xkdjdn9lb2b4b5ykzhnrk2rjikagav8b3fyac3zafcfq600cr4"))))
         ("dlang-undeaD-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/dlang/undeaD.git")
                   (commit "610234f159132f91046d4fb893889fb8ee14cd2f")))
             (file-name (string-append "dlang-undeaD-src-" (string-take commit 7) "-checkout"))
             (sha256
              (base32 "12zxsgvka4a82ghp2gaviph6kz13jzjb5pbc8v6i3rmcnifzpbrl"))))))
      (arguments
       `(#:phases
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
                    (copy-recursively (assoc-ref inputs "dlang-undeaD-src") "undeaD")
                    (copy-recursively (assoc-ref inputs "biod-src") "BioD"))))
           (replace
            'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (zero? (system* "make" "-f" "Makefile.guix" "guix-debug"
                       (string-append "LDC_LIB_PATH="
                                             (assoc-ref inputs "ldc")
                                             "/lib")))))
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
