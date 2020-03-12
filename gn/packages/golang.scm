(define-module (gn packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  )

(define-public go-golang-org-x-image
  (let ((commit "58c23975cae11f062d4b3b0c143fe248faac195d")
        (revision "1"))
    (package
      (name "go-golang-org-x-image")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://go.googlesource.com/image")
                       (commit commit)))
                (file-name (string-append "go.googlesource.com-image-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0i2p2girc1sfcic6xs6vrq0fp3szfx057xppksb67kliywjjrm5x"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/image"
         ; Source-only package
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'build))))
      (home-page "https://go.googlesource.com/image")
      (synopsis "Supplemental Go image libraries")
      (description "This package provides supplemental Go libraries for image
processing.")
      (license license:bsd-3))))

(define-public go-github-com-fiam-gounidecode
  (let ((commit "311ebb7422d3ff70cc1d064c5a297520ff5f304a")
        (revision "1"))
    (package
      (name "go-github-com-fiam-gounidecode")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/fiam/gounidecode")
                       (commit commit)))
                (file-name (string-append "go-github-com-fiam-gounidecode-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "107c2qnlgl9d55pww3kh3x5m5pqw7ijzzqbpsrpxnzn5jls44xbs"))))
      (build-system go-build-system)
      (arguments
       `(#:unpack-path "github.com/fiam/gounidecode"
         #:import-path "github.com/fiam/gounidecode/unidecode"))
      (home-page "https://github.com/fiam/gounidecode/")
      (synopsis "Unicode transliterator for Go")
      (description "Unicode transliterator for Go.")
      (license #f))))       ; No license listed.

(define-public go-golang-org-rainycape-unidecode
  (let ((commit "cb7f23ec59bec0d61b19c56cd88cee3d0cc1870c")
        (revision "1"))
    (package
      (name "go-golang-org-rainycape-unidecode")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/rainycape/unidecode")
                       (commit commit)))
                (file-name (string-append "go-golang-org-rainycape-unidecode-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1wvzdijd640blwkgmw6h09frkfa04kcpdq87n2zh2ymj1dzla5v5"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/rainycape/unidecode"))
      (home-page "https://github.com/rainycape/unidecode")
      (synopsis "Unicode transliterator in Golang")
      (description "Unicode transliterator in Golang - Replaces non-ASCII
characters with their ASCII approximations.")
      (license license:asl2.0))))

(define-public go-github-com-golang-freetype
  (let ((commit "e2365dfdc4a05e4b8299a783240d4a7d5a65d4e4")
        (revision "1"))
    (package
      (name "go-github-com-golang-freetype")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/golang/freetype")
                       (commit commit)))
                (file-name (string-append "go-github-com-golang-freetype-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "194w3djc6fv1rgcjqds085b9fq074panc5vw582bcb8dbfzsrqxc"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/golang/freetype"))
      (propagated-inputs
       `(("go-golang-org-x-image" ,go-golang-org-x-image)))
      (home-page "https://github.com/golang/freetype")
      (synopsis "Freetype font rasterizer in the Go programming language")
      (description "The Freetype font rasterizer in the Go programming language.")
      (license (list license:freetype
                     license:gpl2+)))))

(define-public go-github-com-fogleman-gg
  (package
    (name "go-github-com-fogleman-gg")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fogleman/gg")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nkldjghbqnzj2djfaxhiv35kk341xhcrj9m2dwq65v684iqkk8n"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f      ; Issue with test flags.
       #:import-path "github.com/fogleman/gg"))
    (propagated-inputs
     `(("go-github-com-golang-freetype" ,go-github-com-golang-freetype)))
    (home-page "https://github.com/fogleman/gg")
    (synopsis "2D rendering in Go")
    (description "@code{gg} is a library for rendering 2D graphics in pure Go.")
    (license license:expat)))

(define-public go-github-com-gedex-inflector
  (let ((commit "16278e9db8130ac7ec405dc174cfb94344f16325")
        (revision "1"))
    (package
      (name "go-github-com-gedex-inflector")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/gedex/inflector")
                       (commit commit)))
                (file-name (string-append "go-github-com-gedex-inflector-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "05hjqw1m71vww4914d9h6nqa9jw3lgjzwsy7qaffl02s2lh1amks"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gedex/inflector"))
      (home-page "https://github.com/gedex/inflector")
      (synopsis "Go library that pluralizes and singularizes English nouns")
      (description "Go library that pluralizes and singularizes English nouns.")
      (license license:bsd-2))))

(define-public go-github-com-klauspost-cpuid
  (package
    (name "go-github-com-klauspost-cpuid")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/klauspost/cpuid")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s510210wdj5dkamii1qrk7v87k4qpdcrrjzflp5ha9iscw6b06l"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/klauspost/cpuid"))
    (home-page "https://github.com/klauspost/cpuid")
    (synopsis "CPU feature identification for Go")
    (description "@code{cpuid} provides information about the CPU running the
current program.  CPU features are detected on startup, and kept for fast access
through the life of the application. Currently x86 / x64 (AMD64) is supported,
and no external C (cgo) code is used, which should make the library very eas
to use.")
    (license license:expat)))

(define-public go-github-com-pbnjay-memory
  (let ((commit "974d429e7ae40c89e7dcd41cfcc22a0bfbe42510")
        (revision "1"))
    (package
      (name "go-github-com-pbnjay-memory")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/pbnjay/memory")
                       (commit commit)))
                (file-name (string-append "go-github-com-pbnjay-memory-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0kazg5psdn90pqadrzma5chdwh0l2by9z31sspr47gx93fhjmkkq"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/pbnjay/memory"))
      (home-page "https://github.com/gedex/inflector")
      (synopsis "Go library to report total system memory")
      (description "@code{memory} provides a single method reporting total
physical system memory accessible to the kernel. It does not account for memory
used by other processes.")
      (license license:bsd-3))))

(define-public go-github-com-surgebase-porter2
  (let ((commit "56e4718818e8dc4ea5ba6348402fc7661863732a")
        (revision "1"))
    (package
      (name "go-github-com-surgebase-porter2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/surgebase/porter2")
                       (commit commit)))
                (file-name (string-append "go-github-com-surgebase-porter2-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ivcf83jlj9s7q5y9dfbpyl0br35cz8fcp0dm8sxxvqh54py06v2"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/surgebase/porter2"))
      (native-inputs
       `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
         ("go-github-com-surge-glog" ,go-github-com-surge-glog)))
      (home-page "https://github.com/surgebase/porter2")
      (synopsis "Go library implementing english Porter2 stemmer")
      (description "Porter2 implements the
@url{http://snowball.tartarus.org/algorithms/english/stemmer.html, english
Porter2 stemmer}.  It is written completely using finite state machines to do
suffix comparison, rather than the string-based or tree-based approaches.")
      (license license:asl2.0))))

(define-public go-github-com-surge-glog
  (let ((commit "2578deb2b95c665e6b1ebabf304ce2085c9e1985")
        (revision "1"))
    (package
      (name "go-github-com-surge-glog")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/surge/glog")
                       (commit commit)))
                (file-name (string-append "go-github-com-surge-glog-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1bxcwxvsvr2hfpjz9hrrn0wrgykwmrbyk567102k3vafw9xdcwk4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/surge/glog"))
      (home-page "https://github.com/surge/glog")
      (synopsis "Leveled execution logs for Go")
      (description "Leveled execution logs for Go.")
      (license license:asl2.0))))
