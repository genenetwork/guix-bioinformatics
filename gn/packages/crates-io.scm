(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

;; Please keep these packages sorted alphabetically

(define-public rust-accelerate-src
  (package
    (name "rust-accelerate-src")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "accelerate-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17fiqyq7f9k41pbsyrvk9pxyx9z6fw399wq036cvwkbmb14xcpj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-libc" ,rust-libc))
       #:skip-build? #t)) ; Only available for macOS.
    (home-page "https://github.com/blas-lapack-rs/accelerate-src")
    (synopsis "Source of BLAS and LAPACK via the Accelerate framework")
    (description
      "The package provides a source of BLAS and LAPACK via the Accelerate
 framework.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-addr2line
  (package
    (name "rust-addr2line")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "addr2line" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1daaxrzk6fmfzaqi06y704hcw0rjz199l0n9214ybfm3m3jnmc4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cpp-demangle" ,rust-cpp-demangle)
        ("rust-fallible-iterator"
         ,rust-fallible-iterator)
        ("rust-gimli" ,rust-gimli)
        ("rust-intervaltree" ,rust-intervaltree)
        ("rust-lazycell" ,rust-lazycell)
        ("rust-object" ,rust-object-0.12)
        ("rust-rustc-demangle" ,rust-rustc-demangle)
        ("rust-smallvec" ,rust-smallvec))
       #:cargo-development-inputs
       (("rust-backtrace" ,rust-backtrace)
        ("rust-clap" ,rust-clap)
        ("rust-findshlibs" ,rust-findshlibs)
        ("rust-memmap" ,rust-memmap)
        ("rust-rustc-test" ,rust-rustc-test))
       #:tests? #f)) ; output_equivalence fails
    (home-page "https://github.com/gimli-rs/addr2line")
    (synopsis
      "Symbolication library written in Rust, using `gimli`")
    (description
      "This package provides a cross-platform symbolication library written in Rust, using `gimli`")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-addr2line-0.9
  (package
    (inherit rust-addr2line)
    (name "rust-addr2line")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "addr2line" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17rlf04nx3g3rcy661v24ksnmpk6vqn680g5b5sp8lk20iih2xnx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cpp-demangle" ,rust-cpp-demangle)
        ("rust-fallible-iterator"
         ,rust-fallible-iterator)
        ("rust-gimli" ,rust-gimli-0.18)
        ("rust-intervaltree" ,rust-intervaltree)
        ("rust-lazycell" ,rust-lazycell)
        ("rust-object" ,rust-object-0.12)
        ("rust-rustc-demangle" ,rust-rustc-demangle)
        ("rust-smallvec" ,rust-smallvec))
       #:cargo-development-inputs
       (("rust-backtrace" ,rust-backtrace)
        ("rust-clap" ,rust-clap)
        ("rust-findshlibs" ,rust-findshlibs-0.4)
        ("rust-memmap" ,rust-memmap)
        ("rust-rustc-test" ,rust-rustc-test))
       #:tests? #f)))) ; TODO: All the tests fail.

(define-public rust-afl
  (package
    (name "rust-afl")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "afl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "14k6hnwzqn7rrs0hs87vcfqj4334k9wff38d15378frlxpviaard"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc)
        ("rust-clap" ,rust-clap)
        ("rust-rustc-version" ,rust-rustc-version)
        ("rust-xdg" ,rust-xdg))
       #:cargo-development-inputs
       (("rust-rustc-version" ,rust-rustc-version)
        ("rust-xdg" ,rust-xdg))))
    (home-page "https://github.com/rust-fuzz/afl.rs")
    (synopsis "Fuzzing Rust code with american-fuzzy-lop")
    (description
      "Fuzzing Rust code with american-fuzzy-lop")
    (license license:asl2.0)))

(define-public rust-aho-corasick
  (package
    (name "rust-aho-corasick")
    (version "0.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0b8dh20fhdc59dhhnfi89n2bi80a8zbagzd5c122hf1vv2amxysq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment))))
    (home-page "https://github.com/BurntSushi/aho-corasick")
    (synopsis "Fast multiple substring searching")
    (description
      "Fast multiple substring searching.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-aho-corasick-0.6
  (package
    (inherit rust-aho-corasick)
    (name "rust-aho-corasick")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aho-corasick" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "19f8v503ibvlyr824g5ynicrh1lsmp2i0zmpszr8lqay0qw3vkl1"))))
    (arguments
     `(#:cargo-inputs
       (("rust-memchr" ,rust-memchr))
       #:cargo-development-inputs
       (("rust-csv" ,rust-csv)
        ("rust-docopt" ,rust-docopt)
        ("rust-memmap" ,rust-memmap-0.6)
        ("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive))))))

(define-public rust-android-glue
  (package
    (name "rust-android-glue")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "android-glue" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01y495x4i9vqkwmklwn2xk7sqg666az2axjcpkr4iwngdwi48100"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/tomaka/android-rs-glue")
    (synopsis "Glue for the Android JNI")
    (description "Glue for the Android JNI")
    (license license:expat)))

(define-public rust-ansi-term-0.12
  (package
    (name "rust-ansi-term")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment)
        ("rust-regex" ,rust-regex)
        ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis
      "Library for ANSI terminal colours and styles (bold, underline)")
    (description
      "Library for ANSI terminal colours and styles (bold, underline)")
    (license license:expat)))

(define-public rust-ansi-term-0.9
  (package
    (inherit rust-ansi-term)
    (name "rust-ansi-term")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi-term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xif1bh938qpfc3d0f9xgidibpm65xix11w9gszwqnia00q7rb13"))))
    (arguments '())))

(define-public rust-approx
  (package
    (name "rust-approx")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "approx" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hx580xjdxl3766js9b49rnbnmr8gw8c060809l43k9f0xshprph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-complex" ,rust-num-complex)
        ("rust-num-traits" ,rust-num-traits))))
    (home-page "https://github.com/brendanzab/approx")
    (synopsis "Approximate floating point equality comparisons and assertions")
    (description
     "Approximate floating point equality comparisons and assertions.")
    (license license:asl2.0)))

(define-public rust-approx-0.1
  (package
    (inherit rust-approx)
    (name "rust-approx")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "approx" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "153awzwywmb61xg857b80l63b1x6hifx2pha7lxf6fck9qxwraq8"))))
    (arguments '())))

(define-public rust-argon2rs
  (package
    (name "rust-argon2rs")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "argon2rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "14mkgkrjd4b4zy92pflz6yb4j1wn2chbd8jczxknxbkdm2vb0rrz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blake2-rfc" ,rust-blake2-rfc)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool))
       #:cargo-development-inputs
       (("rust-cargon" ,rust-cargon))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               (("path = \"benches/cargon\",") ""))
             #t)))))
    (home-page "https://github.com/bryant/argon2rs")
    (synopsis "Rust library for the Argon2 hashing algorithm")
    (description
     "This is a purely Rust-based library that provides both variants of the
state-of-the-art Argon2 hashing algorithm, suitable for password hashing and
password-based key derivation.")
    (license license:expat)))

(define-public rust-arrayvec
  (package
    (name "rust-arrayvec")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arrayvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fmhq4ljxr954mdyazaqa9kdxryl5d2ggr5rialylrd6xndkzmxq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-nodrop" ,rust-nodrop)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bencher" ,rust-bencher)
         ("rust-matches" ,rust-matches)
         ("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis
      "A vector with fixed capacity, backed by an array (it can be stored on the stack too). Implements fixed capacity ArrayVec and ArrayString.")
    (description
      "This package provides a vector with fixed capacity, backed by an array (it can be stored on the stack too).  Implements fixed capacity ArrayVec and ArrayString.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ascii
  (package
    (name "rust-ascii")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ascii" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1mn5az4hkxgjhwy157pr1nrfdb3qjpw8jw8v91m2i8wg59b21qwi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-serde" ,rust-serde)
        ("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/tomprogrammer/rust-ascii")
    (synopsis
      "ASCII-only equivalents to `char`, `str` and `String`.")
    (description
      "ASCII-only equivalents to `char`, `str` and `String`.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-atlatl
  (package
    (name "rust-atlatl")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atlatl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18kyvdm56fdb52b1sryi80xgs3nkjdylynsv324aiqnj85l1bfrj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-fst" ,rust-fst-0.1)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-quickcheck" ,rust-quickcheck-0.7)
         ("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/tapeinosyne/atlatl")
    (synopsis "Double-array tries.")
    (description "Double-array tries.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-automod
  (package
    (name "rust-automod")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "automod" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17am5i7z7jpsrq9bm0wyhf4q9850g2kqvzl3ik900x5gc7brwv2a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/automod")
    (synopsis "Pull in every source file in a directory as a module.")
    (description
      "Pull in every source file in a directory as a module.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-average
  (package
    (name "rust-average")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "average" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16ib3vlfp3akpflha1m6598x3jfrgrh9qgrf9vvv11rfqirp22dx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-conv" ,rust-conv)
         ("rust-float-ord" ,rust-float-ord)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-serde" ,rust-serde)
         ("rust-serde-big-array" ,rust-serde-big-array)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-bencher" ,rust-bencher)
         ("rust-proptest" ,rust-proptest)
         ("rust-quantiles" ,rust-quantiles)
         ("rust-rand" ,rust-rand)
         ("rust-rand-distr" ,rust-rand-distr)
         ("rust-rand-xoshiro" ,rust-rand-xoshiro)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-streaming-stats" ,rust-streaming-stats))))
    (home-page "https://github.com/vks/average")
    (synopsis "Calculate statistics iteratively")
    (description "Calculate statistics iteratively")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-average-0.9
  (package
    (inherit rust-average)
    (name "rust-average")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "average" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1f8ya00bv6qki9m7b4lb3bn845rj473mx02qpm7wgy5qc1yp75xs"))))
    (arguments
      `(#:cargo-inputs
        (("rust-conv" ,rust-conv)
         ("rust-float-ord" ,rust-float-ord)
         ("rust-num-integer" ,rust-num-integer)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-serde" ,rust-serde)
         ("rust-serde-big-array" ,rust-serde-big-array)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-bencher" ,rust-bencher)
         ("rust-quantiles" ,rust-quantiles)
         ("rust-rand" ,rust-rand-0.6)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-streaming-stats" ,rust-streaming-stats))))))

(define-public rust-backtrace
  (package
    (name "rust-backtrace")
    (version "0.3.35")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0mfwbb6832rh1za304w8x37bvs9fjbybpmmz0iksqfzsaf108w8k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-addr2line" ,rust-addr2line-0.9)
        ("rust-backtrace-sys" ,rust-backtrace-sys)
        ("rust-cfg-if" ,rust-cfg-if)
        ("rust-compiler-builtins"
         ,rust-compiler-builtins)
        ("rust-cpp-demangle" ,rust-cpp-demangle)
        ("rust-findshlibs" ,rust-findshlibs)
        ("rust-goblin" ,rust-goblin)
        ("rust-libc" ,rust-libc)
        ("rust-memmap" ,rust-memmap)
        ("rust-rustc-demangle" ,rust-rustc-demangle)
        ("rust-rustc-serialize" ,rust-rustc-serialize)
        ("rust-rustc-std-workspace-core"
         ,rust-rustc-std-workspace-core)
        ("rust-serde" ,rust-serde)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis
      "A library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (description
      "This package provides a library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-base64
  (package
    (name "rust-base64")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13k6bvd3n6dm7jqn9x918w65dd9xhx454bqphbnv0bkd6n9dj98b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/marshallpierce/rust-base64")
    (synopsis
      "encodes and decodes base64 as bytes or utf8")
    (description
      "encodes and decodes base64 as bytes or utf8")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-base64-0.9
  (package
    (inherit rust-base64)
    (name "rust-base64")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base64" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0hs62r35bgxslawyrn1vp9rmvrkkm76fqv0vqcwd048vs876r7a8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder))
       #:cargo-development-inputs
       (("rust-safemem" ,rust-safemem)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-bincode
  (package
    (name "rust-bincode")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bincode" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xx6bp39irvsndk6prnmmq8m1l9p6q2qj21j6mfks2y81pjsa14z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-autocfg" ,rust-autocfg)
         ("rust-byteorder" ,rust-byteorder)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-bytes" ,rust-serde-bytes)
         ("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://github.com/servo/bincode")
    (synopsis "Binary serialization / deserialization strategy")
    (description
      "This package provides a binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
    (license license:expat)))

(define-public rust-bindgen
  (package
    (name "rust-bindgen")
    (version "0.51.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l4sifn771v83q8wafyvmsrphjni8i8wvgdlddd09v35f3fhq9qq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-cexpr" ,rust-cexpr)
         ("rust-cfg-if" ,rust-cfg-if)
         ("rust-clang-sys" ,rust-clang-sys)
         ("rust-clap" ,rust-clap)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-fxhash" ,rust-fxhash)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-log" ,rust-log)
         ("rust-peeking-take-while"
          ,rust-peeking-take-while)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-regex" ,rust-regex)
         ("rust-shlex" ,rust-shlex)
         ("rust-which" ,rust-which))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap)
         ("rust-diff" ,rust-diff)
         ("rust-shlex" ,rust-shlex))
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'set-environmental-variable
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((clang (assoc-ref inputs "libclang")))
                (setenv "LIBCLANG_PATH"
                        (string-append clang "/lib")))
              #t)))))
    (inputs
     `(("libclang" ,clang)))
    (home-page
      "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-bindgen-0.46
  (package
    (inherit rust-bindgen)
    (name "rust-bindgen")
    (version "0.46.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qclvj5pydn5camw396b0r3nz4nn3p5wpxg4fgg1favp043pyzwg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-cexpr" ,rust-cexpr)
         ("rust-cfg-if" ,rust-cfg-if)
         ("rust-clang-sys" ,rust-clang-sys-0.26)
         ("rust-clap" ,rust-clap)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-hashbrown" ,rust-hashbrown)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-log" ,rust-log)
         ("rust-peeking-take-while"
          ,rust-peeking-take-while)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-regex" ,rust-regex)
         ("rust-which" ,rust-which))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap)
         ("rust-diff" ,rust-diff)
         ("rust-shlex" ,rust-shlex))))))

(define-public rust-bindgen-0.39
  (package
    (inherit rust-bindgen)
    (name "rust-bindgen")
    (version "0.39.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "14awv09jqiayzjdm77crizln61wxlj7lc8mpgk4c7vz95mgyvi7a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-cexpr" ,rust-cexpr-0.2)
        ("rust-cfg-if" ,rust-cfg-if)
        ("rust-clang-sys" ,rust-clang-sys-0.23)
        ("rust-clap" ,rust-clap)
        ("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-lazy-static" ,rust-lazy-static)
        ("rust-log" ,rust-log)
        ("rust-peeking-take-while"
         ,rust-peeking-take-while)
        ("rust-proc-macro2" ,rust-proc-macro2-0.3.5) ; 0.3.5
        ("rust-quote" ,rust-quote-0.5)
        ("rust-regex" ,rust-regex)
        ("rust-which" ,rust-which-1))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap)
        ("rust-diff" ,rust-diff)
        ("rust-shlex" ,rust-shlex))))))

(define-public rust-bit-set
  (package
    (name "rust-bit-set")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bit-set" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "100ac8867bvbx9kv634w4xjk98b71i8nq4wdcvpf3cf4ha4j6k78"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bit-vec" ,rust-bit-vec))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/contain-rs/bit-set")
    (synopsis "A set of bits")
    (description
      "This package provides a set of bits")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bitflags-0.9
  (package
    (inherit rust-bitflags)
    (name "rust-bitflags")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "19dk39gfwmhi3iy1x0wgml1fv1bkb525ywy25zwihbm063i05zaf"))))))

(define-public rust-bitflags-0.8
  (package
    (inherit rust-bitflags)
    (name "rust-bitflags")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1x5z8hmirpnapkx6sww8gkc6x0q8ppni0lbsigm3mrba5byfjw0k"))))))

(define-public rust-bitflags-0.7
  (package
    (inherit rust-bitflags)
    (name "rust-bitflags")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0v8hh6wdkpk9my8z8442g4hqrqf05h0qj53dsay6mv18lqvqklda"))))))

(define-public rust-blake2-rfc
  (package
    (name "rust-blake2-rfc")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blake2-rfc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0034g47hyq2bzmk40895ill1mbnpmmjakdq3dmm9clidvl5m6vax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec)
        ("rust-constant-time-eq" ,rust-constant-time-eq)
        ("rust-clippy" ,rust-clippy))
       #:cargo-development-inputs
       (("rust-data-encoding" ,rust-data-encoding))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ddont-require-rust-clippy
           (lambda _
             (substitute* "Cargo.toml"
               (("0.0.41") "0.0"))
             #t)))))
    (home-page "https://github.com/cesarb/blake2-rfc")
    (synopsis "pure Rust implementation of BLAKE2 based on RFC 7693")
    (description
     "This is a pure Rust implementation of BLAKE2 based on RFC 7693.  This
crate is limited to the features described in the RFC: only the @code{digest
length} and @code{key length} parameters can be used.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-blas-src
  (package
    (name "rust-blas-src")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blas-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0cxln7bgwxbknaf3qbv4hscy9k53msax14x0szvvp680km3z9zs6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-accelerate-src" ,rust-accelerate-src)
        ("rust-intel-mkl-src" ,rust-intel-mkl-src)
        ("rust-netlib-src" ,rust-netlib-src)
        ("rust-openblas-src" ,rust-openblas-src-0.6))))
    (home-page
      "https://github.com/blas-lapack-rs/blas-src")
    (synopsis
      "The package provides a BLAS source of choice.")
    (description
      "The package provides a BLAS source of choice.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-blas-src-0.2
  (package
    (inherit rust-blas-src)
    (name "rust-blas-src")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blas-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0l9c1gjhld3ajalak1ipklxfjvwqyy3l7xl019spdbqlrk8r9f57"))))))

(define-public rust-blobby
  (package
    (name "rust-blobby")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blobby" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xicpf3s2mi5xqnx8ps5mdych4ib5nh2nfsbrsg8ar8bjk1girbg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder))
        #:cargo-development-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-hex" ,rust-hex))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Iterator over simple binary blob storage")
    (description
      "Iterator over simple binary blob storage")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-block
  (package
    (name "rust-block")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-objc-test-utils" ,rust-objc-test-utils))))
    (home-page
      "http://github.com/SSheldon/rust-block")
    (synopsis
      "Rust interface for Apple's C language extension of blocks.")
    (description
      "Rust interface for Apple's C language extension of blocks.")
    (license license:expat)))

(define-public rust-bodyparser
  (package
    (name "rust-bodyparser")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bodyparser" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0c1gynj9l7wv3mjrzr5jifmy0pjdwachfqz09aygdmmab3xan8zh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-iron" ,rust-iron)
        ("rust-persistent" ,rust-persistent)
        ("rust-plugin" ,rust-plugin)
        ("rust-serde" ,rust-serde)
        ("rust-serde-json" ,rust-serde-json))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://github.com/iron/body-parser")
    (synopsis "Body parsing middleware for Iron.")
    (description "Body parsing middleware for Iron.")
    (license license:expat)))

(define-public rust-bstr
  (package
    (name "rust-bstr")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bstr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i82qr3z4vwcdx7gjsjrqy64w1w88i5s7b7ab97hm4mbb1djqv4d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-memchr" ,rust-memchr)
         ("rust-regex-automata" ,rust-regex-automata)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-ucd-parse" ,rust-ucd-parse)
         ("rust-unicode-segmentation"
          ,rust-unicode-segmentation))))
    (home-page "https://github.com/BurntSushi/bstr")
    (synopsis
      "A string type that is not required to be valid UTF-8.")
    (description
      "This package provides a string type that is not required to be valid UTF-8.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bumpalo
  (package
    (name "rust-bumpalo")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bumpalo" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "018b5calz3895v04shk9bn7i73r4zf8yf7p1dqg92s3xya13vm1c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/fitzgen/bumpalo")
    (synopsis
      "Fast bump allocation arena for Rust")
    (description
      "This package provides a fast bump allocation arena for Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bytecount
  (package
    (name "rust-bytecount")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytecount" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vplsx73zncb7mz8x0fs3k0p0rz5bmavj09vjk5nqn4z6fa7h0dh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-packed-simd" ,rust-packed-simd))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion)
         ("rust-quickcheck" ,rust-quickcheck)
         ("rust-rand" ,rust-rand))))
    (home-page "https://github.com/llogiq/bytecount")
    (synopsis
      "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
    (description
      "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bytecount-0.4
  (package
    (inherit rust-bytecount)
    (name "rust-bytecount")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytecount" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "13qpy38z5wx0rzcdvr2h0ixbfgi1dbrif068il3hwn3k2mah88mr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-packed-simd" ,rust-packed-simd))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-byteorder
  (package
    (name "rust-byteorder")
    (version "1.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byteorder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xbwjlmq2ziqjmjvkqxdx1yh136xxhilxd40bky1w4d7hn4xvhx7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/BurntSushi/byteorder")
    (synopsis
      "Library for reading/writing numbers in big-endian and little-endian.")
    (description
      "Library for reading/writing numbers in big-endian and little-endian.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-byteorder-0.5
  (package
    (inherit rust-byteorder)
    (name "rust-byteorder")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "byteorder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ma8pkyz1jbglr29m1yzlc9ghmv6672nvsrn7zd0yn5jqs60xh8g"))))
    (arguments
      `(#:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.2)
         ("rust-rand" ,rust-rand-0.3))
        #:tests? #f)))) ; Tests needs 'unicode' crate.

(define-public rust-bytes
  (package
    (name "rust-bytes")
    (version "0.4.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bytes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0768a55q2fsqdjsvcv98ndg9dq7w2g44dvq1avhwpxrdzbydyvr0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-either" ,rust-either)
         ("rust-iovec" ,rust-iovec)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/tokio-rs/bytes")
    (synopsis
      "Types and traits for working with bytes")
    (description
      "Types and traits for working with bytes")
    (license license:expat)))

(define-public rust-c2-chacha
  (package
    (name "rust-c2-chacha")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "c2-chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00a11qdc8mg3z0k613rhprkc9p6xz0y7b1681x32ixg0hr3x0r3x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-ppv-lite86" ,rust-ppv-lite86)
         ("rust-stream-cipher" ,rust-stream-cipher))
        #:cargo-development-inputs
        (("rust-hex-literal" ,rust-hex-literal-0.1))))
    (home-page
      "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "The ChaCha family of stream ciphers")
    (description
      "The ChaCha family of stream ciphers")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cargo-metadata
  (package
    (name "rust-cargo-metadata")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo-metadata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09n4fp9hrg0z84y5q0q98rlinh0832zls3q0s0ip4dbxzlqkf2vh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-semver" ,rust-semver)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap)
         ("rust-docopt" ,rust-docopt)
         ("rust-structopt" ,rust-structopt))
        #:cargo-test-flags '("--release" "--test" "selftest")))
    (home-page "https://github.com/oli-obk/cargo_metadata")
    (synopsis
      "structured access to the output of `cargo metadata`")
    (description
      "structured access to the output of `cargo metadata`")
    (license license:expat)))

(define-public rust-cargo-metadata-0.6
  (package
    (inherit rust-cargo-metadata)
    (name "rust-cargo-metadata")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo-metadata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1givpi2w7iwqqnl87x5yc15zcm5hs6yw490sb6abkfp1h39v9lg5"))))
    (arguments
      `(#:cargo-inputs
        (("rust-error-chain" ,rust-error-chain)
         ("rust-semver" ,rust-semver)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap)
         ("rust-docopt" ,rust-docopt-0.8))))))

(define-public rust-cargo-metadata-0.5
  (package
    (inherit rust-cargo-metadata)
    (name "rust-cargo-metadata")
    (version "0.5.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo-metadata" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1l3ba9mb0ihh4n33s41y3lifpfy41dgcbccz216fs0yacfwa1z0y"))))
    (arguments
     `(#:cargo-inputs
       (("rust-error-chain" ,rust-error-chain-0.11)
        ("rust-semver" ,rust-semver)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive)
        ("rust-serde-json" ,rust-serde-json))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap)
        ("rust-docopt" ,rust-docopt-0.8))))))

(define-public rust-cast
  (package
    (name "rust-cast")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09yl2700crxa4n860b080msij25klvs1kfzazhp2aihchvr16q4j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.4))
       #:tests? #f)) ; Tests require 'unicode' crate.
    (home-page "https://github.com/japaric/cast.rs")
    (synopsis
      "Ergonomic, checked cast functions for primitive types")
    (description
      "Ergonomic, checked cast functions for primitive types")
    (license (list license:expat license:asl2.0))))

(define-public rust-cexpr
  (package
    (name "rust-cexpr")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cexpr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1by64ini3f058pwad3immx5cc12wr0m0kwgaxa8apzym03mj9ym7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nom" ,rust-nom-4))
       #:cargo-development-inputs
       (("rust-clang-sys" ,rust-clang-sys))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
             #t)))))
    (inputs
     `(("libclang" ,clang)))
    (home-page "https://github.com/jethrogb/rust-cexpr")
    (synopsis "A C expression parser and evaluator")
   (description
      "This package provides a C expression parser and evaluator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cexpr-0.2
  (package
    (inherit rust-cexpr)
    (name "rust-cexpr")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cexpr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0v1xa3758czmj8h97gh548mr8g0v13ixxvrlm1s79nb7jmgc9aj2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-nom" ,rust-nom-3))
       #:cargo-development-inputs
       (("rust-clang-sys" ,rust-clang-sys-0.11))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
             #t)))))
    (inputs
     `(("libclang" ,clang)))))

(define-public rust-cgl
  (package
    (name "rust-cgl")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cgl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zs7skrsyrsm759vfy2cygkx52fx91b567a12bpaz1sf4d8hbv8c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc))
       #:skip-build? #t)) ; Only for macOS.
    (home-page "https://github.com/servo/cgl-rs")
    (synopsis "Rust bindings for CGL on Mac")
    (description "Rust bindings for CGL on Mac")
    (license #f)))

(define-public rust-cgmath
  (package
    (name "rust-cgmath")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cgmath" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rvgila6ivr0dh1bxza450a4yfwdi2pwj3h1vnwg0jy4xk6l8f98"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-approx" ,rust-approx)
         ("rust-mint" ,rust-mint)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-rand" ,rust-rand)
         ("rust-serde" ,rust-serde)
         ("rust-simd" ,rust-simd))
        #:cargo-development-inputs
        (("rust-glium" ,rust-glium)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/brendanzab/cgmath")
    (synopsis
      "A linear algebra and mathematics library for computer graphics.")
    (description
      "This package provides a linear algebra and mathematics library for computer graphics.")
    (license license:asl2.0)))

(define-public rust-cgmath-0.16
  (package
    (inherit rust-cgmath)
    (name "rust-cgmath")
    (version "0.16.1")                                                                                                                                                                                          (source
    (origin
    (method url-fetch)
    (uri (crate-uri "cgmath" version))
    (file-name
    (string-append name "-" version ".tar.gz"))
    (sha256
    (base32
    "07754c03v3srzf64ghsl3fggrdi4kjy6l3vyq2d2wfjfixybb934"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-approx" ,rust-approx-0.1)
         ("rust-mint" ,rust-mint)
         ("rust-num-traits" ,rust-num-traits-0.1)
         ("rust-rand" ,rust-rand-0.4)
         ("rust-serde" ,rust-serde)
         ("rust-simd" ,rust-simd))
        #:cargo-development-inputs
        (("rust-glium" ,rust-glium)
         ("rust-serde-json" ,rust-serde-json))))))

(define-public rust-chrono
  (package
    (name "rust-chrono")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "chrono" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1glam3iqhshbamzgf0npn7hgghski92r31lm7gg8841hnxc1zn3p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-num-integer" ,rust-num-integer)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-serde" ,rust-serde)
         ("rust-time" ,rust-time))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode)
         ("rust-doc-comment" ,rust-doc-comment)
         ("rust-num-iter" ,rust-num-iter)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ci-info
  (package
    (name "rust-ci-info")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ci_info" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "01n3gxmwp765m6xg1fl8v1y12wsvbqvlcai27kdr5d2skrijyfb7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-envmnt" ,rust-envmnt)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive))
       #:tests? #f)) ; Tests require internet.
    (home-page "https://github.com/sagiegurari/ci_info")
    (synopsis
      "Provides current CI environment information.")
    (description
      "Provides current CI environment information.")
    (license license:asl2.0)))

(define-public rust-clang-sys-0.23
  (package
    (inherit rust-clang-sys)
    (name "rust-clang-sys")
    (version "0.23.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hjr333izzhs6bic84qwnyzy5xzmvasib8f3zkzj4ln3a97c1xyp"))))
    (arguments
      `(#:cargo-inputs
        (("rust-glob" ,rust-glob-0.2)
         ("rust-libc" ,rust-libc)
         ("rust-libloading" ,rust-libloading))
        #:cargo-development-inputs
        (("rust-glob" ,rust-glob-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
             #t)))))
    (inputs
     `(("libclang" ,clang)))))

(define-public rust-clang-sys-0.11
  (package
    (inherit rust-clang-sys)
    (name "rust-clang-sys")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17i47skqp1d9svil2m1wspnhz7ci1x0fipia70ns0qffciwiz48r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-0.7)
         ("rust-clippy" ,rust-clippy)
         ("rust-glob" ,rust-glob-0.2)
         ("rust-lazy-static" ,rust-lazy-static-0.2)
         ("rust-libc" ,rust-libc)
         ("rust-libloading" ,rust-libloading-0.3))
        #:cargo-development-inputs
        (("rust-clippy" ,rust-clippy)
         ("rust-glob" ,rust-glob-0.2))
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'set-environmental-variable
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((clang (assoc-ref inputs "libclang")))
                (setenv "LIBCLANG_PATH"
                        (string-append clang "/lib")))
              #t)))))
    (inputs
      `(("libclang" ,clang)))))

(define-public rust-clap
  (package
    (name "rust-clap")
    (version "2.33.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nf6ld3bims1n5vfzhkvcb55pdzh04bbhzf8nil5vvw05nxzarsh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term)
        ("rust-atty" ,rust-atty)
        ("rust-bitflags" ,rust-bitflags)
        ("rust-clippy" ,rust-clippy)
        ("rust-strsim" ,rust-strsim-0.8)
        ("rust-term-size" ,rust-term-size)
        ("rust-textwrap" ,rust-textwrap)
        ("rust-unicode-width" ,rust-unicode-width)
        ("rust-vec-map" ,rust-vec-map)
        ("rust-yaml-rust" ,rust-yaml-rust-0.3))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static)
        ("rust-regex" ,rust-regex)
        ("rust-version-sync" ,rust-version-sync))))
    (home-page "https://clap.rs/")
    (synopsis "Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured
Command Line Argument Parser.")
    (license license:expat)))

(define-public rust-clippy
  (package
    (name "rust-clippy")
    (version "0.0.302")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clippy" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1562x3sq9mgmc8j39gd34wqm7ybrdvpmj7cc1n450gwsawayw4fr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-term" ,rust-term-0.5))
       #:skip-build? #t)) ; DEPRECATED: please run `rustup component add clippy-preview` instead
    (home-page "https://github.com/rust-lang/rust-clippy")
    (synopsis "Helpful lints to avoid common pitfalls in Rust")
    (description
     "This package provides a bunch of helpful lints to avoid common pitfalls in Rust.")
    (license (list license:asl2.0
                   license:expat))))

;; This is the last version hosted on crates.io
(define-public rust-clippy-0.0.212
  (package
    (inherit rust-clippy)
    (name "rust-clippy")
    (version "0.0.212")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clippy" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cq5s3m6ckgi2nxjzzlmb3fdx4ym96zg25ng49zrrhqc7bqkl9by"))))
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace)
        ("rust-clippy-lints" ,rust-clippy-lints)
        ("rust-num-traits" ,rust-num-traits)
        ("rust-regex" ,rust-regex)
        ("rust-semver" ,rust-semver)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-ansi-term" ,rust-ansi-term)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.5)
        ("rust-clippy-mini-macro-test"
         ,rust-clippy-mini-macro-test)
        ("rust-compiletest-rs" ,rust-compiletest-rs)
        ("rust-derive-new" ,rust-derive-new)
        ("rust-lazy-static" ,rust-lazy-static)
        ("rust-rustc-version" ,rust-rustc-version)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive))
       ;; thread 'main' panicked at 'current rustc version information does not contain a rustc commit date'
       #:skip-build? #t))))

(define-public rust-clippy-mini-macro-test
  (package
    (name "rust-clippy-mini-macro-test")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clippy-mini-macro-test" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01175ynzasmyymx7w4rgh0dzcp9mqac9y4fgz9xa9xb56cgjz9x2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t)) ; Requires unstable features.
    (home-page
      "https://github.com/rust-lang-nursery/rust-clippy")
    (synopsis
      "A macro to test clippy's procedural macro checks")
    (description
      "This package provides a macro to test clippy's procedural macro checks")
    (license license:mpl2.0)))

(define-public rust-clippy-lints
  (package
    (name "rust-clippy-lints")
    (version "0.0.212")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clippy-lints" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04l579yx9485qx8ksr9m153kmb9gml6v6p5xmmr9cr05ah32c8xx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cargo-metadata" ,rust-cargo-metadata-0.5)
         ("rust-if-chain" ,rust-if-chain-0.1)
         ("rust-itertools" ,rust-itertools-0.7)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-matches" ,rust-matches)
         ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.1)
         ("rust-quine-mc-cluskey" ,rust-quine-mc-cluskey)
         ("rust-regex-syntax" ,rust-regex-syntax)
         ("rust-semver" ,rust-semver)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-toml" ,rust-toml-0.4)
         ("rust-unicode-normalization"
          ,rust-unicode-normalization)
         ("rust-url" ,rust-url-1))
       #:skip-build? #t)) ; Everything fails hard
    (home-page
      "https://github.com/rust-lang-nursery/rust-clippy")
    (synopsis
      "A bunch of helpful lints to avoid common pitfalls in Rust")
    (description
      "This package provides a bunch of helpful lints to avoid common pitfalls in Rust")
    (license license:mpl2.0)))

(define-public rust-cocoa
  (package
    (name "rust-cocoa")
    (version "0.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cocoa" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n9pklag536ghbw93hhld8gzp1fykag67mc6h953p2c0x12h1llc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-block" ,rust-block)
         ("rust-core-foundation" ,rust-core-foundation)
         ("rust-core-graphics" ,rust-core-graphics)
         ("rust-foreign-types" ,rust-foreign-types)
         ("rust-libc" ,rust-libc)
         ("rust-objc" ,rust-objc))))
    (home-page
      "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa for macOS")
    (description "Bindings to Cocoa for macOS")
    (license #f)))

(define-public rust-color-quant
  (package
    (name "rust-color-quant")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "color-quant" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ga56jrafnjm80903nnqjkyii4bwd6a7visxh0g8hgi6cmrvbfqd"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/PistonDevelopers/color_quant.git")
    (synopsis
      "Color quantization library to reduce n colors to 256 colors.")
    (description
      "Color quantization library to reduce n colors to 256 colors.")
    (license license:expat)))

(define-public rust-colored
  (package
    (name "rust-colored")
    (version "1.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "colored" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00vwd3r2jrd6qz4r91bwqhmkl371wyyjvirrc7bzh9r91yv91nvc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-winconsole" ,rust-winconsole))
        #:cargo-development-inputs
        (("rust-ansi-term" ,rust-ansi-term-0.9)
         ("rust-rspec" ,rust-rspec-1.0.0-beta3))))
    (home-page "https://github.com/mackwic/colored")
    (synopsis
      "The most simple way to add colors in your terminal")
    (description
      "The most simple way to add colors in your terminal")
    (license license:mpl2.0)))

(define-public rust-compiler-error
  (package
    (name "rust-compiler-error")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compiler-error" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0irh7c0gznk2k6mj3cmqw7x4pg59lppmy1y8d6k5xc926rnmz5zg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)) ; Build requires unstable features.
       #:tests? #f)) ; Tests require unstable features.
    (home-page "https://github.com/lu-zero/compiler_error")
    (synopsis "Triggerable compiler error")
    (description "Triggerable compiler error")
    (license license:expat)))

(define-public rust-compiletest-rs
  (package
    (name "rust-compiletest-rs")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compiletest-rs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1qz70s11n0bdy61wd36bw1d00riscxknqi8wdmldbynw3gd53wjs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-diff" ,rust-diff)
        ("rust-filetime" ,rust-filetime-0.1)
        ("rust-getopts" ,rust-getopts)
        ("rust-libc" ,rust-libc)
        ("rust-log" ,rust-log-0.3)
        ("rust-miow" ,rust-miow-0.2)
        ("rust-rustc-serialize" ,rust-rustc-serialize)
        ("rust-tempdir" ,rust-tempdir)
        ("rust-winapi" ,rust-winapi))
       #:skip-build? #t)) ; Requires unstable features.
    (home-page "https://github.com/laumann/compiletest-rs")
    (synopsis "Compiletest utility from the Rust compiler")
    (description
     "This project is an attempt at extracting the @code{compiletest} utility
from the Rust compiler.  The @code{compiletest} utility is useful for library
and plugin developers, who want to include test programs that should fail to
compile, issue warnings or otherwise produce compile-time output.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-compiletest-rs-0.2
  (package
    (inherit rust-compiletest-rs)
    (name "rust-compiletest-rs")
    (version "0.2.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compiletest-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0njz4shbhl1pvb6ngpi1wpz2gr5lf2dcha22lpdk995pzrwd6h97"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-log-0.3)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-tempdir" ,rust-tempdir))
        #:skip-build? #t)))) ; Requires unstable features.

(define-public rust-console
  (package
    (name "rust-console")
    (version "0.7.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "console" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a4n2syzik9lh02v2i4wdazvm05d99bib7dw0lqvz8mq2hn7r9cc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-atty" ,rust-atty)
         ("rust-clicolors-control"
          ,rust-clicolors-control)
         ("rust-encode-unicode" ,rust-encode-unicode)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-libc" ,rust-libc)
         ("rust-parking-lot" ,rust-parking-lot)
         ("rust-regex" ,rust-regex)
         ("rust-termios" ,rust-termios)
         ("rust-unicode-width" ,rust-unicode-width)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/mitsuhiko/console")
    (synopsis
      "A terminal and console abstraction for Rust")
    (description
      "This package provides a terminal and console abstraction for Rust")
    (license license:expat)))

(define-public rust-console-error-panic-hook
  (package
    (name "rust-console-error-panic-hook")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "console-error-panic-hook" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04d2narcrzk9bnddz17rr2l819l82pr0h6d98s2w9q236n87dndq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen))))
    (home-page
      "https://github.com/rustwasm/console_error_panic_hook")
    (synopsis
      "A panic hook for `wasm32-unknown-unknown` that logs panics to `console.error`")
    (description
      "This package provides a panic hook for `wasm32-unknown-unknown` that logs panics to `console.error`")
    (license #f)))

(define-public rust-conv
  (package
    (name "rust-conv")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "conv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "168j1npqrif1yqxbgbk0pdrx9shzhs5ylc5a4xw49b6hbxi11zvq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-custom-derive" ,rust-custom-derive))
        #:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/DanielKeep/rust-conv")
    (synopsis
      "This crate provides a number of conversion traits with more specific semantics than those provided by 'as' or 'From'/'Into'.")
    (description
      "This crate provides a number of conversion traits with more specific semantics than those provided by 'as' or 'From'/'Into'.")
    (license license:expat)))

(define-public rust-cookie
  (package
    (name "rust-cookie")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mdvqixahcywvqp0y8k2skkgbpfhsp0w73l9mz93dcrx1gq091l8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-time" ,rust-time)
         ("rust-base64" ,rust-base64)
         ("rust-ring" ,rust-ring)
         ("rust-url" ,rust-url))))
    (home-page "https://github.com/SergioBenitez/cookie-rs")
    (synopsis "Crate for parsing HTTP cookie headers and managing a cookie jar. Supports signed and private (encrypted + signed) jars.")
    (description "Crate for parsing HTTP cookie headers and managing a cookie jar. Supports signed and private (encrypted + signed) jars.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cookie-store
  (package
    (name "rust-cookie-store")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0204vzqszkjs5j4bqf8rrzrjj9c6f1zdydsid8ndkc7h7bx1v90l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cookie" ,rust-cookie)
         ("rust-idna" ,rust-idna)
         ("rust-log" ,rust-log)
         ("rust-publicsuffix" ,rust-publicsuffix)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-time" ,rust-time)
         ("rust-try-from" ,rust-try-from)
         ("rust-url" ,rust-url))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-pretty-assertions" ,rust-pretty-assertions))))
    (home-page
      "https://github.com/pfernie/cookie_store")
    (synopsis
      "Implementation of Cookie storage and retrieval per [RFC6265](http://tools.ietf.org/html/rfc6265)")
    (description
      "Implementation of Cookie storage and retrieval per [RFC6265](http://tools.ietf.org/html/rfc6265)")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cookie-store-0.7
  (package
    (inherit rust-cookie-store)
    (name "rust-cookie-store")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cookie_store" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "174i9k9g62pfx7y1nqynywdpjplkl3j4hi3ck6bz2r996qzhnxa6"))))
    (arguments
      `(#:cargo-inputs
        (("rust-cookie" ,rust-cookie)
         ("rust-idna" ,rust-idna)
         ("rust-log" ,rust-log)
         ("rust-publicsuffix" ,rust-publicsuffix)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-time" ,rust-time)
         ("rust-try-from" ,rust-try-from)
         ("rust-url" ,rust-url))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-pretty-assertions" ,rust-pretty-assertions))))))

(define-public rust-core-arch
  (package
    (name "rust-core-arch")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core_arch" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04vdvr9vj0f1cv2p54nsszmrrk9w1js4c0z4i0bdlajl1lydslim"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-wasm-bindgen-test"
          ,rust-wasm-bindgen-test))))
    (home-page
      "https://github.com/rust-lang/stdarch")
    (synopsis
      "`core::arch` - Rust's core library architecture-specific intrinsics.")
    (description
      "`core::arch` - Rust's core library architecture-specific intrinsics.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-core-foundation
  (package
    (name "rust-core-foundation")
    (version "0.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0va97wf49c8dzm9c8pgyk1jn7z21rl0bj1syf2zz5m2z2hzy1f95"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-chrono" ,rust-chrono)
         ("rust-core-foundation-sys"
          ,rust-core-foundation-sys)
         ("rust-libc" ,rust-libc)
         ("rust-uuid" ,rust-uuid))))
    (home-page
      "https://github.com/servo/core-foundation-rs")
    (synopsis
      "Bindings to Core Foundation for macOS")
    (description
      "Bindings to Core Foundation for macOS")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-core-graphics
  (package
    (name "rust-core-graphics")
    (version "0.17.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-graphics" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1acm3vygngnilzlr6klym5ywh7kfzh2xxrh2l41152hwmdl0jyan"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-core-foundation" ,rust-core-foundation)
         ("rust-foreign-types" ,rust-foreign-types)
         ("rust-libc" ,rust-libc))))
    (home-page
      "https://github.com/servo/core-graphics-rs")
    (synopsis "Bindings to Core Graphics for OS X")
    (description
      "Bindings to Core Graphics for OS X")
    (license #f)))

(define-public rust-cpp-demangle
  (package
    (name "rust-cpp-demangle")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpp_demangle" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0a4hqsfc0sfdwy7pcr0rc1fjp2j47fxbkqfc2lfrbi4zlm5hq36k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-afl" ,rust-afl)
         ("rust-cfg-if" ,rust-cfg-if)
         ("rust-glob" ,rust-glob-0.2))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap)
         ("rust-diff" ,rust-diff)
         ("rust-glob" ,rust-glob-0.2))))
    (home-page
      "https://github.com/gimli-rs/cpp_demangle")
    (synopsis "A crate for demangling C++ symbols")
    (description
      "This package provides a crate for demangling C++ symbols")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crates-index
  (package
    (name "rust-crates-index")
    (version "0.13.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crates-index" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n7pp6mk59hw3nqlh8irxc9pp0g5ziw7bprqsw2lxvg13cvdp76s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-error-chain" ,rust-error-chain)
         ("rust-git2" ,rust-git2-0.8)
         ("rust-glob" ,rust-glob-0.2)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))
        #:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             #t)))
        #:tests? #f)) ; tests with curl want network access
    (inputs
     `(("openssl" ,openssl)))
    (home-page
      "https://github.com/frewsxcv/rust-crates-index")
    (synopsis
      "Library for retrieving and interacting with the crates.io index")
    (description
      "Library for retrieving and interacting with the crates.io index")
    (license license:asl2.0)))

(define-public rust-crc32fast
  (package
    (name "rust-crc32fast")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crc32fast" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1c9dhkvf3brrzzplcijaywxi2w8wv5578i0ryhcm7x8dmzi5s4ms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/srijs/rust-crc32fast")
    (synopsis
      "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
    (description
      "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-criterion
  (package
    (name "rust-criterion")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "criterion" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iig7r9c6bkn5qb6axxkblc1amif6k49lix35rhqs728cphh71wk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-atty" ,rust-atty)
         ("rust-cast" ,rust-cast)
         ("rust-clap" ,rust-clap)
         ("rust-criterion-plot" ,rust-criterion-plot)
         ("rust-csv" ,rust-csv)
         ("rust-itertools" ,rust-itertools)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-rand-core" ,rust-rand-core)
         ("rust-rand-os" ,rust-rand-os)
         ("rust-rand-xoshiro" ,rust-rand-xoshiro)
         ("rust-rayon" ,rust-rayon)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-tinytemplate" ,rust-tinytemplate)
         ("rust-walkdir" ,rust-walkdir))
        #:cargo-development-inputs
        (("rust-approx" ,rust-approx)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-rand" ,rust-rand)
         ("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://bheisler.github.io/criterion.rs/book/index.html")
    (synopsis
      "Statistics-driven micro-benchmarking library")
    (description
      "Statistics-driven micro-benchmarking library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-criterion-0.2
  (package
    (inherit rust-criterion)
    (name "rust-criterion")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "criterion" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1543wlpc4p1kz7sqqa7ylr8bkdr8l4f34hy4bxj7krpkahwhaqq3"))))
    (arguments
      `(#:cargo-inputs
        (("rust-atty" ,rust-atty)
         ("rust-cast" ,rust-cast)
         ("rust-clap" ,rust-clap)
         ("rust-criterion-plot" ,rust-criterion-plot-0.3)
         ("rust-csv" ,rust-csv)
         ("rust-itertools" ,rust-itertools)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-libc" ,rust-libc)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-rand-core" ,rust-rand-core)
         ("rust-rand-os" ,rust-rand-os)
         ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.1)
         ("rust-rayon" ,rust-rayon)
         ("rust-rayon-core" ,rust-rayon-core)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-tinytemplate" ,rust-tinytemplate)
         ("rust-walkdir" ,rust-walkdir))
        #:cargo-development-inputs
        (("rust-approx" ,rust-approx)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-rand" ,rust-rand)
         ("rust-tempdir" ,rust-tempdir))))))

(define-public rust-criterion-plot
  (package
    (name "rust-criterion-plot")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "criterion-plot" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18kjl0fh2n5ws6ssiqskikmz893dm9rfdgi5j2l2qddyig7cdkgc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder)
        ("rust-cast" ,rust-cast)
        ("rust-itertools" ,rust-itertools))
       #:cargo-development-inputs
       (("rust-itertools-num" ,rust-itertools-num)
        ("rust-num-complex" ,rust-num-complex)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/bheisler/criterion.rs")
    (synopsis "Criterion's plotting library")
    (description "Criterion's plotting library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-criterion-plot-0.3
  (package
    (inherit rust-criterion-plot)
    (name "rust-criterion-plot")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "criterion-plot" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "13pv09z4ryp70qyzablkibwa2mh6c2852qq1sjr9wjigvwnj3ybn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder)
        ("rust-cast" ,rust-cast)
        ("rust-itertools" ,rust-itertools))
       #:cargo-development-inputs
       (("rust-itertools-num" ,rust-itertools-num)
        ("rust-num-complex" ,rust-num-complex)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-crossbeam
  (package
    (name "rust-crossbeam")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g5jysq5x4gndc1v5sq9n3f1m97k7qihwdpigw6ar6knj14qm09d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-crossbeam-channel"
          ,rust-crossbeam-channel)
         ("rust-crossbeam-deque" ,rust-crossbeam-deque)
         ("rust-crossbeam-epoch" ,rust-crossbeam-epoch)
         ("rust-crossbeam-queue" ,rust-crossbeam-queue)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam")
    (synopsis "Tools for concurrent programming")
    (description "Tools for concurrent programming")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-deque
  (package
    (name "rust-crossbeam-deque")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-deque" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0was9x71cz5g1y3670cyy6jdmsdfg6k9mbf0ddz2k1mdd7hx535i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-epoch" ,rust-crossbeam-epoch)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-deque-0.6
  (package
    (inherit rust-crossbeam-deque)
    (name "rust-crossbeam-deque")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-deque" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04rcpgjs6ns57vag8a3dzx26190dhbvy2l0p9n22b9p1yf64pr05"))))
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-epoch" ,rust-crossbeam-epoch)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.6))))))

(define-public rust-crossbeam-deque-0.2
  (package
    (inherit rust-crossbeam-deque)
    (name "rust-crossbeam-deque")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-deque" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wwwbnvxh0rza38xiws8qc46klzhv19zgvarn37pijis6v2zhfgp"))))
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.3)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.4))))))

(define-public rust-crossbeam-epoch
  (package
    (name "rust-crossbeam-epoch")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-epoch" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d408b9x82mdbnb405gw58v5mmdbj2rl28a1h7b9rmn25h8f7j84"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-arrayvec" ,rust-arrayvec)
         ("rust-cfg-if" ,rust-cfg-if)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-memoffset" ,rust-memoffset-0.2)
         ("rust-scopeguard" ,rust-scopeguard-0.3))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description "Epoch-based garbage collection")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-epoch-0.3
  (package
    (inherit rust-crossbeam-epoch)
    (name "rust-crossbeam-epoch")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-epoch" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0l4igvp2i7b6dgaiq040j8kj8hygwdpr6ppzh1hrbsbx83sj2wcj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec)
        ("rust-cfg-if" ,rust-cfg-if)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.2)
        ("rust-lazy-static" ,rust-lazy-static)
        ("rust-memoffset" ,rust-memoffset-0.2)
        ("rust-nodrop" ,rust-nodrop)
        ("rust-rand" ,rust-rand-0.3)
        ("rust-scopeguard" ,rust-scopeguard-0.3))))))

(define-public rust-crossbeam-queue
  (package
    (name "rust-crossbeam-queue")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-queue" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jsa9dbxnwqcxfws09vaschf92d4imlbbikmcn4ka8z7rzb9r5vw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Concurrent queues")
    (description "Concurrent queues")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-utils
  (package
    (name "rust-crossbeam-utils")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p5aa8k3wpsn17md4rx038ac2azm9354knbxdfvn7dd7yk76yc7q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-lazy-static" ,rust-lazy-static))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
      "Utilities for concurrent programming")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-utils-0.2
  (package
    (inherit rust-crossbeam-utils)
    (name "rust-crossbeam-utils")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n8qr52sw9y6yxzyfxi1phh55rsxms7ry4iipdd8vmd16ag8jq17"))))
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if))))))

(define-public rust-csv
  (package
    (name "rust-csv")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "csv" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zgq18xam24rbggm3ybmrygipa0mrr7rscf9r8hmi9vkzp6rql9p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bstr" ,rust-bstr)
         ("rust-csv-core" ,rust-csv-core)
         ("rust-itoa" ,rust-itoa)
         ("rust-ryu" ,rust-ryu)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde" ,rust-serde))))
    (home-page
      "https://github.com/BurntSushi/rust-csv")
    (synopsis
      "Fast CSV parsing with support for serde.")
    (description
      "Fast CSV parsing with support for serde.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-csv-core
  (package
    (name "rust-csv-core")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "csv-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k5zs0x0qmmn27pa5kcg86lg84s29491fw5sh3zswxswnavasp4v"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-memchr" ,rust-memchr))
        #:cargo-development-inputs
        (("rust-arrayvec" ,rust-arrayvec))))
    (home-page
      "https://github.com/BurntSushi/rust-csv")
    (synopsis
      "Bare bones CSV parsing with no_std support.")
    (description
      "Bare bones CSV parsing with no_std support.")
    (license #f)))

(define-public rust-ct-logs
  (package
    (name "rust-ct-logs")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ct-logs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04wiwiv4ghni3x2vni3z711mlz0ndqvh04vmdkbw3nr7zbsqcdjd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-sct" ,rust-sct))))
    (home-page "https://github.com/ctz/ct-logs")
    (synopsis
      "Google's list of Certificate Transparency logs for use with sct crate")
    (description
      "Google's list of Certificate Transparency logs for use with sct crate")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-ctor
  (package
    (name "rust-ctor")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ctor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p7fd2zp3lkb098sn740jlf3np8qg5ivycsc037b4jhqsixf736d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-libc-print" ,rust-libc-print))))
    (home-page
      "https://github.com/mmastrac/rust-ctor")
    (synopsis
      "__attribute__((constructor)) for Rust")
    (description
      "__attribute__((constructor)) for Rust")
    (license (list license:asl2.0 license:expat))))

(define-public rust-custom-derive
  (package
    (name "rust-custom-derive")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "custom_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1f81bavw1wnykwh21hh4yyzigs6zl6f6pkk9p3car8kq95yfb2pg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-rustc-serialize" ,rust-rustc-serialize))))
    (home-page
     "https://github.com/DanielKeep/rust-custom-derive/tree/custom_derive-master")
    (synopsis "Custom derivation macro for Rust")
    (description
     "This crate provides a macro that enables the use of custom @code{derive}
attributes.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-darling
  (package
    (name "rust-darling")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13ia8dx03gy867j3gjqs03zxfnkdz000gysf8lk5bbgg6ajjkriz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-darling-core" ,rust-darling-core)
         ("rust-darling-macro" ,rust-darling-macro))
        #:cargo-development-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page
      "https://github.com/TedDriggs/darling")
    (synopsis
      "A proc-macro library for reading attributes into structs when
      implementing custom derives.
      ")
      (description
        "This package provides a proc-macro library for reading attributes into structs when
        implementing custom derives.
        ")
        (license license:expat)))

(define-public rust-darling-core
  (package
    (name "rust-darling-core")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "119vd2nkgc6phshw4ka9733x9iskvgxds8ks6gr1rd2lxhmm2m7f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-ident-case" ,rust-ident-case)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-strsim" ,rust-strsim)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/TedDriggs/darling")
    (synopsis
      "Helper crate for proc-macro library for reading attributes into structs when
      implementing custom derives. Use https://crates.io/crates/darling in your code.
      ")
      (description
        "Helper crate for proc-macro library for reading attributes into structs when
        implementing custom derives.  Use https://crates.io/crates/darling in your code.
        ")
        (license license:expat)))

(define-public rust-darling-macro
  (package
    (name "rust-darling-macro")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "darling-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hb2bajmf18kgbg6rzvxa78ph7bbsrlnlacq52vi021cwlrf9lqc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-darling-core" ,rust-darling-core)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page
      "https://github.com/TedDriggs/darling")
    (synopsis
      "Internal support for a proc-macro library for reading attributes into structs when
      implementing custom derives. Use https://crates.io/crates/darling in your code.
      ")
      (description
        "Internal support for a proc-macro library for reading attributes into structs when
        implementing custom derives.  Use https://crates.io/crates/darling in your code.
        ")
        (license license:expat)))

(define-public rust-demo-hack
  (package
    (name "rust-demo-hack")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "demo-hack" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m0114p1g0zzrdph5bg03i8m8p70vrwn3whs191jrbjcrmh5lmnp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-demo-hack-impl" ,rust-demo-hack-impl)
         ("rust-proc-macro-hack" ,rust-proc-macro-hack))))
    (home-page
      "https://github.com/dtolnay/proc-macro-hack")
    (synopsis "Demo of proc-macro-hack")
    (description "Demo of proc-macro-hack")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-demo-hack-impl
  (package
    (name "rust-demo-hack-impl")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "demo-hack-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f1fdl60xjas9wlmcl9v6f56vgm3mzwr019kcifav5464rx3w3ld"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro-hack" ,rust-proc-macro-hack)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page
      "https://github.com/dtolnay/proc-macro-hack")
    (synopsis "Demo of proc-macro-hack")
    (description "Demo of proc-macro-hack")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-derivative
  (package
    (name "rust-derivative")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derivative" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fpfcw0if70gnp8hvz6ki2wasldzi31pnwx6jmjq18zpxqqa8b4l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn)
         ("rust-trybuild" ,rust-trybuild))))
    (home-page
      "https://github.com/mcarton/rust-derivative")
    (synopsis
      "A set of alternative `derive` attributes for Rust")
    (description
      "This package provides a set of alternative `derive` attributes for Rust")
    (license #f)))

(define-public rust-derive-builder
  (package
    (name "rust-derive-builder")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derive-builder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rk2ahk10cqhcmzaym154y18ff3qjqyv2hcm5krh2wmpqglq1g6z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-compiletest-rs" ,rust-compiletest-rs)
         ("rust-darling" ,rust-darling)
         ("rust-derive-builder-core"
          ,rust-derive-builder-core)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-skeptic" ,rust-skeptic)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-pretty-assertions"
          ,rust-pretty-assertions)
         ("rust-skeptic" ,rust-skeptic))))
    (home-page
      "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
      "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (description
      "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license #f)))

(define-public rust-derive-builder-0.5
  (package
    (inherit rust-derive-builder)
    (name "rust-derive-builder")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derive-builder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fgl8dsigr7h70clxjq8xmsfc021w5ag262wfgcqv0ian1m8x6cc"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-compiletest-rs" ,rust-compiletest-rs)
         ("rust-derive-builder-core" ,rust-derive-builder-core-0.2)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-quote" ,rust-quote)
         ("rust-skeptic" ,rust-skeptic)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-pretty-assertions"
          ,rust-pretty-assertions)
         ("rust-skeptic" ,rust-skeptic))))))

(define-public rust-derive-builder-core
  (package
    (name "rust-derive-builder-core")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derive-builder-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1af5fkgswbxyyhy39wjqzi16ss90j05kdck072zs3p8gb3za8s4b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-darling" ,rust-darling)
         ("rust-log" ,rust-log)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-pretty-assertions" ,rust-pretty-assertions-0.5))))
    (home-page
      "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
      "Internal helper library for the derive_builder crate.")
    (description
      "Internal helper library for the derive_builder crate.")
    (license #f)))

(define-public rust-derive-builder-core-0.2
  (package
    (inherit rust-derive-builder-core)
    (name "rust-derive-builder-core")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derive-builder-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mxpl1ja3l60w1v5vr3733hr5mcpds2hfl6shrmy3a2zkvp28pkk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-log-0.3)
         ("rust-quote" ,rust-quote-0.3)
         ("rust-syn" ,rust-syn-0.11))
        #:cargo-development-inputs
        (("rust-pretty-assertions"
          ,rust-pretty-assertions-0.2))))))

(define-public rust-derive-new
  (package
    (name "rust-derive-new")
    (version "0.5.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "derive-new" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ncibp4jhpkym7namg3viqyw8hljd32n6abg64af8qjwrn91iwvi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/nrc/derive-new")
    (synopsis
      "`#[derive(new)]` implements simple constructor functions for structs and enums.")
    (description
      "`#[derive(new)]` implements simple constructor functions for structs and enums.")
    (license license:expat)))

(define-public rust-diff
  (package
    (name "rust-diff")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diff" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fhavni46a2rib93ig5fgbqmm48ysms5sxzb3h9bp7vp2bwnjarw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-speculate" ,rust-speculate))))
    (home-page
      "https://github.com/utkarshkukreti/diff.rs")
    (synopsis
      "An LCS based slice and string diffing implementation.")
    (description
      "An LCS based slice and string diffing implementation.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-difference
  (package
    (name "rust-difference")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "difference" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1621wx4k8h452p6xzmzzvm7mz87kxh4yqz0kzxfjj9xmjxlbyk2j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getopts" ,rust-getopts))
        #:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-term" ,rust-term-0.5))))
    (home-page
      "https://github.com/johannhof/difference.rs")
    (synopsis
      "A Rust text diffing and assertion library.")
    (description
      "This package provides a Rust text diffing and assertion library.")
    (license license:expat)))

(define-public rust-difference-1
  (package
    (inherit rust-difference)
    (name "rust-difference")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "difference" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a5v0b73z7vywbclll32wjsfkdgh6wn9prnq91z0d3lag4clsc5k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getopts" ,rust-getopts))
        #:cargo-development-inputs
        (("rust-term" ,rust-term))))))

(define-public rust-dirs-2
  (package
    (inherit rust-dirs)
    (name "rust-dirs")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qymhyq7w7wlf1dirq6gsnabdyzg6yi2yyxkx6c4ldlkbjdaibhk"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-dirs-sys" ,rust-dirs-sys))))))

(define-public rust-dirs-sys
  (package
    (name "rust-dirs-sys")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yyykdcmbc476z1v9m4z5jb8y91dw6kgzpkiqi2ig07xx0yv585g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-libc" ,rust-libc)
         ("rust-redox-users" ,rust-redox-users)
         ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/soc/dirs-sys-rs")
    (synopsis
      "System-level helper functions for the dirs and directories crates.")
    (description
      "System-level helper functions for the dirs and directories crates.")
    (license #f)))

(define-public rust-docopt
  (package
    (name "rust-docopt")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "docopt" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s9rcpmnnivs502q69lc1h1wrwapkq09ikgbfbgqf31idmc5llkz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex)
         ("rust-serde" ,rust-serde)
         ("rust-strsim" ,rust-strsim))))
    (home-page "https://github.com/docopt/docopt.rs")
    (synopsis "Command line argument parsing.")
    (description "Command line argument parsing.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-docopt-0.8
  (package
    (inherit rust-docopt)
    (name "rust-docopt")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "docopt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jha611mffc2qnxvdl3pmglz07akl99lk1vihhb3nl1cd69x7b6q"))))
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex-0.2)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-strsim" ,rust-strsim-0.6))))))

(define-public rust-docopt-0.7
  (package
    (inherit rust-docopt)
    (name "rust-docopt")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "docopt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1n6gbhsks2w9y0b4bwqyawh4ghbkka09w6pjcrq9i1sd51pflcmb"))))
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static-0.2)
         ("rust-regex" ,rust-regex-0.2)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-strsim" ,rust-strsim-0.6))))))

(define-public rust-duct
  (package
    (name "rust-duct")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "duct" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1vm1nzyi434h2zwix7c925qfa886ri1qx4nkq4hdrgkq7h9ayh1n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazycell" ,rust-lazycell)
         ("rust-libc" ,rust-libc)
         ("rust-os-pipe" ,rust-os-pipe)
         ("rust-shared-child" ,rust-shared-child))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://github.com/oconnor663/duct.rs")
    (synopsis
      "a library for creating shell pipelines")
    (description
      "a library for creating shell pipelines")
    (license license:expat)))

(define-public rust-either
  (package
    (name "rust-either")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yyggfd5yq9hyyp0bd5jj0fgz3rwws42d19ri0znxwwqs3hcy9sm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-serde" ,rust-serde))))
    (home-page "https://github.com/bluss/either")
    (synopsis
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-encode-unicode
  (package
    (name "rust-encode-unicode")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encode_unicode" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1g8a8pixkxz6r927f4sc4r15qyc0szxdxb1732v8q7h0di4wkclh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ascii" ,rust-ascii)
         ("rust-clippy" ,rust-clippy))
        #:cargo-development-inputs
        (("rust-lazy-static" ,rust-lazy-static-1.0))))
    (home-page
      "https://github.com/tormol/encode_unicode")
    (synopsis
      "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.")
    (description
      "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-encoding
  (package
    (name "rust-encoding")
    (version "0.2.33")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v1ndmkarh9z3n5hk53da4z56hgk9wa5kcsm7cnx345raqw983bb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-encoding-index-japanese"
          ,rust-encoding-index-japanese)
         ("rust-encoding-index-korean"
          ,rust-encoding-index-korean)
         ("rust-encoding-index-simpchinese"
          ,rust-encoding-index-simpchinese)
         ("rust-encoding-index-singlebyte"
          ,rust-encoding-index-singlebyte)
         ("rust-encoding-index-tradchinese"
          ,rust-encoding-index-tradchinese))
        #:cargo-development-inputs
        (("rust-getopts" ,rust-getopts))))
    (home-page
      "https://github.com/lifthrasiir/rust-encoding")
    (synopsis "Character encoding support for Rust")
    (description
      "Character encoding support for Rust")
    (license license:expat)))

(define-public rust-encoding-index-japanese
  (package
    (name "rust-encoding-index-japanese")
    (version "1.20141219.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_index_japanese" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "148c1lmd640p1d7fzk0nv7892mbyavvwddgqvcsm78798bzv5s04"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-encoding-index-tests"
          ,rust-encoding-index-tests))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((".*../tests.*") ""))
             #t)))))
    (home-page
      "https://github.com/lifthrasiir/rust-encoding")
    (synopsis "Index tables for Japanese character encodings")
    (description
      "Index tables for Japanese character encodings")
    (license license:cc0)))

(define-public rust-encoding-index-korean
  (package
    (name "rust-encoding-index-korean")
    (version "1.20141219.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_index_korean" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10cxabp5ppygbq4y6y680856zl9zjvq7ahpiw8zj3fmwwsw3zhsd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-encoding-index-tests"
          ,rust-encoding-index-tests))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((".*../tests.*") ""))
             #t)))))
    (home-page
      "https://github.com/lifthrasiir/rust-encoding")
    (synopsis "Index tables for Korean character encodings")
    (description
      "Index tables for Korean character encodings")
    (license license:cc0)))

(define-public rust-encoding-index-simpchinese
  (package
    (name "rust-encoding-index-simpchinese")
    (version "1.20141219.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_index_simpchinese" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xria2i7mc5dqdrpqxasdbxv1qx46jjbm53if3y1i4cvj2a72ynq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-encoding-index-tests"
          ,rust-encoding-index-tests))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((".*../tests.*") ""))
             #t)))))
    (home-page
      "https://github.com/lifthrasiir/rust-encoding")
    (synopsis "Index tables for simple Chienese character encodings")
    (description
      "Index tables for simple Chienese character encodings")
    (license license:cc0)))

(define-public rust-encoding-index-singlebyte
  (package
    (name "rust-encoding-index-singlebyte")
    (version "1.20141219.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_index_singlebyte" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jp85bz2pprzvg9m95w4q0vibh67b6w3bx35lafay95jzyndal9k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-encoding-index-tests"
          ,rust-encoding-index-tests))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((".*../tests.*") ""))
             #t)))))
    (home-page
      "https://github.com/lifthrasiir/rust-encoding")
    (synopsis "Index tables for various single-byte character encodings")
    (description
      "Index tables for various single-byte character encodings")
    (license license:cc0)))

(define-public rust-encoding-index-tests
  (package
    (name "rust-encoding-index-tests")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_index_tests" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s85y091gl17ixass49bzaivng7w8p82p6nyvz2r3my9w4mxhim2"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/lifthrasiir/rust-encoding")
    (synopsis "Helper macros used to test index tables for character encodings")
    (description
      "Helper macros used to test index tables for character encodings.")
    (license license:cc0)))

(define-public rust-encoding-index-tradchinese
  (package
    (name "rust-encoding-index-tradchinese")
    (version "1.20141219.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_index_tradchinese" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "060ci4iz6xfvzk38syfbjvs7pix5hch3mvxkksswmqwcd3aj03px"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-encoding-index-tests"
          ,rust-encoding-index-tests))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((".*../tests.*") ""))
             #t)))))
    (home-page "https://github.com/lifthrasiir/rust-encoding")
    (synopsis "Index tables for traditional Chienese character encodings")
    (description
      "Index tables for traditional Chienese character encodings")
    (license license:cc0)))

(define-public rust-encoding-rs
  (package
    (name "rust-encoding-rs")
    (version "0.8.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "encoding_rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v902qqnbd37vdq4rjvp6k05wmghrasfdcjy30gp1xpjg5f7hma1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-packed-simd" ,rust-packed-simd)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://docs.rs/encoding_rs/")
    (synopsis
      "A Gecko-oriented implementation of the Encoding Standard")
    (description
      "This package provides a Gecko-oriented implementation of the Encoding Standard")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-enum-as-inner
  (package
    (name "rust-enum-as-inner")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "enum-as-inner" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0zg3h7k3g1z7a9ayqy63sk302d4dg5g2h274ddv80mj4jxn2cn1x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/bluejekyll/enum-as-inner")
    (synopsis "A deriving proc-macro for generating functions to automatically give access to the inner members of enum.")
    (description "A deriving proc-macro for generating functions to automatically give access to the inner members of enum.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-env-logger
  (package
    (name "rust-env-logger")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1lx2s5nk96xx4i3m4zc4ghqgi8kb07dsnyiv8jk2clhax42dxz5a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-atty" ,rust-atty)
         ("rust-humantime" ,rust-humantime)
         ("rust-log" ,rust-log)
         ("rust-regex" ,rust-regex)
         ("rust-termcolor" ,rust-termcolor))))
    (home-page
      "https://github.com/sebasmagri/env_logger/")
    (synopsis
      "A logging implementation for `log` which is configured via an environment variable.")
    (description
      "This package provides a logging implementation for `log` which is configured via an environment variable.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-env-logger-0.5
  (package
    (inherit rust-env-logger)
    (name "rust-env-logger")
    (version "0.5.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env_logger" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0f0c4i4c65jh8lci0afl5yg74ac0lbnpxcp81chj114zwg9a9c0m"))))
    (arguments
     `(#:cargo-inputs
       (("rust-atty" ,rust-atty)
        ("rust-humantime" ,rust-humantime)
        ("rust-log" ,rust-log)
        ("rust-regex" ,rust-regex)
        ("rust-termcolor" ,rust-termcolor))))))

(define-public rust-env-logger-0.4
  (package
    (inherit rust-env-logger)
    (name "rust-env-logger")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env_logger" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nydz2lidsvx9gs0v2zcz68rzqx8in7fzmiprgsrhqh17vkj3prx"))))
   (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log)
       ("rust-regex" ,rust-regex-0.2))))))

(define-public rust-env-logger-0.3
  (package
    (inherit rust-env-logger)
    (name "rust-env-logger")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "env_logger" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bvcjgkw4s3k1rd7glpflgc8s9a393zjd6jfdgvs8gjvwj0dgaqm"))))
   (arguments
    `(#:cargo-inputs
      (("rust-log" ,rust-log)
       ("rust-regex" ,rust-regex-0.1))))))

(define-public rust-envmnt
  (package
    (name "rust-envmnt")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "envmnt" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12zkq3p999bypyxmjnpiqw9r3hmifb3bcikd7j3as1fdcbq01fyl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-indexmap" ,rust-indexmap))))
    (home-page
      "https://github.com/sagiegurari/envmnt")
    (synopsis
      "Environment variables utility functions.")
    (description
      "Environment variables utility functions.")
    (license license:asl2.0)))

(define-public rust-erased-serde
  (package
    (name "rust-erased-serde")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "erased-serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0q7bnxs5zskfq5iillig55g7891dllcxh2p8y8k1p2j72syf9viv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-cbor" ,rust-serde-cbor)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/dtolnay/erased-serde")
    (synopsis
      "Type-erased Serialize and Serializer traits")
    (description
      "Type-erased Serialize and Serializer traits")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-error-chain
  (package
    (name "rust-error-chain")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-chain" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ndpw1ny2kxqpw6k1shq8k56z4vfpk4xz9zr8ay988k0rffrxd1s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace)
        ("rust-version-check" ,rust-version-check-0.1))
       #:cargo-development-inputs
       (("rust-version-check" ,rust-version-check-0.1))
       #:tests? #f)) ; has_backtrace_depending_on_env fails
    (home-page "https://github.com/rust-lang-nursery/error-chain")
    (synopsis
      "Yet another error boilerplate library.")
    (description
      "Yet another error boilerplate library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-error-chain-0.11
  (package
    (inherit rust-error-chain)
    (name "rust-error-chain")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "error-chain" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1wykkr0naizbkwxjwia1rch8xhwvgij9khqvjzs07mrmqifislgz"))))
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace))
       #:tests? #f)))) ; has_backtrace_depending_on_env fails

(define-public rust-expectest
  (package
    (name "rust-expectest")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "expectest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00dv47irmsyq7brzjhz4xns3p722gm98zp39h9hq2mrzd5marpgq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-num-traits" ,rust-num-traits))))
    (home-page
      "https://github.com/zummenix/expectest")
    (synopsis
      "Crate provides matchers and matcher functions for unit testing.")
    (description
      "Crate provides matchers and matcher functions for unit testing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-expectest-0.9
  (package
    (inherit rust-expectest)
    (name "rust-expectest")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "expectest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0f24q2a53x7sfmmrqjbwbk7pahzwkpd829fcr023kb7q5xnd6z4g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-num-traits" ,rust-num-traits-0.1))))))

(define-public rust-failure
  (package
    (name "rust-failure")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "failure" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qppmgv4i5jj6vrss91qackqnl0a12h7lnby4l7j5fdy78yxhnvr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-backtrace" ,rust-backtrace)
         ("rust-failure-derive" ,rust-failure-derive))))
    (home-page
      "https://rust-lang-nursery.github.io/failure/")
    (synopsis
      "Experimental error handling abstraction.")
    (description
      "Experimental error handling abstraction.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-failure-derive
  (package
    (name "rust-failure-derive")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "failure_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q97n7dp51j5hndzic9ng2fgn6f3z5ya1992w84l7vypby8n647a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn)
         ("rust-synstructure" ,rust-synstructure-0.10))
        #:cargo-development-inputs
        (("rust-failure" ,rust-failure))))
    (home-page
      "https://rust-lang-nursery.github.io/failure/")
    (synopsis "derives for the failure crate")
    (description "derives for the failure crate")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-filetime-0.1
  (package
    (inherit rust-filetime)
    (name "rust-filetime")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "filetime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03xishfxzpr4nfz4g3r218d6b6g94rxsqw9pw96m6wa8wgrm6iki"))))
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-libc" ,rust-libc)
         ("rust-redox-syscall" ,rust-redox-syscall))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))))

(define-public rust-findshlibs-0.4
  (package
    (inherit rust-findshlibs)
    (name "rust-findshlibs")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "findshlibs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "045csyaxhygdiwsr21mqcd9m4c3r270xg3vrv6rssaz5nzwmhzrg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-lazy-static" ,rust-lazy-static)
        ("rust-libc" ,rust-libc)
        ("rust-bindgen" ,rust-bindgen-0.39))
       #:cargo-development-inputs
       (("rust-bindgen" ,rust-bindgen-0.39)
        ("rust-cfg-if" ,rust-cfg-if))))))

(define-public rust-rustfix
  (package
    (name "rust-rustfix")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustfix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01zn0ysnass3mmrhxk90584y713vjfq1x97mi4saac99g9vsql3i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-failure" ,rust-failure)
         ("rust-log" ,rust-log)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json))
        #:cargo-development-inputs
        (("rust-difference" ,rust-difference)
         ("rust-duct" ,rust-duct)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-proptest" ,rust-proptest)
         ("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://github.com/rust-lang-nursery/rustfix")
    (synopsis
      "Automatically apply the suggestions made by rustc")
    (description
      "Automatically apply the suggestions made by rustc")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustversion
  (package
    (name "rust-rustversion")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustversion" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s3ib2paa5gq17x4qsmjmnsw68z7b5d5av1wsiqcrihmqb7kk0dl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/rustversion")
    (synopsis
      "Conditional compilation according to rustc compiler version")
    (description
      "Conditional compilation according to rustc compiler version")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustversion-0.1.3
  (package
    (inherit rust-rustversion)
    (name "rust-rustversion")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustversion" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "158mxvmn1kxxskvb3l4sj4abkcs315byli3i52rr831424ljsxbl"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2) ; 0.4
        ("rust-quote" ,rust-quote) ;0.6
        ("rust-syn" ,rust-syn)))))) ; 0.15.25

(define-public rust-flame
  (package
    (name "rust-flame")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flame" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0c5bmhyimzxch3pmh0w3z9n57saasgix4bmbbksr9vp1c5j71hhz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static)
        ("rust-thread-id" ,rust-thread-id)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive)
        ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/TyOverby/flame")
    (synopsis "A cool flamegraph library for rust")
    (description
     "Flamegraphs are a great way to view profiling information.  At a glance, they give you information about how much time your program spends in critical sections of your code giving you some much-needed insight into where optimizations may be needed.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-flamer
  (package
    (name "rust-flamer")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flamer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1avszq3fn4ix7p6wjfdkli6fjyxccks1qhzja92a6kpxakd35drn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-flame" ,rust-flame)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/llogiq/flamer")
    (synopsis "A proc macro to insert appropriate flame::start_guard(_) calls (for use with flame)")
    (description
      "A proc macro to insert appropriate flame::start_guard(_) calls (for use with flame)")
    (license license:asl2.0)))

(define-public rust-flate2
  (package
    (name "rust-flate2")
    (version "1.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "flate2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10j6bpgpipywmrsxxmp1q48qd9vp1c4fs64y2hv02r48cfxgznia"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crc32fast" ,rust-crc32fast)
         ("rust-futures" ,rust-futures)
         ("rust-libc" ,rust-libc)
         ("rust-libz-sys" ,rust-libz-sys)
         ("rust-miniz-sys" ,rust-miniz-sys)
         ("rust-miniz-oxide" ,rust-miniz-oxide)
         ("rust-miniz-oxide" ,rust-miniz-oxide)
         ("rust-tokio-io" ,rust-tokio-io))
        #:cargo-development-inputs
        (("rust-futures" ,rust-futures)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-rand" ,rust-rand)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-tokio-threadpool" ,rust-tokio-threadpool))))
    (home-page
      "https://github.com/alexcrichton/flate2-rs")
    (synopsis
      "Bindings to miniz.c for DEFLATE compression and decompression exposed as
      Reader/Writer streams. Contains bindings for zlib, deflate, and gzip-based
      streams.")
    (description
      "Bindings to miniz.c for DEFLATE compression and decompression exposed as
      Reader/Writer streams.  Contains bindings for zlib, deflate, and gzip-based
      streams.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-float-ord
  (package
    (name "rust-float-ord")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "float-ord" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kin50365sr3spnbscq43lksymybi99ai9rkqdw90m6vixhlibbv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/notriddle/rust-float-ord")
    (synopsis
      "A total ordering for floating-point numbers")
    (description
      "This package provides a total ordering for floating-point numbers")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-foreign-types
  (package
    (name "rust-foreign-types")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ca4i38yrf9iy5k47lr1ylb3rvcbn36d81k5pr5kzf6kmj6p111n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-foreign-types-macros" ,rust-foreign-types-macros)
         ("rust-foreign-types-shared" ,rust-foreign-types-shared))))
    (home-page
      "https://github.com/sfackler/foreign-types")
    (synopsis
      "A framework for Rust wrappers over C APIs.")
    (description
      "A framework for Rust wrappers over C APIs.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-foreign-types-macros
  (package
    (name "rust-foreign-types-macros")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16yjigjcsklcwy2ad32l24k1nwm9n3bsnyhxc3z9whjbsrj60qk6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/sfackler/foreign-types")
    (synopsis "An internal crate used by foreign-types")
    (description
      "An internal crate used by foreign-types.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fs2
  (package
    (name "rust-fs2")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04v2hwk7035c088f19mfl5b1lz84gnvv2hv6m935n0hmirszqr4m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))
        #:tests? #f)) ; Tests require unstable features.
    (home-page "https://github.com/danburkert/fs2-rs")
    (synopsis
      "Cross-platform file locks and file duplication.")
    (description
      "Cross-platform file locks and file duplication.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fs2-0.2
  (package
    (inherit rust-fs2)
    (name "rust-fs2")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vsih93cvds3x6f3w9bc5rnkyv8haix1px4jpcqvjyd9l7ji9m5w"))))
    (arguments
      `(#:cargo-inputs
        (("rust-kernel32-sys" ,rust-kernel32-sys)
         ("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi-0.2))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))
        #:tests? #f)))) ; Tests require unstable features.

(define-public rust-fst
  (package
    (name "rust-fst")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fst" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mpby7wa5mkpgjiilam94a2l9mxx9wpgs3nw2nr1a0czzwsb8zwj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-0.5)
         ("rust-memmap" ,rust-memmap-0.6))
        #:cargo-development-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-fst-levenshtein" ,rust-fst-levenshtein)
         ("rust-fst-regex" ,rust-fst-regex)
         ("rust-lazy-static" ,rust-lazy-static-0.2)
         ("rust-quickcheck" ,rust-quickcheck-0.7)
         ("rust-rand" ,rust-rand))
        #:tests? #f)) ; TODO: Fix tests.
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Use finite state transducers to compactly represents sets or maps of many strings (> 1 billion is possible).")
    (description
      "Use finite state transducers to compactly represents sets or maps of many strings (> 1 billion is possible). ")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-fst-0.1
  (package
    (inherit rust-fst)
    (name "rust-fst")
    (version "0.1.38")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fst" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15hm5ijqbb9a67zpz79i9sbk3f1cfp71rgqfqfffl3kgbs54crs6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-0.5)
        ("rust-memmap" ,rust-memmap-0.4)
        ("rust-regex-syntax" ,rust-regex-syntax-0.3)
        ("rust-utf8-ranges" ,rust-utf8-ranges-0.1))
       #:cargo-development-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-lazy-static" ,rust-lazy-static-0.1)
        ("rust-quickcheck" ,rust-quickcheck-0.2)
        ("rust-rand" ,rust-rand))
       #:tests? #f)))) ; Tests require 'unicode' create.

(define-public rust-fst-levenshtein
  (package
    (name "rust-fst-levenshtein")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fst-levenshtein" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s5ml10442bbnpmilmwjh4pfixsj6837rg68vjzg63i3djd4524y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fst" ,rust-fst)
         ("rust-utf8-ranges" ,rust-utf8-ranges))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Search finite state transducers with fuzzy queries using Levenshtein automata.")
    (description
      "Search finite state transducers with fuzzy queries using Levenshtein automata.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-fst-regex
  (package
    (name "rust-fst-regex")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fst-regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "126xrv3s8mrq8nqsahmpy0nlks6l3wlivqyf6a0i4g7d3vcs3b47"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fst" ,rust-fst)
         ("rust-regex-syntax" ,rust-regex-syntax-0.3)
         ("rust-utf8-ranges" ,rust-utf8-ranges))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Search finite state transducers with regular expression.")
    (description
      "Search finite state transducers with regular expression.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-futf
  (package
    (name "rust-futf")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fxc18bnabird5jl941nsd6d25vq8cn8barmz4d30dlkzbiir73w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-mac" ,rust-mac)
         ("rust-new-debug-unreachable"
          ,rust-new-debug-unreachable))
        #:tests? #f ; Requires unstable features.
        ))
    (home-page "https://github.com/servo/futf")
    (synopsis "Handling fragments of UTF-8")
    (description "Handling fragments of UTF-8")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-channel-preview
  (package
    (name "rust-futures-channel-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-channel-preview" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1blgpikhw391lzrfqcgg4xsn5xc0dlybni77ka7f0vb08zaixir1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-core-preview"
         ,rust-futures-core-preview)
        ("rust-futures-sink-preview"
         ,rust-futures-sink-preview))))
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis "Channels for asynchronous communication using futures-rs")
    (description
     "This package provides channels for asynchronous communication using
futures-rs.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-executor-preview
  (package
    (name "rust-futures-executor-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-executor-preview" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "053g5kf2qa1xhdkwp3d1grrizzy4683mpbb3y0vvm00hwl7jdfl7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures-channel-preview"
          ,rust-futures-channel-preview)
         ("rust-futures-core-preview"
          ,rust-futures-core-preview)
         ("rust-futures-util-preview"
          ,rust-futures-util-preview)
         ("rust-num-cpus" ,rust-num-cpus))))
    (home-page
      "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis
      "Executors for asynchronous tasks based on the futures-rs library.")
    (description
      "Executors for asynchronous tasks based on the futures-rs library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-preview
  (package
    (name "rust-futures-preview")
    (version "0.3.0-alpha.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-preview" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dg4qijba037xqykminifxpnjasabcjx9pwa3ww8wcmj9w6gka7g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures-channel-preview"
          ,rust-futures-channel-preview)
         ("rust-futures-core-preview"
          ,rust-futures-core-preview)
         ("rust-futures-executor-preview"
          ,rust-futures-executor-preview)
         ("rust-futures-io-preview"
          ,rust-futures-io-preview)
         ("rust-futures-sink-preview"
          ,rust-futures-sink-preview)
         ("rust-futures-util-preview"
          ,rust-futures-util-preview))))
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis
      "An implementation of futures and streams featuring zero allocations, composability, and iterator-like interfaces.")
    (description
      "An implementation of futures and streams featuring zero allocations, composability, and iterator-like interfaces.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-select-macro-preview
  (package
    (name "rust-futures-select-macro-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri
               "futures-select-macro-preview"
               version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a90ivjzkgz7msiz5si05xzi8xwsk5gar1gkrbmrgqpgkliqd7a6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro-hack" ,rust-proc-macro-hack)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page
      "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis
      "The `select!` macro for waiting on multiple different `Future`s at once and handling the first one to complete.")
    (description
      "The `select!` macro for waiting on multiple different `Future`s at once and handling the first one to complete.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-timer
  (package
    (name "rust-futures-timer")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-timer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l35r7nm8p43j0adkhybnwxzbjiqy0b00kgccjy3l513m9abb7lg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures-preview" ,rust-futures-preview)
         ("rust-pin-utils" ,rust-pin-utils))
        #:cargo-development-inputs
        (("rust-runtime" ,rust-runtime))))
    (home-page "https://github.com/rustasync/futures-timer")
    (synopsis
      "Timeouts and intervals for futures.")
    (description
      "Timeouts and intervals for futures.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-util-preview
  (package
    (name "rust-futures-util-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-util-preview" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kizm86wgr5qldyavskfi0r1msg6m4x2pkj0d4r04br2ig29i0dg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-futures-channel-preview"
          ,rust-futures-channel-preview)
         ("rust-futures-core-preview"
          ,rust-futures-core-preview)
         ("rust-futures-io-preview"
          ,rust-futures-io-preview)
         ("rust-futures-select-macro-preview"
          ,rust-futures-select-macro-preview)
         ("rust-futures-sink-preview"
          ,rust-futures-sink-preview)
         ("rust-memchr" ,rust-memchr)
         ("rust-pin-utils" ,rust-pin-utils)
         ("rust-proc-macro-hack" ,rust-proc-macro-hack)
         ("rust-proc-macro-nested"
          ,rust-proc-macro-nested)
         ("rust-rand" ,rust-rand)
         ("rust-slab" ,rust-slab)
         ("rust-tokio-io" ,rust-tokio-io))))
    (home-page
      "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis
      "Common utilities and extension traits for the futures-rs library.")
    (description
      "Common utilities and extension traits for the futures-rs library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fxhash
  (package
    (name "rust-fxhash")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fxhash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "037mb9ichariqi45xm6mz0b11pa92gj38ba0409z3iz239sns6y3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder))
        #:cargo-development-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-seahash" ,rust-seahash))))
    (home-page "https://github.com/cbreeden/fxhash")
    (synopsis
      "A fast, non-secure, hashing algorithm derived from an internal hasher used in FireFox and Rustc.")
    (description
      "This package provides a fast, non-secure, hashing algorithm derived from an internal hasher used in FireFox and Rustc.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-generic-array
  (package
    (name "rust-generic-array")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generic-array" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kddwxpd58y807y1r3lijg7sw3gxm6nczl6wp57gamhv6mhygl8f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-typenum" ,rust-typenum))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/fizyk20/generic-array")
    (synopsis
      "Generic types implementing functionality of arrays")
    (description
      "Generic types implementing functionality of arrays")
    (license license:expat)))

(define-public rust-genmesh
  (package
    (name "rust-genmesh")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "genmesh" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17qybydyblf3hjiw7mq181jpi4vrbb8dmsj0wi347r8k0m354g89"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cgmath" ,rust-cgmath-0.16)
         ("rust-mint" ,rust-mint))))
    (home-page "https://github.com/gfx-rs/genmesh")
    (synopsis "A package for generating 3D meshes")
    (description
      "This package provides a package for generating 3D meshes")
    (license license:asl2.0)))

(define-public rust-getrandom
  (package
    (name "rust-getrandom")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getrandom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0macrjfkgsjn6ikr94agapp4fkxmr8w7y2g7qis4icc4a17cwp76"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-libc" ,rust-libc)
         ("rust-log" ,rust-log)
         ("rust-stdweb" ,rust-stdweb)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen))))
    (home-page
      "https://github.com/rust-random/getrandom")
    (synopsis
      "A small cross-platform library for retrieving random data from system source")
    (description
      "This package provides a small cross-platform library for retrieving random data from system source")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-gif
  (package
    (name "rust-gif")
    (version "0.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gif" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bw174f7civdfgryvc8pvyhicpr96hzdajnda4s3y8iv3ch907a7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-color-quant" ,rust-color-quant)
         ("rust-libc" ,rust-libc)
         ("rust-lzw" ,rust-lzw))
        #:cargo-development-inputs
        (("rust-glob" ,rust-glob))))
    (home-page
      "https://github.com/image-rs/image-gif")
    (synopsis "GIF de- and encoder")
    (description "GIF de- and encoder")
    (license #f)))

(define-public rust-git2
  (package
    (name "rust-git2")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "git2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09lj6i26yial0drdbmfh36avz6wizaxqb0k41sqn2kca1qv01d4c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-libc" ,rust-libc)
         ("rust-libgit2-sys" ,rust-libgit2-sys)
         ("rust-log" ,rust-log)
         ("rust-openssl-probe" ,rust-openssl-probe)
         ("rust-openssl-sys" ,rust-openssl-sys)
         ("rust-url" ,rust-url))
        #:cargo-development-inputs
        (("rust-docopt" ,rust-docopt)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-tempdir" ,rust-tempdir)
         ("rust-thread-id" ,rust-thread-id)
         ("rust-time" ,rust-time))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
             #t)))))
    (inputs
     `(("libgit2" ,libgit2)
       ("openssl" ,openssl)))
    (home-page
      "https://github.com/rust-lang/git2-rs")
    (synopsis
      "Bindings to libgit2 for interoperating with git repositories. This library is
      both threadsafe and memory safe and allows both reading and writing git
      repositories.")
    (description
      "Bindings to libgit2 for interoperating with git repositories.  This library is
      both threadsafe and memory safe and allows both reading and writing git
      repositories.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-git2-0.8
  (package
    (inherit rust-git2)
    (name "rust-git2")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "git2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1c0a16k6gwlpmy901f9z8ndli3qzs5h1aca468i00jm1pwlr6cy7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-libc" ,rust-libc)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.7)
        ("rust-log" ,rust-log)
        ("rust-openssl-probe" ,rust-openssl-probe)
        ("rust-openssl-sys" ,rust-openssl-sys)
        ("rust-url" ,rust-url-1))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt)
        ("rust-openssl-src" ,rust-openssl-src)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive)
        ("rust-tempdir" ,rust-tempdir)
        ("rust-thread-id" ,rust-thread-id)
        ("rust-time" ,rust-time))
        #:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
             #t)))))))

(define-public rust-gimli
  (package
    (name "rust-gimli")
    (version "0.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gimli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "006dpaa63y01wb58nvs2hhj3qqx52yxg20njjflr0frfbyp1hb8n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-arrayvec" ,rust-arrayvec)
         ("rust-byteorder" ,rust-byteorder)
         ("rust-fallible-iterator"
          ,rust-fallible-iterator)
         ("rust-indexmap" ,rust-indexmap)
         ("rust-stable-deref-trait"
          ,rust-stable-deref-trait))
        #:cargo-development-inputs
        (("rust-crossbeam" ,rust-crossbeam)
         ("rust-getopts" ,rust-getopts)
         ("rust-memmap" ,rust-memmap)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-object" ,rust-object)
         ("rust-rayon" ,rust-rayon)
         ("rust-regex" ,rust-regex)
         ("rust-test-assembler" ,rust-test-assembler)
         ("rust-typed-arena" ,rust-typed-arena))))
    (home-page "https://github.com/gimli-rs/gimli")
    (synopsis
      "A library for reading and writing the DWARF debugging format.")
    (description
      "This package provides a library for reading and writing the DWARF debugging format.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-gimli-0.18
  (package
    (inherit rust-gimli)
    (name "rust-gimli")
    (version "0.18.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gimli" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ma1zg2klqr47rasm7jn3zzd1j1pj2a8wkfbv5zsx10qh43phy4k"))))))

(define-public rust-glium
  (package
    (name "rust-glium")
    (version "0.26.0-alpha3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glium" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gsvz19cnfaqygyqmvskl2qz3ykr7mjiab7478awpw77vhpn3vkb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-backtrace" ,rust-backtrace)
         ("rust-fnv" ,rust-fnv)
         ("rust-glutin" ,rust-glutin)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-smallvec" ,rust-smallvec)
         ("rust-takeable-option" ,rust-takeable-option))
        #:cargo-development-inputs
        (("rust-cgmath" ,rust-cgmath)
         ("rust-genmesh" ,rust-genmesh)
         ("rust-gl-generator" ,rust-gl-generator)
         ("rust-image" ,rust-image)
         ("rust-obj" ,rust-obj)
         ("rust-rand" ,rust-rand)
         ("rust-rental" ,rust-rental))))
    (home-page "https://github.com/glium/glium")
    (synopsis
      "Elegant and safe OpenGL wrapper.

      Glium is an intermediate layer between OpenGL and your application. You still need to manually handle
      the graphics pipeline, but without having to use OpenGL's old and error-prone API.

      Its objectives:

      - Be safe to use. Many aspects of OpenGL that can trigger a crash if misused are automatically handled by glium.
      - Provide an API that enforces good pratices such as RAII or stateless function calls.
      - Be compatible with all OpenGL versions that support shaders, providing unified API when things diverge.
      - Avoid all OpenGL errors beforehand.
      - Produce optimized OpenGL function calls, and allow the user to easily use modern OpenGL techniques.
      ")
      (description
        "Elegant and safe OpenGL wrapper.

        Glium is an intermediate layer between OpenGL and your application.  You still need to manually handle
        the graphics pipeline, but without having to use OpenGL's old and error-prone API.

        Its objectives:

        - Be safe to use.  Many aspects of OpenGL that can trigger a crash if misused are automatically handled by glium.
        - Provide an API that enforces good pratices such as RAII or stateless function calls.
        - Be compatible with all OpenGL versions that support shaders, providing unified API when things diverge.
        - Avoid all OpenGL errors beforehand.
        - Produce optimized OpenGL function calls, and allow the user to easily use modern OpenGL techniques.
        ")
        (license license:asl2.0)))

(define-public rust-gl-generator
  (package
    (name "rust-gl-generator")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gl-generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gdchvay0k0g931b2ki33mkfixcw4radk5b8sqsm29rahxg3v8ir"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-khronos-api" ,rust-khronos-api)
         ("rust-log" ,rust-log)
         ("rust-xml-rs" ,rust-xml-rs))))
    (home-page
      "https://github.com/brendanzab/gl-rs/")
    (synopsis
      "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (description
      "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (license license:asl2.0)))

(define-public rust-glutin
  (package
    (name "rust-glutin")
    (version "0.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jcr3fg5wmq32db4jjvrs9867d61z6ivwcv12qsibzmvn6ifg34k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-android-glue" ,rust-android-glue)
         ("rust-cgl" ,rust-cgl)
         ("rust-cocoa" ,rust-cocoa)
         ("rust-core-foundation" ,rust-core-foundation)
         ("rust-core-graphics" ,rust-core-graphics)
         ("rust-derivative" ,rust-derivative)
         ("rust-glutin-egl-sys" ,rust-glutin-egl-sys)
         ("rust-glutin-emscripten-sys"
          ,rust-glutin-emscripten-sys)
         ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys)
         ("rust-glutin-glx-sys" ,rust-glutin-glx-sys)
         ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-libloading" ,rust-libloading)
         ("rust-objc" ,rust-objc)
         ("rust-osmesa-sys" ,rust-osmesa-sys)
         ("rust-parking-lot" ,rust-parking-lot)
         ("rust-wayland-client" ,rust-wayland-client)
         ("rust-winapi" ,rust-winapi)
         ("rust-winit" ,rust-winit))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis
      "Cross-platform OpenGL context provider.")
    (description
      "Cross-platform OpenGL context provider.")
    (license license:asl2.0)))

(define-public rust-glutin-egl-sys
  (package
    (name "rust-glutin-egl-sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin-egl-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09nk7nknjsw2svzqrxmggc53h37xl9a9xd83v4dbdckcmf3qkx13"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-gl-generator" ,rust-gl-generator))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The egl bindings for glutin")
    (description "The egl bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-emscripten-sys
  (package
    (name "rust-glutin-emscripten-sys")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin-emscripten-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ix0jmm8p5if4qarzdfl5mz9rbq4hhgqarakb3bzwvyz13dkynr4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The emscripten bindings for glutin")
    (description
      "The emscripten bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-gles2-sys
  (package
    (name "rust-glutin-gles2-sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin-gles2-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pswvl5zyqmqwzjr674yzslj0al2xbqsp2ai9ggb9qbshlq6r6c9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-objc" ,rust-objc))
        #:cargo-development-inputs
        (("rust-gl-generator" ,rust-gl-generator))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The gles2 bindings for glutin")
    (description "The gles2 bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-glx-sys
  (package
    (name "rust-glutin-glx-sys")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin-glx-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mxs3mil68xqqb49466n5rpwpcllj6fwqjgrcrzzmz26bv5ab40j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-x11-dl" ,rust-x11-dl))
        #:cargo-development-inputs
        (("rust-gl-generator" ,rust-gl-generator))))
    (inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The glx bindings for glutin")
    (description "The glx bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-wgl-sys
  (package
    (name "rust-glutin-wgl-sys")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glutin-wgl-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08chlfzpj59q36qm212i4k879gvjzha7i90q90fds8pw3v4vn0gq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-gl-generator" ,rust-gl-generator))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The wgl bindings for glutin")
    (description "The wgl bindings for glutin")
    (license license:asl2.0)))

(define-public rust-goblin
  (package
    (name "rust-goblin")
    (version "0.0.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "goblin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1c31nrraiixy0mnda2227ih3h2g1k4pllg2kwk8yj6lwj4fjdyp3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-log)
         ("rust-plain" ,rust-plain)
         ("rust-scroll" ,rust-scroll))))
    (home-page "https://github.com/m4b/goblin")
    (synopsis
      "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (description
      "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (license license:expat)))

(define-public rust-goblin-0.0.22
  (package
    (inherit rust-goblin)
    (name "rust-goblin")
    (version "0.0.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "goblin" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a76i6zz71hjwd11pwmc8iirddj6345mfp02zl5d6bzb04sdambz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-log)
         ("rust-plain" ,rust-plain)
         ("rust-scroll" ,rust-scroll))))))

(define-public rust-h2
  (package
    (name "rust-h2")
    (version "0.1.26")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "h2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qn457y8xh03p7c7cpk76r22gqpyqxc58g5022j3iya7d0j4rcx5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-bytes" ,rust-bytes)
         ("rust-fnv" ,rust-fnv)
         ("rust-futures" ,rust-futures)
         ("rust-http" ,rust-http)
         ("rust-indexmap" ,rust-indexmap)
         ("rust-log" ,rust-log)
         ("rust-slab" ,rust-slab)
         ("rust-string" ,rust-string)
         ("rust-tokio-io" ,rust-tokio-io))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-hex" ,rust-hex)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-rand" ,rust-rand)
         ("rust-rustls" ,rust-rustls)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-rustls" ,rust-tokio-rustls)
         ("rust-walkdir" ,rust-walkdir)
         ("rust-webpki" ,rust-webpki)
         ("rust-webpki-roots" ,rust-webpki-roots))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis
      "A Tokio aware, HTTP/2.0 client & server implementation for Rust.")
    (description
      "A Tokio aware, HTTP/2.0 client & server implementation for Rust.")
    (license license:expat)))

(define-public rust-half
  (package
    (name "rust-half")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "half" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "032h6rdvkvg4k0j3zrpvzf3f1w1wnnzj0izjf0xvr5v5qsfxiqx4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-serde" ,rust-serde))))
    (home-page
      "https://github.com/starkat99/half-rs")
    (synopsis
      "Half-precision floating point f16 type for Rust implementing the IEEE 754-2008 binary16 type.")
    (description
      "Half-precision floating point f16 type for Rust implementing the IEEE 754-2008 binary16 type.")
    (license #f)))

(define-public rust-hashbrown
  (package
    (name "rust-hashbrown")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1np350nrzysy021ndn2135q5vpzrp5nli78ywz114d1vcnv2kbiv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder)
        ("rust-rayon" ,rust-rayon)
        ("rust-scopeguard" ,rust-scopeguard)
        ("rust-serde" ,rust-serde))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1.2)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-rayon" ,rust-rayon)
        ("rust-rustc-hash" ,rust-rustc-hash)
        ("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's SwissTable hash map.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-heck
  (package
    (name "rust-heck")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01a2v7yvkiqxakdqz4hw3w3g4sm52ivz9cs3qcsv2arxsmw4wmi0"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-unicode-segmentation"
          ,rust-unicode-segmentation))))
    (home-page
      "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      "heck is a case conversion library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hex-literal
  (package
    (name "rust-hex-literal")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hex-literal" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ni2nv3di0jpih2xnmlnr6s96zypkdr8xrw2cvk4f8fx5wb6inn3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-hex-literal-impl" ,rust-hex-literal-impl)
         ("rust-proc-macro-hack" ,rust-proc-macro-hack))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Procedural macro for converting hexadecimal string to byte array at compile time.")
    (description
      "Procedural macro for converting hexadecimal string to byte array at compile time.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hex-literal-0.1
  (package
    (inherit rust-hex-literal)
    (name "rust-hex-literal")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hex-literal" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ffnn5g9q5xhdmzj2ic5hk9y18kyqflbmqcssqcya9gixs5r5hnx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-hex-literal-impl" ,rust-hex-literal-impl-0.1)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack))))))

(define-public rust-hex-literal-impl
  (package
    (name "rust-hex-literal-impl")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hex-literal-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04m6d1k57a9h3hhdgn0vq1hkfwjv9hfkw6q73bqn0my0qw45s286"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro-hack" ,rust-proc-macro-hack))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Internal implementation of the hex-literal crate")
    (description
      "Internal implementation of the hex-literal crate")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hex-literal-impl-0.1
  (package
    (inherit rust-hex-literal-impl)
    (name "rust-hex-literal-impl")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hex-literal-impl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nnxqhyn9l998ma04ip79bmpqv1as6003s03g26ynhrr471p022j"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.4))))))

(define-public rust-html5ever
  (package
    (name "rust-html5ever")
    (version "0.24.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "html5ever" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1js4cr04941ld4r4fqpblvfigy75ds48qcbqhnr7nmz4l6q86m02"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-log)
         ("rust-mac" ,rust-mac)
         ("rust-markup5ever" ,rust-markup5ever))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-phf-codegen" ,rust-phf-codegen)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-rustc-test" ,rust-rustc-test)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-string-cache-codegen" ,rust-string-cache-codegen)
         ("rust-syn" ,rust-syn-1)
         ("rust-typed-arena" ,rust-typed-arena))))
    (home-page "https://github.com/servo/html5ever")
    (synopsis
      "High-performance browser-grade HTML5 parser")
    (description
      "High-performance browser-grade HTML5 parser")
    (license #f)))

(define-public rust-html5ever-0.22
  (package
    (inherit rust-html5ever)
    (name "rust-html5ever")
    (version "0.22.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "html5ever" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vn2hj096dkp0s4lxv4j7j48wpdhfjx5ry2l5xaxmhcdc5mgl4y2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-log)
         ("rust-mac" ,rust-mac)
         ("rust-markup5ever" ,rust-markup5ever-0.7))
        #:cargo-development-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-rustc-test" ,rust-rustc-test)
         ("rust-syn" ,rust-syn)
         ("rust-typed-arena" ,rust-typed-arena))))))

(define-public rust-http
  (package
    (name "rust-http")
    (version "0.1.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r5nlliz3340dqn1fanr4f13lxfqiwp9r1mhgw8lkr1rz5bcnarp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-fnv" ,rust-fnv)
         ("rust-itoa" ,rust-itoa))
        #:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment)
         ("rust-indexmap" ,rust-indexmap)
         ("rust-quickcheck" ,rust-quickcheck-0.6)
         ("rust-rand" ,rust-rand-0.4)
         ("rust-seahash" ,rust-seahash)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/hyperium/http")
    (synopsis
      "A general purpose library of common HTTP types")
    (description
      "A general purpose library of common HTTP types")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-http-body
  (package
    (name "rust-http-body")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "http-body" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0b99404k4mw6a92hvyr0qwzkqv4f866ykg0x7913limjq5cwhhb7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-http" ,rust-http)
         ("rust-tokio-buf" ,rust-tokio-buf))))
    (home-page
      "https://github.com/hyperium/http-body")
    (synopsis
      "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (description
      "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (license license:expat)))

(define-public rust-httparse
  (package
    (name "rust-httparse")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "httparse" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yf23ldnjwfkkhkca7f4w15mky9961gjz28dlwyybhphc7l9l5yd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-pico-sys" ,rust-pico-sys))))
    (home-page
      "https://github.com/seanmonstar/httparse")
    (synopsis
      "A push parser for the HTTP 1.x protocol.")
    (description
      "A push parser for the HTTP 1.x protocol.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-humantime
  (package
    (name "rust-humantime")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "humantime" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "057ilhy6vc9iqhhby5ymh45m051pgxwq2z437gwkbnqhw7rfb9rw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-quick-error" ,rust-quick-error))
        #:cargo-development-inputs
        (("rust-chrono" ,rust-chrono)
         ("rust-rand" ,rust-rand-0.4)
         ("rust-time" ,rust-time))))
    (home-page
      "https://github.com/tailhook/humantime")
    (synopsis
      "A parser and formatter for std::time::{Duration, SystemTime}")
    (description
      "A parser and formatter for std::time::{Duration, SystemTime}")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hyper
  (package
    (name "rust-hyper")
    (version "0.12.33")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13rc9js5n8b338lwif94v6mfni5cjjvxf2jcdsrlzvnqx6y4rd3w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-futures-cpupool" ,rust-futures-cpupool)
         ("rust-h2" ,rust-h2)
         ("rust-http" ,rust-http)
         ("rust-http-body" ,rust-http-body)
         ("rust-httparse" ,rust-httparse)
         ("rust-iovec" ,rust-iovec)
         ("rust-itoa" ,rust-itoa)
         ("rust-log" ,rust-log)
         ("rust-net2" ,rust-net2)
         ("rust-time" ,rust-time)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-buf" ,rust-tokio-buf)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-reactor" ,rust-tokio-reactor)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-tokio-threadpool" ,rust-tokio-threadpool)
         ("rust-tokio-timer" ,rust-tokio-timer)
         ("rust-want" ,rust-want))
        #:cargo-development-inputs
        (("rust-futures-timer" ,rust-futures-timer)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-pretty-env-logger"
          ,rust-pretty-env-logger)
         ("rust-rustc-version" ,rust-rustc-version)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-spmc" ,rust-spmc)
         ("rust-tokio-fs" ,rust-tokio-fs)
         ("rust-tokio-mockstream" ,rust-tokio-mockstream)
         ("rust-url" ,rust-url))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description
      "This package provides a fast and correct HTTP library.")
    (license license:expat)))

(define-public rust-hyper-0.10
  (package
    (inherit rust-hyper)
    (name "rust-hyper")
    (version "0.10.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wwjh9p3mzvg3fss2lqz5r7ddcgl1fh9w6my2j69d6k0lbcm41ha"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-httparse" ,rust-httparse)
        ("rust-language-tags" ,rust-language-tags)
        ("rust-log" ,rust-log-0.3)
        ("rust-mime" ,rust-mime-0.2)
        ("rust-num-cpus" ,rust-num-cpus)
        ("rust-time" ,rust-time)
        ("rust-traitobject" ,rust-traitobject)
        ("rust-typeable" ,rust-typeable)
        ("rust-unicase" ,rust-unicase)
        ("rust-url" ,rust-url))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger))))))

(define-public rust-hyper-old-types
  (package
    (name "rust-hyper-old-types")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-old-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i69sks0bwamzqdbx8ffgkssxffv6crdmwjgl47nr5pkxi8vx5k8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64" ,rust-base64)
         ("rust-bytes" ,rust-bytes)
         ("rust-http" ,rust-http)
         ("rust-httparse" ,rust-httparse)
         ("rust-language-tags" ,rust-language-tags)
         ("rust-log" ,rust-log)
         ("rust-mime" ,rust-mime)
         ("rust-percent-encoding" ,rust-percent-encoding)
         ("rust-time" ,rust-time)
         ("rust-unicase" ,rust-unicase))))
    (home-page "https://hyper.rs")
    (synopsis "HTTP types from hyper 0.11.x")
    (description "HTTP types from hyper 0.11.x")
    (license license:expat)))

(define-public rust-hyper-native-tls
  (package
    (name "rust-hyper-native-tls")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-native-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s30y20qy0akzss91yxsq1x1q7rr04jy33i0cq72nx22yjc5advd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-antidote" ,rust-antidote)
         ("rust-hyper" ,rust-hyper)
         ("rust-native-tls" ,rust-native-tls))))
    (home-page "https://github.com/sfackler/hyper-native-tls")
    (synopsis "native-tls support for Hyper 0.10.")
    (description "native-tls support for Hyper 0.10.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hyper-rustls
  (package
    (name "rust-hyper-rustls")
    (version "0.17.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0li9xkzmqd40dbjbl9g0nbf2ka9y0q538ififyd30zsavz3qb7bi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-ct-logs" ,rust-ct-logs)
         ("rust-futures" ,rust-futures)
         ("rust-hyper" ,rust-hyper)
         ("rust-rustls" ,rust-rustls)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-rustls" ,rust-tokio-rustls)
         ("rust-webpki" ,rust-webpki)
         ("rust-webpki-roots" ,rust-webpki-roots))
        #:cargo-development-inputs
        (("rust-tokio" ,rust-tokio)
         ("rust-tokio-tcp" ,rust-tokio-tcp))))
    (home-page "https://github.com/ctz/hyper-rustls")
    (synopsis
      "Rustls+hyper integration for pure rust HTTPS")
    (description
      "Rustls+hyper integration for pure rust HTTPS")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-hyper-tls
  (package
    (name "rust-hyper-tls")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyper-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kqp4sz8613j6nv375wfj3gh95ff4nb6a3rb1f2vbx0almm0v01s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-hyper" ,rust-hyper)
         ("rust-native-tls" ,rust-native-tls)
         ("rust-tokio-io" ,rust-tokio-io))
        #:cargo-development-inputs
        (("rust-tokio" ,rust-tokio))))
    (home-page "https://hyper.rs")
    (synopsis
      "Default TLS implementation for use with hyper")
    (description
      "Default TLS implementation for use with hyper")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hyphenation
  (package
    (name "rust-hyphenation")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyphenation" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0k5msv8calmnfd5kw1rmq4bg5hn1vcd39kbsxl57sdld63xwd4q4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-atlatl" ,rust-atlatl)
         ("rust-bincode" ,rust-bincode)
         ("rust-hyphenation-commons"
          ,rust-hyphenation-commons)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-atlatl" ,rust-atlatl)
         ("rust-bincode" ,rust-bincode)
         ("rust-hyphenation-commons"
          ,rust-hyphenation-commons)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-pocket-resources" ,rust-pocket-resources)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-serde" ,rust-serde)
         ("rust-unicode-normalization"
          ,rust-unicode-normalization)
         ("rust-unicode-segmentation"
          ,rust-unicode-segmentation))))
    (home-page
      "https://github.com/tapeinosyne/hyphenation")
    (synopsis
      "Knuth-Liang hyphenation for a variety of languages")
    (description
      "Knuth-Liang hyphenation for a variety of languages")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hyphenation-commons
  (package
    (name "rust-hyphenation-commons")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyphenation_commons" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pasnbk3rbdgf30jjjh1h24a9pxpdrnn0ihcivmpnzqha6mn2d4y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-atlatl" ,rust-atlatl)
         ("rust-serde" ,rust-serde))))
    (home-page
      "https://github.com/tapeinosyne/hyphenation")
    (synopsis
      "Proemial code for the `hyphenation` library")
    (description
      "Proemial code for the `hyphenation` library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ident-case
  (package
    (name "rust-ident-case")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ident-case" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/TedDriggs/ident_case")
    (synopsis
      "Utility for applying case rules to Rust identifiers.")
    (description
      "Utility for applying case rules to Rust identifiers.")
    (license #f)))

(define-public rust-idna
  (package
    (name "rust-idna")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "idna" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a9066imqpdrm1aavfasdyb1zahqaz8jmdcwdawvb1pf60y6gqh2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-matches" ,rust-matches)
         ("rust-unicode-bidi" ,rust-unicode-bidi)
         ("rust-unicode-normalization"
          ,rust-unicode-normalization))
        #:cargo-development-inputs
        (("rust-rustc-test" ,rust-rustc-test)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/servo/rust-url/")
    (synopsis
      "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (description
      "IDNA (Internationalizing Domain Names in Applications) and Punycode.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-idna-0.1
  (package
    (inherit rust-idna)
    (name "rust-idna")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "idna" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kl4gs5kaydn4v07c6ka33spm9qdh2np0x7iw7g5zd8z1c7rxw1q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-matches" ,rust-matches)
         ("rust-unicode-bidi" ,rust-unicode-bidi)
         ("rust-unicode-normalization"
          ,rust-unicode-normalization))
        #:cargo-development-inputs
        (("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-rustc-test" ,rust-rustc-test))))))

(define-public rust-if-chain
  (package
    (name "rust-if-chain")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "if-chain" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zgcn31bahnsmsjc0cgk0cy38p8sfjs79yvi6rjs5zz5b5xhqdn3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/lfairy/if_chain")
    (synopsis
      "Macro for writing nested `if let` expressions.")
    (description
      "Macro for writing nested `if let` expressions.")
    (license #f)))

(define-public rust-if-chain-0.1
  (package
    (inherit rust-if-chain)
    (name "rust-if-chain")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "if-chain" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v2phdgq9i313svbrcaldygivd0jn25gpml7h6vyf906mbcrbb2b"))))))

(define-public rust-image
  (package
    (name "rust-image")
    (version "0.22.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "image" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09kqym26z03j31rfzr9zd8i8y2d0apxmm4mf8bf4axdyxymfhjvv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-gif" ,rust-gif)
         ("rust-jpeg-decoder" ,rust-jpeg-decoder)
         ("rust-num-iter" ,rust-num-iter)
         ("rust-num-rational" ,rust-num-rational)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-png" ,rust-png)
         ("rust-scoped-threadpool"
          ,rust-scoped-threadpool)
         ("rust-tiff" ,rust-tiff))
        #:cargo-development-inputs
        (("rust-crc32fast" ,rust-crc32fast)
         ("rust-glob" ,rust-glob)
         ("rust-num-complex" ,rust-num-complex)
         ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/image-rs/image")
    (synopsis
      "Imaging library written in Rust. Provides basic filters and decoders for the most common image formats.")
    (description
      "Imaging library written in Rust.  Provides basic filters and decoders for the most common image formats.")
    (license license:expat)))

(define-public rust-indexmap
  (package
    (name "rust-indexmap")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "indexmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13f5k1kl2759y4xfy0vhays35fmrkmhqngbr2ny8smvrbz0ag0by"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-itertools" ,rust-itertools-0.7)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-quickcheck" ,rust-quickcheck-0.6)
         ("rust-rand" ,rust-rand)
         ("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis
      "A hash table with consistent order and fast iteration.

      The indexmap is a hash table where the iteration order of the key-value
      pairs is independent of the hash values of the keys. It has the usual
      hash table functionality, it preserves insertion order except after
      removals, and it allows lookup of its elements by either hash table key
      or numerical index. A corresponding hash set type is also provided.

      This crate was initially published under the name ordermap, but it was renamed to
      indexmap.")
    (description
      "This package provides a hash table with consistent order and fast iteration.

      The indexmap is a hash table where the iteration order of the key-value
      pairs is independent of the hash values of the keys.  It has the usual
      hash table functionality, it preserves insertion order except after
      removals, and it allows lookup of its elements by either hash table key
      or numerical index.  A corresponding hash set type is also provided.

      This crate was initially published under the name ordermap, but it was renamed to
      indexmap.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-insta
  (package
    (name "rust-insta")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "insta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jk41bzid509y3s2r3hwh0s8cmcyywd0jzcgbs4ixb6sm2b7d0ll"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-chrono" ,rust-chrono)
         ("rust-ci-info" ,rust-ci-info)
         ("rust-console" ,rust-console)
         ("rust-difference" ,rust-difference)
         ("rust-failure" ,rust-failure)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-pest" ,rust-pest)
         ("rust-pest-derive" ,rust-pest-derive)
         ("rust-ron" ,rust-ron)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-serde-yaml" ,rust-serde-yaml)
         ("rust-uuid" ,rust-uuid))))
    (home-page "https://github.com/mitsuhiko/insta")
    (synopsis "A snapshot testing library for Rust")
    (description
      "This package provides a snapshot testing library for Rust")
    (license license:asl2.0)))

(define-public rust-intel-mkl-src
  (package
    (name "rust-intel-mkl-src")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "intel-mkl-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16kq725cl4rnvfpwq9x4rl83ylcqs7d0xryagx8ijm6bdblbfabc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,rust-failure)
        ("rust-pkg-config" ,rust-pkg-config)
        ("rust-reqwest" ,rust-reqwest)
        ("rust-tar" ,rust-tar)
        ("rust-xz2" ,rust-xz2))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc))))
    (inputs
     `(("pkg-config" ,pkg-config)
       ("xz" ,xz)))
    (home-page "https://github.com/rust-math/intel-mkl-src")
    (synopsis "Redistribution of Intel MKL as a crate")
    (description
     "Redistribution of Intel @acronym{MKL, Math Kernel Library} as a crate.")
    (license (list license:non-copyleft "Intel Simplified Software License"
                   license:expat)))) ; some wrapper codes

(define-public rust-intervaltree
  (package
    (name "rust-intervaltree")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "intervaltree" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10k40gsv79kwnsqrzwmnmm6psa5fqws8yggavmbggvymv16hffdg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-smallvec" ,rust-smallvec))))
    (home-page
      "https://github.com/main--/rust-intervaltree")
    (synopsis
      "A simple and generic implementation of an immutable interval tree.")
    (description
      "This package provides a simple and generic implementation of an immutable interval tree.")
    (license license:expat)))

(define-public rust-iron
  (package
    (name "rust-iron")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iron" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1s4mf8395f693nhwsr0znw3j5frzn56gzllypyl50il85p50ily6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-hyper" ,rust-hyper-0.10)
         ("rust-log" ,rust-log)
         ("rust-mime-guess" ,rust-mime-guess)
         ("rust-modifier" ,rust-modifier)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-plugin" ,rust-plugin)
         ("rust-typemap" ,rust-typemap)
         ("rust-url" ,rust-url)
         ("rust-hyper-native-tls" ,rust-hyper-native-tls))
        #:cargo-development-inputs
        (("rust-mime" ,rust-mime)
         ("rust-time" ,rust-time))))
    (home-page "https://github.com/iron/iron")
    (synopsis "Extensible, Concurrency Focused Web Development in Rust.")
    (description "Extensible, Concurrency Focused Web Development in Rust.")
    (license license:expat)))

(define-public rust-ipconfig
  (package
    (name "rust-ipconfig")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ipconfig" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gyqiqr4nk2dw9ild1aq3hnv6984sgydfdq7ki586q5ydwhzlyda"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-socket2" ,rust-socket2)
         ("rust-widestring" ,rust-widestring)
         ("rust-winapi" ,rust-winapi)
         ("rust-winreg" ,rust-winreg))))
    (home-page
      "https://github.com/liranringel/ipconfig")
    (synopsis
      "Get network adapters information and network configuration for windows.")
    (description
      "Get network adapters information and network configuration for windows.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itertools
  (package
    (name "rust-itertools")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n2k13b6w4x2x6np2lykh9bj3b3z4hwh2r4cn3z2dgnfq7cng12v"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-either" ,rust-either))
        #:cargo-development-inputs
        (("rust-permutohedron" ,rust-permutohedron)
         ("rust-quickcheck" ,rust-quickcheck-0.7)
         ("rust-rand" ,rust-rand-0.6))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itertools-0.7
  (package
    (inherit rust-itertools)
    (name "rust-itertools")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "03cpsj26xmyamcalclqzr1i700vwx8hnbgxbpjvs354f8mnr8iqd"))))
    (arguments
     `(#:cargo-inputs
       (("rust-either" ,rust-either))
       #:cargo-development-inputs
       (("rust-permutohedron" ,rust-permutohedron)
        ("rust-quickcheck" ,rust-quickcheck-0.5))))))

(define-public rust-itertools-num
  (package
    (name "rust-itertools-num")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itertools-num" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1rr7ig9nkpampcas23s91x7yac6qdnwssq3nap522xbgkqps4wm8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-traits" ,rust-num-traits))
       #:cargo-development-inputs
       (("rust-itertools" ,rust-itertools-0.7)
        ("rust-quickcheck" ,rust-quickcheck-0.7))))
    (home-page "https://github.com/bluss/itertools-num")
    (synopsis
      "Numerical iterator tools. Extra iterators and iterator methods and functions.")
    (description
      "Numerical iterator tools.  Extra iterators and iterator methods and functions.")
    (license #f)))

(define-public rust-itoa-0.3
  (package
    (inherit rust-itoa)
    (name "rust-itoa")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "136vwi6l2k1vrlvfx49lhficj813pk88xrcx1q3axqh1mwms6943"))))))

(define-public rust-jemalloc-ctl
  (package
    (name "rust-jemalloc-ctl")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jemalloc-ctl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dx4mik2ww5ic4qqv5zx9dxq6pbkclxnxa9bscg4z4njkpzsa0n5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-jemalloc-sys" ,rust-jemalloc-sys)
        ("rust-libc" ,rust-libc)
        ("rust-paste" ,rust-paste))
       #:cargo-development-inputs
       (("rust-jemallocator" ,rust-jemallocator))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-jemalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jemalloc (assoc-ref inputs "jemalloc")))
               (setenv "JEMALLOC_OVERRIDE"
                       (string-append jemalloc "/lib/libjemalloc_pic.a")))
             #t)))))
    (inputs
     `(("jemalloc" ,jemalloc)))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis
      "A safe wrapper over jemalloc's control and introspection APIs")
    (description
      "This package provides a safe wrapper over jemalloc's control and introspection APIs")
    (license #f)))

(define-public rust-jemallocator
  (package
    (name "rust-jemallocator")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jemallocator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sabfa5118b7l4ars5n36s2fjyfn59w4d6mjs6rrmsa5zky67bj3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-jemalloc-sys" ,rust-jemalloc-sys)
        ("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-paste" ,rust-paste))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-jemalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jemalloc (assoc-ref inputs "jemalloc")))
               (setenv "JEMALLOC_OVERRIDE"
                       (string-append jemalloc "/lib/libjemalloc_pic.a")))
             #t)))))
    (inputs
     `(("jemalloc" ,jemalloc)))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis "A Rust allocator backed by jemalloc")
    (description
      "This package provides a Rust allocator backed by jemalloc")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-jpeg-decoder
  (package
    (name "rust-jpeg-decoder")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jpeg-decoder" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1flj2wq4xdzv6nqs3vk2l3jsg4lpwiz6lfrccb30kr7azs7y3an1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-rayon" ,rust-rayon))
        #:cargo-development-inputs
        (("rust-png" ,rust-png)
         ("rust-walkdir" ,rust-walkdir))))
    (home-page
      "https://github.com/kaksmet/jpeg-decoder")
    (synopsis "JPEG decoder")
    (description "JPEG decoder")
    (license #f)))

(define-public rust-js-sys
  (package
    (name "rust-js-sys")
    (version "0.3.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "js-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m6p0bdgdkhwwy6bvapl9z3bw9nq3li2x480anfffn3calm4zz0y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-wasm-bindgen" ,rust-wasm-bindgen))
        #:cargo-development-inputs
        (("rust-futures" ,rust-futures)
         ("rust-wasm-bindgen-futures"
          ,rust-wasm-bindgen-futures)
         ("rust-wasm-bindgen-test"
          ,rust-wasm-bindgen-test))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Bindings for all JS global objects and functions in all JS environments like Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (description
      "Bindings for all JS global objects and functions in all JS environments like Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-khronos-api
  (package
    (name "rust-khronos-api")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "khronos-api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/brendanzab/gl-rs/")
    (synopsis
      "The Khronos XML API Registry, exposed as byte string constants.")
    (description
      "The Khronos XML API Registry, exposed as byte string constants.")
    (license license:asl2.0)))

(define-public rust-lazycell
  (package
    (name "rust-lazycell")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazycell" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0gvqycmpv7parc98i6y64ai7rvxrn1947z2a6maa02g4kvxdd55j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clippy" ,rust-clippy))))
    (home-page "https://github.com/indiv0/lazycell")
    (synopsis "A library providing a lazily filled Cell struct")
    (description
     "This package provides a library providing a lazily filled Cell struct.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-lazy-static-1.2
  (package
    (inherit rust-lazy-static)
    (name "rust-lazy-static")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy-static" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18fy414dxmg92qjsh1dl045yrpnrc64f7hbl792ran5mkndwhx53"))))
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin-0.4))))))

(define-public rust-lazy-static-1.0
  (package
    (inherit rust-lazy-static)
    (name "rust-lazy-static")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy-static" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1lvdk5mkhw0im12fignd0i38miy2gyh5cjfrrwqs7dk2scspqjgv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin-0.4))
       #:tests? #f)))) ; TODO: Fix doc test.

(define-public rust-lazy-static-0.2
  (package
    (inherit rust-lazy-static)
    (name "rust-lazy-static")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy-static" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wxy8vak7jsx6r8gx475pjqpx11p2bfq4wvw6idmqi31mp3k7w3n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-compiletest-rs" ,rust-compiletest-rs)
        ("rust-spin" ,rust-spin-0.4))
       #:tests? #f)))) ; Tests fail to compile.

(define-public rust-lazy-static-0.1
  (package
    (inherit rust-lazy-static)
    (name "rust-lazy-static")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy-static" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "05vl1h4b0iv800grsdyc3fg2bq29p70wjav6zpjvxxd5i8d6s66g"))))
    (arguments '())))

(define-public rust-lexical-core
  (package
    (name "rust-lexical-core")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lexical-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gr5y3ykghd3wjc00l3iizkj1dxylyhwi6fj6yn2qg06nzx771iz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-dtoa" ,rust-dtoa)
         ("rust-ryu" ,rust-ryu)
         ("rust-stackvector" ,rust-stackvector)
         ("rust-static-assertions"
          ,rust-static-assertions))
        #:cargo-development-inputs
        (("rust-approx" ,rust-approx)
         ("rust-proptest" ,rust-proptest)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-rustc-version" ,rust-rustc-version))))
    (home-page
      "https://github.com/Alexhuszagh/rust-lexical/tree/master/lexical-core")
    (synopsis
      "Lexical, to- and from-string conversion routines.")
    (description
      "Lexical, to- and from-string conversion routines.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libc-print
  (package
    (name "rust-libc-print")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libc-print" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sh4l815w7zxg8w17fvwj63y421sjqxxrdamzwyvg90n6mr70phv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-libc" ,rust-libc))))
    (home-page
      "https://github.com/mmastrac/rust-libc-print")
    (synopsis
      "println! and eprintln! macros on libc without stdlib")
    (description
      "println! and eprintln! macros on libc without stdlib")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libflate
  (package
    (name "rust-libflate")
    (version "0.1.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libflate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p8z839c5lpl0g01mf8iglys9lgcjxw6xjw56crhwp8z7gs5s4yr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-adler32" ,rust-adler32)
         ("rust-crc32fast" ,rust-crc32fast)
         ("rust-rle-decode-fast" ,rust-rle-decode-fast)
         ("rust-take-mut" ,rust-take-mut))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap))))
    (home-page "https://github.com/sile/libflate")
    (synopsis
      "A Rust implementation of DEFLATE algorithm and related formats (ZLIB, GZIP)")
    (description
      "This package provides a Rust implementation of DEFLATE algorithm and related formats (ZLIB, GZIP)")
    (license license:expat)))

(define-public rust-libloading-0.3
  (package
    (inherit rust-libloading)
    (name "rust-libloading")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libloading" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0risz19rllhdc0d7nkpwkf4pcbjjgg1iim0kkmzb6kkp874hl0ha"))))
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys)
        ("rust-lazy-static" ,rust-lazy-static-0.2)
        ("rust-winapi" ,rust-winapi)
        ("rust-target-build-utils" ,rust-target-build-utils))
       #:cargo-development-inputs
       (("rust-target-build-utils" ,rust-target-build-utils))
       #:tests? #f)))) ; TODO: Fix tests

(define-public rust-linked-hash-map
  (package
    (name "rust-linked-hash-map")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "linked-hash-map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10qgbvh00q36ql0jh00rxh2jlq6qvl11n6mig0cvkpf4xf5bd4df"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-clippy" ,rust-clippy)
         ("rust-heapsize" ,rust-heapsize)
         ("rust-serde" ,rust-serde)
         ("rust-serde-test" ,rust-serde-test))))
    (home-page
      "https://github.com/contain-rs/linked-hash-map")
    (synopsis
      "A HashMap wrapper that holds key-value pairs in insertion order")
    (description
      "This package provides a HashMap wrapper that holds key-value pairs in insertion order")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-linked-hash-map-0.4
  (package
    (inherit rust-linked-hash-map)
    (name "rust-linked-hash-map")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "linked-hash-map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fd958y02ggwpa2246kmjky9xmnww7vxg0ik3rxgy23hgwlyqq3q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-clippy" ,rust-clippy)
         ("rust-heapsize" ,rust-heapsize-0.3)
         ("rust-serde" ,rust-serde-0.9)
         ("rust-serde-test" ,rust-serde-test-0.9))))))

(define-public rust-linked-hash-map-0.3
  (package
    (inherit rust-linked-hash-map)
    (name "rust-linked-hash-map")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "linked-hash-map" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1kaf95grvfqchxn8pl0854g8ab0fzl56217hndhhhz5qqm2j09kd"))))
    (arguments
     `(#:cargo-inputs
       (("rust-clippy" ,rust-clippy)
        ("rust-serde" ,rust-serde-0.8)
        ("rust-serde-test" ,rust-serde-test-0.8))))))

(define-public rust-lipsum
  (package
    (name "rust-lipsum")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lipsum" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nlxkz8zjxqmbrxqasr36a5fqn2n33cxy11w0x0a0b6mcx04dr2q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand" ,rust-rand))
        #:cargo-development-inputs
        (("rust-rand-xorshift" ,rust-rand-xorshift)
         ("rust-version-sync" ,rust-version-sync))))
    (home-page "https://github.com/mgeisler/lipsum/")
    (synopsis
      "Lipsum is a lorem ipsum text generation library. Use this if you need
      some filler text for your application.

      The text is generated using a simple Markov chain, which you can also
      instantiate to generate your own pieces of pseudo-random text.")
    (description
      "Lipsum is a lorem ipsum text generation library.  Use this if you need
      some filler text for your application.

      The text is generated using a simple Markov chain, which you can also
      instantiate to generate your own pieces of pseudo-random text.")
    (license license:expat)))

(define-public rust-lock-api-0.3
  (package
    (inherit rust-lock-api)
    (name "rust-lock-api")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1p04271jikw69ja0ap0plrfwm9incf1iny48g0b3ma9k4mw2x4gq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-owning-ref" ,rust-owning-ref)
        ("rust-scopeguard" ,rust-scopeguard)
        ("rust-serde" ,rust-serde))))))

(define-public rust-log
  (package
    (name "rust-log")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1cx51p86d9s1y2vbyi6jr4wj5cs7nryikygfcpbc42h7ajnvcxf2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-serde" ,rust-serde))
       #:cargo-development-inputs
       (("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/rust-lang-nursery/log")
    (synopsis
      "A lightweight logging facade for Rust")
    (description
      "This package provides a lightweight logging facade for Rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-log-0.3
  (package
    (inherit rust-log)
    (name "rust-log")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jq23hhn5h35k7pa8r7wqnsywji6x3wn1q5q7lif5q536if8v7p1"))))
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log))))))

(define-public rust-loom
  (package
    (name "rust-loom")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "loom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0w95rjw0sv3f5psaxlgfl4fsj6imjv16v2pap18alx2w7n2f8j24"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-futures-util-preview"
          ,rust-futures-util-preview)
         ("rust-generator" ,rust-generator)
         ("rust-scoped-tls" ,rust-scoped-tls)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/carllerche/loom")
    (synopsis
      "Permutation testing for concurrent code")
    (description
      "Permutation testing for concurrent code")
    (license license:expat)))

(define-public rust-lru-cache
  (package
    (name "rust-lru-cache")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lru-cache" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "071viv6g2p3akwqmfb3c8vsycs5n7kr17b70l7la071jv0d4zqii"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-heapsize" ,rust-heapsize)
         ("rust-linked-hash-map" ,rust-linked-hash-map))))
    (home-page
      "https://github.com/contain-rs/lru-cache")
    (synopsis
      "A cache that holds a limited number of key-value pairs")
    (description
      "This package provides a cache that holds a limited number of key-value pairs")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-mac
  (package
    (name "rust-mac")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "194vc7vrshqff72rl56f9xgb0cazyl4jda7qsv31m5l6xx7hq7n4"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/reem/rust-mac.git")
    (synopsis
      "A collection of great and ubiqutitous macros.")
    (description
      "This package provides a collection of great and ubiqutitous macros.")
    (license #f)))

(define-public rust-malloc-buf
  (package
    (name "rust-malloc-buf")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "malloc-buf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zap9m0xmd5sdsxil7v2rgb1dzlq0308f826pwvqdvjyaz0chciz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-libc" ,rust-libc))))
    (home-page
      "https://github.com/SSheldon/malloc_buf")
    (synopsis
      "Structs for handling malloc'd memory passed to Rust.")
    (description
      "Structs for handling malloc'd memory passed to Rust.")
    (license license:expat)))

(define-public rust-malloc-buf-0.0
  (package
    (inherit rust-malloc-buf)
    (name "rust-malloc-buf")
    (version "0.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "malloc-buf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-libc" ,rust-libc))))))

(define-public rust-markup5ever
  (package
    (name "rust-markup5ever")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "markup5ever" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00wxigkiw8f777pjp7q5kfq77xpwda9zskkwp698assh8yfisf35"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-log)
         ("rust-phf" ,rust-phf)
         ("rust-string-cache" ,rust-string-cache)
         ("rust-tendril" ,rust-tendril))
        #:cargo-development-inputs
        (("rust-phf-codegen" ,rust-phf-codegen)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-string-cache-codegen" ,rust-string-cache-codegen))))
    (home-page "https://github.com/servo/html5ever")
    (synopsis
      "Common code for xml5ever and html5ever")
    (description
      "Common code for xml5ever and html5ever")
    (license #f)))

(define-public rust-markup5ever-0.7
  (package
    (inherit rust-markup5ever)
    (name "rust-markup5ever")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "markup5ever" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zv2k29zkpf2nb54c20ghs9r7p2kxn1hcm550m4yyghchpwkcxl9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-phf" ,rust-phf)
         ("rust-string-cache" ,rust-string-cache)
         ("rust-tendril" ,rust-tendril)
         ("rust-phf-codegen" ,rust-phf-codegen)
         ("rust-string-cache-codegen" ,rust-string-cache-codegen))
        #:cargo-development-inputs
        (("rust-phf-codegen" ,rust-phf-codegen)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-string-cache-codegen" ,rust-string-cache-codegen))))))

(define-public rust-matrixmultiply
  (package
    (name "rust-matrixmultiply")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matrixmultiply" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16sgc1j87hmsqmhlqpqgcpbrb00f267ikbr55fhxla8nhwnxgznw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rawpointer" ,rust-rawpointer))
        #:cargo-development-inputs
        (("rust-bencher" ,rust-bencher)
         ("rust-itertools" ,rust-itertools-0.7))))
    (home-page
      "https://github.com/bluss/matrixmultiply/")
    (synopsis "General matrix multiplication for f32 and f64 matrices.")
    (description "General matrix multiplication for f32 and f64 matrices. Operates on matrices with general layout (they can use arbitrary row and column stride). Detects and uses AVX or SSE2 on x86 platforms transparently for higher performance. Uses a microkernel strategy, so that the implementation is easy to parallelize and optimize.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-matrixmultiply-0.1
  (package
    (inherit rust-matrixmultiply)
    (name "rust-matrixmultiply")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matrixmultiply" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "00p0fpjhm45qdzi37mgv7ggsy8b9gqvq4999yrbgyn1dxkf6gbfw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-rawpointer" ,rust-rawpointer))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher))))))

(define-public rust-matrixmultiply-0.1.8
  (package
    (inherit rust-matrixmultiply)
    (name "rust-matrixmultiply")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matrixmultiply" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "006i0czg8ggd5vz5fbyf8k6y65r3z368yy3mm4cg0njaazzhmcln"))))
    (arguments
     `(#:cargo-inputs
       ()
       #:cargo-development-inputs
       ()))))

(define-public rust-maybe-uninit
  (package
    (name "rust-maybe-uninit")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "maybe-uninit" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "004y0nzmpfdrhz251278341z6ql34iv1k6dp1h6af7d6nd6jwc30"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/est31/maybe-uninit")
    (synopsis
      "MaybeUninit for friends of backwards compatibility")
    (description
      "MaybeUninit for friends of backwards compatibility")
    (license (list license:asl2.0 license:expat))))

(define-public rust-memchr
  (package
    (name "rust-memchr")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13j6ji9x9ydpi9grbss106gqqr3xn3bcfp28aydqfa4751qrfmw8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc))
        #:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page
      "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-memchr-1
  (package
    (inherit rust-memchr)
    (name "rust-memchr")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memchr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yjyja34pzhipdl855q3m21w1lyih4lw79x2dp3czwdla4pap3ql"))))
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc))
        #:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.4))
        #:tests? #f)))) ; Tests require 'unicode' crate.

(define-public rust-memmap-0.4
  (package
    (inherit rust-memmap)
    (name "rust-memmap")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1skgf49pasjqkq639frhg4fjpz039blxpscgx9ahh1qhm8j349b9"))))
    (arguments
      `(#:cargo-inputs
        (("rust-fs2" ,rust-fs2-0.2)
         ("rust-kernel32-sys" ,rust-kernel32-sys)
         ("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))))

(define-public rust-memoffset
  (package
    (name "rust-memoffset")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memoffset" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zqpz1apkxvzbi41q07vaxpn3bmvhqqkmg8bbbpbgfrv0gdpaq6f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-rustc-version" ,rust-rustc-version))))
    (home-page "https://github.com/Gilnaa/memoffset")
    (synopsis
      "offset_of functionality for Rust structs.")
    (description
      "offset_of functionality for Rust structs.")
    (license license:expat)))

(define-public rust-memoffset-0.2
  (package
    (inherit rust-memoffset)
    (name "rust-memoffset")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memoffset" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1cvm2z7dy138s302ii7wlzcxbka5a8yfl5pl5di7lbdnw9hw578g"))))))

(define-public rust-mime-0.2
  (package
    (inherit rust-mime)
    (name "rust-mime")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mime" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1q1s1ax1gaz8ld3513nvhidfwnik5asbs1ma3hp6inp5dn56nqms"))))
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.3)
        ("rust-heapsize" ,rust-heapsize-0.3)
        ("rust-serde" ,rust-serde-0.8))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-0.8))))))

(define-public rust-mime-guess
  (package
    (name "rust-mime-guess")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mime_guess" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16c5ssgali30db6jh1cndy77dd1qgcykhshiyfyjvxxf94wx03hs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-mime" ,rust-mime)
         ("rust-unicase" ,rust-unicase))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2))))
    (home-page "https://github.com/abonander/mime_guess")
    (synopsis
      "MIME/MediaType guessing by file extension. Uses a static map of known file extension -> MIME type mappings.")
    (description
      "MIME/MediaType guessing by file extension. Uses a static map of known file extension -> MIME type mappings.")
    (license license:expat)))

(define-public rust-mint
  (package
    (name "rust-mint")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mint" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01j14izcqvrcsxrhi53f0cw5k92k8q1zai0nbv2vy7bksrq5h4dr"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/kvark/mint")
    (synopsis "Math interoperability standard types")
    (description
      "Math interoperability standard types")
    (license license:expat)))

(define-public rust-mio
  (package
    (name "rust-mio")
    (version "0.6.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08zzs227vrnyz5kvws6awzlgzb8zqpnihs71hkqlw07dlfb1kxc3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fuchsia-zircon" ,rust-fuchsia-zircon)
         ("rust-fuchsia-zircon-sys"
          ,rust-fuchsia-zircon-sys)
         ("rust-iovec" ,rust-iovec)
         ("rust-kernel32-sys" ,rust-kernel32-sys)
         ("rust-libc" ,rust-libc)
         ("rust-log" ,rust-log)
         ("rust-miow" ,rust-miow-0.2)
         ("rust-net2" ,rust-net2)
         ("rust-slab" ,rust-slab)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-tempdir" ,rust-tempdir))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description "Lightweight non-blocking IO")
    (license license:expat)))

(define-public rust-mio-uds
  (package
    (name "rust-mio-uds")
    (version "0.6.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mio-uds" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09gimdbnj7b9yca99pk8lxh9jhl79msj795c8fxi2sqr9slmfqln"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-iovec" ,rust-iovec)
         ("rust-libc" ,rust-libc)
         ("rust-mio" ,rust-mio))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://github.com/alexcrichton/mio-uds")
    (synopsis
      "Unix domain socket bindings for mio")
    (description
      "Unix domain socket bindings for mio")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-native-tls
  (package
    (name "rust-native-tls")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "native-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ki7cj4wzyd2nach4qdjly69sp7rs0yz3n3z2ii4mm1gqajg2bab"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static)
        ("rust-libc" ,rust-libc)
        ("rust-log" ,rust-log)
        ("rust-openssl" ,rust-openssl)
        ("rust-openssl-probe" ,rust-openssl-probe)
        ("rust-openssl-sys" ,rust-openssl-sys)
        ("rust-schannel" ,rust-schannel)
        ("rust-security-framework" ,rust-security-framework)
        ("rust-security-framework-sys" ,rust-security-framework-sys)
        ("rust-tempfile" ,rust-tempfile))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             #t)))))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://github.com/sfackler/rust-native-tls")
    (synopsis
      "An abstraction over platform-specific TLS implementations.")
    (description
      "An abstraction over platform-specific TLS implementations.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ndarray
  (package
    (name "rust-ndarray")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndarray" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0a5rfwcbqnvbwi3nw5sfz6kf0flhmjxs64s0b4kxc6lhmyl81wvw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-itertools" ,rust-itertools-0.7)
        ("rust-matrixmultiply" ,rust-matrixmultiply-0.1)
        ("rust-num-complex" ,rust-num-complex)
        ("rust-num-traits" ,rust-num-traits)
        ("rust-blas-src" ,rust-blas-src-0.2)
        ("rust-cblas-sys" ,rust-cblas-sys)
        ("rust-rustc-serialize" ,rust-rustc-serialize)
        ("rust-serde" ,rust-serde))
       #:cargo-development-inputs
       (("rust-defmac" ,rust-defmac-0.1)
        ("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rawpointer" ,rust-rawpointer))))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
      "ndarray implements an n-dimensional container for general elements and for numerics.")
    (description
      "ndarray implements an n-dimensional container for general elements and for numerics.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-new-debug-unreachable
  (package
    (name "rust-new-debug-unreachable")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "new-debug-unreachable" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c1br326qa0rrzxrn2rd5ah7xaprig2i9r4rwsx06vnvc1f003zl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f)) ; SIGILL: illegal instruction
    (home-page
      "https://github.com/mbrubeck/rust-debug-unreachable")
    (synopsis
      "panic in debug, intrinsics::unreachable() in release (fork of debug_unreachable)")
    (description
      "panic in debug, intrinsics::unreachable() in release (fork of debug_unreachable)")
    (license license:expat)))

(define-public rust-no-panic
  (package
    (name "rust-no-panic")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "no-panic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "089gmyxg7kviimqn5nmghm5kngnmi77a0c6fbv0j67jxx7pjhq3r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-tempfile" ,rust-tempfile))))
    (home-page "https://github.com/dtolnay/no-panic")
    (synopsis
      "Attribute macro to require that the compiler prove a function can't ever panic.")
    (description
      "Attribute macro to require that the compiler prove a function can't ever panic.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-nom
  (package
    (name "rust-nom")
    (version "5.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06snml9wfrvk8k71l4md6gg29jgj4pa8wzsg180q3qr0jf2isxp9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-lexical-core" ,rust-lexical-core)
         ("rust-memchr" ,rust-memchr)
         ("rust-regex" ,rust-regex))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-doc-comment" ,rust-doc-comment)
         ("rust-jemallocator" ,rust-jemallocator)
         ("rust-version-check" ,rust-version-check))))
    (home-page "https://github.com/Geal/nom")
    (synopsis
      "A byte-oriented, zero-copy, parser combinators library")
    (description
      "This package provides a byte-oriented, zero-copy, parser combinators library")
    (license license:expat)))

(define-public rust-nom-4
  (package
    (inherit rust-nom)
    (name "rust-nom")
    (version "4.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1mkvby8b4m61p4g1px0pwr58yfkphyp1jcfbp4qfp7l6iqdaklia"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-memchr" ,rust-memchr)
         ("rust-regex" ,rust-regex)
         ("rust-version-check" ,rust-version-check-0.1))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-jemallocator" ,rust-jemallocator)
         ("rust-version-check" ,rust-version-check-0.1))))))

(define-public rust-nom-3
  (package
    (inherit rust-nom)
    (name "rust-nom")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yr8fazcspgawl6s7wmx5llz61s68jl88cnrph18fa7xf06cbbh5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-compiler-error" ,rust-compiler-error)
        ("rust-lazy-static" ,rust-lazy-static-0.2)
        ("rust-memchr" ,rust-memchr-1)
        ("rust-regex" ,rust-regex-0.2))
       #:tests? #f)))) ; stream::tests::seeking_consumer fails

(define-public rust-num-complex
  (package
    (name "rust-num-complex")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-complex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z6zjdzx1g1hj4y132ddy83d3p3zvw06igbf59npxxrzzcqwzc7w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-num-traits" ,rust-num-traits)
         ("rust-rand" ,rust-rand)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-autocfg" ,rust-autocfg))))
    (home-page
      "https://github.com/rust-num/num-complex")
    (synopsis
      "Complex numbers implementation for Rust")
    (description
      "Complex numbers implementation for Rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-rational
  (package
    (name "rust-num-rational")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-rational" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m5l76rdzzq98cfhnbjsxfngz6w75pal5mnfflpxqapysmw5527j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-num-bigint" ,rust-num-bigint)
         ("rust-num-integer" ,rust-num-integer)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-autocfg" ,rust-autocfg))))
    (home-page
      "https://github.com/rust-num/num-rational")
    (synopsis
      "Rational numbers implementation for Rust")
    (description
      "Rational numbers implementation for Rust")
    (license #f)))

(define-public rust-obj
  (package
    (name "rust-obj")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "obj" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p550ws511h9qic01ppn1p3kgzwyhd2gd1rnrb2z17hgc8yv9bxh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-genmesh" ,rust-genmesh))))
    (home-page "https://github.com/kvark/obj")
    (synopsis
      "A package for loading Wavefront .obj files")
    (description
      "This package provides a package for loading Wavefront .obj files")
    (license license:asl2.0)))

(define-public rust-objc
  (package
    (name "rust-objc")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "objc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03ar7qxhailxgb0zi5lszv7fhwl6b1xkas5y4m8wy1vyng90zlii"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-malloc-buf" ,rust-malloc-buf-0.0)
         ("rust-objc-exception" ,rust-objc-exception))))
    (inputs
     `(("gcc-objc" ,gcc-objc)))
    (home-page
      "http://github.com/SSheldon/rust-objc")
    (synopsis
      "Objective-C Runtime bindings and wrapper for Rust.")
    (description
      "Objective-C Runtime bindings and wrapper for Rust.")
    (license license:expat)))

(define-public rust-objc-exception
  (package
    (name "rust-objc-exception")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "objc-exception" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1p00damjvy4nbfmrc90d9kbdygycrk76kq1s8v9k1hm35ydd5309"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gcc" ,rust-gcc))))
    (inputs
     `(("gcc-objc" ,gcc-objc)))
    (home-page "http://github.com/SSheldon/rust-objc-exception")
    (synopsis
      "Rust interface for Objective-C's throw and try/catch statements.")
    (description
      "Rust interface for Objective-C's throw and try/catch statements.")
    (license license:expat)))

(define-public rust-object
  (package
    (name "rust-object")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "object" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05gfh6da1aavksxxq52hpzy40yiqpwzra8lfk7pcc45qqrdw97nq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-goblin" ,rust-goblin)
         ("rust-scroll" ,rust-scroll)
         ("rust-target-lexicon" ,rust-target-lexicon)
         ("rust-uuid" ,rust-uuid)
         ("rust-crc32fast" ,rust-crc32fast)
         ("rust-flate2" ,rust-flate2)
         ("rust-indexmap" ,rust-indexmap)
         ("rust-parity-wasm" ,rust-parity-wasm))
        #:cargo-development-inputs
        (("rust-memmap" ,rust-memmap))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
      "A unified interface for parsing object file formats.")
    (description
      "A unified interface for parsing object file formats.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-object-0.12
  (package
    (name "rust-object")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "object" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dch1ajjp05d16lig1dnvisfis0hrlrvw9lcwy1hwgdcym3z6jnz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-flate2" ,rust-flate2)
         ("rust-goblin" ,rust-goblin-0.0.22)
         ("rust-parity-wasm" ,rust-parity-wasm)
         ("rust-scroll" ,rust-scroll)
         ("rust-uuid" ,rust-uuid))
        #:cargo-development-inputs
        (("rust-memmap" ,rust-memmap))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
      "A unified interface for parsing object file formats.")
    (description
      "This package provides a unified interface for parsing object file formats.")
    (license #f)))

(define-public rust-odds
  (package
    (name "rust-odds")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "odds" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rdnxa0na4897yb0svb3figz35g4imxjv61yfm2j21gbh5q8v8d9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rawpointer" ,rust-rawpointer)
         ("rust-rawslice" ,rust-rawslice)
         ("rust-unchecked-index" ,rust-unchecked-index))
        #:cargo-development-inputs
        (("rust-itertools" ,rust-itertools)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-memchr" ,rust-memchr)
         ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page
      "https://github.com/bluss/odds")
    (synopsis
      "Odds and ends")
    (description
      "Odds and ends  collection miscellania. Extra functionality for slices (`.find()`, `RevSlice`), strings and other things. Things in odds may move to more appropriate crates if we find them.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openblas-src
  (package
    (name "rust-openblas-src")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openblas-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dyf7yh6rmkk7k3pgcp5p8248f08hhajkigw42bfwjw1d3jk6d8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-libc" ,rust-libc))
       #:cargo-build-flags '("--release" "--features=system")
       #:cargo-test-flags '("--release" "--features=system")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-openblas
           (lambda _
             (delete-file-recursively "source")
             #t)))))
    (inputs
     `(("gfortran" ,gfortran "lib")
       ("openblas" ,openblas)))
    (home-page "https://github.com/blas-lapack-rs/openblas-src")
    (synopsis "Source of BLAS and LAPACK via OpenBLAS")
    (description
     "The package provides a source of BLAS and LAPACK via OpenBLAS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openblas-src-0.6
  (package
    (inherit rust-openblas-src)
    (name "rust-openblas-src")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openblas-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1bbs55s8bz2z0gcj7kh9ykxqn3x79m4cnmip7r6n5w4msyinalmg"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-libc" ,rust-libc))
       #:cargo-build-flags '("--release" "--features=system")
       #:cargo-test-flags '("--release" "--features=system")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-openblas
           (lambda _
             (delete-file-recursively "source")
             #t)))))
    (inputs
     `(("gfortran" ,gfortran "lib")
       ("openblas" ,openblas)))))

(define-public rust-openssl
  (package
    (name "rust-openssl")
    (version "0.10.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "05dg25rmg17rl3ykfl2yf69ghfd5z6zf6di38qw1awjvkddbnll1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-cfg-if" ,rust-cfg-if)
         ("rust-foreign-types" ,rust-foreign-types) ; 0.3
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-libc" ,rust-libc)
         ("rust-openssl-sys" ,rust-openssl-sys))
        #:cargo-development-inputs
        (("rust-hex" ,rust-hex)
         ("rust-tempdir" ,rust-tempdir))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             #t)))))
    (inputs
     `(("openssl" ,openssl)))
    (home-page
      "https://github.com/sfackler/rust-openssl")
    (synopsis "OpenSSL bindings")
    (description "OpenSSL bindings")
    (license license:asl2.0)))

(define-public rust-ordermap
  (package
    (name "rust-ordermap")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ordermap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1m0vxmlm1x92m1ydgpddzg5mrfk3ddy8gk3r9dmpml18qrs9ch4i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde))
       #:cargo-development-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-itertools" ,rust-itertools-0.7)
        ("rust-lazy-static" ,rust-lazy-static)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/bluss/ordermap")
    (synopsis
      "A hash table with consistent order and fast iteration. NOTE: This crate was renamed to indexmap. Please use it under its new name.")
    (description
      "A hash table with consistent order and fast iteration. NOTE: This crate was renamed to indexmap. Please use it under its new name.")
    (properties `((superseded . ,rust-indexmap)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ordermap-0.3
  (package
    (inherit rust-ordermap)
    (name "rust-ordermap")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ordermap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0qr0a50l0qakbjcck93qdayd0xl8gzpp42x0n7b75cs4ybsx6vm8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde))
       #:cargo-development-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-itertools" ,rust-itertools-0.7)
        ("rust-lazy-static" ,rust-lazy-static)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde-test" ,rust-serde-test))))))

(define-public rust-osmesa-sys
  (package
    (name "rust-osmesa-sys")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "osmesa-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fq1q1zcgfb0qydrg9r2738jlwc4hqxgb9vj11z72bjxx7kfrkw8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-shared-library" ,rust-shared-library))))
    (home-page
      "https://github.com/Daggerbot/osmesa-rs.git")
    (synopsis "OSMesa library bindings for Rust")
    (description "OSMesa library bindings for Rust")
    (license license:cc0)))

(define-public rust-output-vt100
  (package
    (name "rust-output-vt100")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "output-vt100" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ygqplpxz4gg3i8f3rkan2q69pqll7gv65l2mmd8r9dphnvwbkak"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/Phundrak/output-vt100-rs")
    (synopsis
      "Utility to activate escape codes in Windows' CMD and PowerShell")
    (description
      "Utility to activate escape codes in Windows' CMD and PowerShell")
    (license license:expat)))

(define-public rust-packed-simd
  (package
    (name "rust-packed-simd")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "packed_simd" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0822wqf6kzw4ig9ykndg348w2bxkhs3x64brzsvdxh2a1pyajpm8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-core-arch" ,rust-core-arch)
         ("rust-sleef-sys" ,rust-sleef-sys))
        #:cargo-development-inputs
        (("rust-arrayvec" ,rust-arrayvec)
         ("rust-paste" ,rust-paste)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen)
         ("rust-wasm-bindgen-test"
          ,rust-wasm-bindgen-test))))
    (home-page
      "https://github.com/rust-lang-nursery/packed_simd")
    (synopsis "Portable Packed SIMD vectors")
    (description "Portable Packed SIMD vectors")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-parking-lot
  (package
    (name "rust-parking-lot")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lk2vq3hp88ygpgsrypdr3ss71fidnqbykva0csgxhmn5scb2hpq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lock-api" ,rust-lock-api-0.3)
         ("rust-parking-lot-core" ,rust-parking-lot-core))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-rand" ,rust-rand)
         ("rust-rustc-version" ,rust-rustc-version))))
    (home-page
      "https://github.com/Amanieu/parking_lot")
    (synopsis
      "More compact and efficient implementations of the standard synchronization primitives.")
    (description
      "More compact and efficient implementations of the standard synchronization primitives.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-parking-lot-0.7
  (package
    (inherit rust-parking-lot)
    (name "rust-parking-lot")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0dz32cqx9200n1lk3kwyb599vabfid3f8sj1aq85sw42s2pb8hdb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-lock-api" ,rust-lock-api)
        ("rust-parking-lot-core" ,rust-parking-lot-core-0.4))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.6))))))

(define-public rust-parking-lot-core
  (package
    (name "rust-parking-lot-core")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ay67dpnrn68ryyvp720m9i8hzp189fd4d6slrs1lvmcwywv2xmq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-backtrace" ,rust-backtrace)
         ("rust-cfg-if" ,rust-cfg-if)
         ("rust-cloudabi" ,rust-cloudabi)
         ("rust-libc" ,rust-libc)
         ("rust-petgraph" ,rust-petgraph)
         ("rust-redox-syscall" ,rust-redox-syscall)
         ("rust-smallvec" ,rust-smallvec)
         ("rust-thread-id" ,rust-thread-id)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-rustc-version" ,rust-rustc-version))))
    (home-page
      "https://github.com/Amanieu/parking_lot")
    (synopsis
      "An advanced API for creating custom synchronization primitives.")
    (description
      "An advanced API for creating custom synchronization primitives.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-parking-lot-core-0.4
  (package
    (inherit rust-parking-lot-core)
    (name "rust-parking-lot-core")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parking_lot_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1jcq8aq4wv9y5fip7jg12jdwjd5g5r3x857xdma8vcin769cgj4l"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-rustc-version" ,rust-rustc-version)
        ("rust-winapi" ,rust-winapi)
        ("rust-backtrace" ,rust-backtrace)
        ("rust-petgraph" ,rust-petgraph)
        ("rust-thread-id" ,rust-thread-id))))))

(define-public rust-paste
  (package
    (name "rust-paste")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ygs077hlq8qlx5y46sfgrmhlqqgkmvvhn4x3y10arawalf4ljhz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-paste-impl" ,rust-paste-impl)
         ("rust-proc-macro-hack" ,rust-proc-macro-hack))))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis
      "Macros for all your token pasting needs")
    (description
      "Macros for all your token pasting needs")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-paste-impl
  (package
    (name "rust-paste-impl")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "paste-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rkh8nixmb7r1y0mjnsz62p6r1bqah5ciri7bwhmgcmq4gk9drr6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro-hack" ,rust-proc-macro-hack)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/dtolnay/paste")
    (synopsis
      "Implementation detail of the `paste` crate")
    (description
      "Implementation detail of the `paste` crate")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-persistent
  (package
    (name "rust-persistent")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "persistent" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iblxjjzd0h784l5y573nw5z3pdb3330k69hh413agagkh0a13wf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-iron" ,rust-iron)
         ("rust-plugin" ,rust-plugin))))
    (home-page "https://github.com/iron/persistent")
    (synopsis "A set of middleware for sharing server-global data in Iron.")
    (description "A set of middleware for sharing server-global data in Iron.")
    (license license:expat)))

(define-public rust-pest
  (package
    (name "rust-pest")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "134686mwxm73asbiads53zfchqvvcrsrsyax2cghfcizmvg8ac4k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-ucd-trie" ,rust-ucd-trie))))
    (home-page "https://pest.rs/")
    (synopsis "The Elegant Parser")
    (description "The Elegant Parser")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pest-derive
  (package
    (name "rust-pest-derive")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1l5jfa6ril71cw5nsiw0r45br54dd8cj2r1nc2d1wq6wb3jilgc3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-pest" ,rust-pest)
         ("rust-pest-generator" ,rust-pest-generator))))
    (home-page "https://pest.rs/")
    (synopsis "pest's derive macro")
    (description "pest's derive macro")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pest-generator
  (package
    (name "rust-pest-generator")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ipnv77lqhj4d4fpfxi8m168lcjp482kszaknlardmpgqiv0a4k3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-pest" ,rust-pest)
         ("rust-pest-meta" ,rust-pest-meta)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page "https://pest.rs/")
    (synopsis "pest code generator")
    (description "pest code generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pest-meta
  (package
    (name "rust-pest-meta")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pest_meta" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kaprdz3jis9bjfwhri1zncbsvack5m3gx2g5flspdy7wxnyljgj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-maplit" ,rust-maplit)
        ("rust-pest" ,rust-pest)
        ("rust-sha-1" ,rust-sha-1))))
    (home-page "https://pest.rs/")
    (synopsis "pest meta language parser and validator")
    (description
      "pest meta language parser and validator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-petgraph
  (package
    (name "rust-petgraph")
    (version "0.4.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "petgraph" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kyfmca854s54jk26g2x1kjb04c3k7cjilaxyr0if8lhxv8mjdlw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fixedbitset" ,rust-fixedbitset)
         ("rust-ordermap" ,rust-ordermap-0.3)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-defmac" ,rust-defmac)
         ("rust-itertools" ,rust-itertools)
         ("rust-odds" ,rust-odds)
         ("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/petgraph/petgraph")
    (synopsis
      "Graph data structure library. Provides graph types and graph algorithms.")
    (description
      "Graph data structure library. Provides graph types and graph algorithms.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-phf
  (package
    (name "rust-phf")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "066xwv4dr6056a9adlkarwp4n94kbpwngbmd47ngm3cfbyw49nmk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-phf-macros" ,rust-phf-macros)
         ("rust-phf-shared" ,rust-phf-shared))
        #:tests? #f)) ; TODO: Fix tests
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis
      "Runtime support for perfect hash function data structures")
    (description
      "Runtime support for perfect hash function data structures")
    (license license:expat)))

(define-public rust-phf-codegen
  (package
    (name "rust-phf-codegen")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf-codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zjiblicfm0nrmr2xxrs6pnf6zz2394wgch6dcbd8jijkq98agmh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-phf-generator" ,rust-phf-generator)
         ("rust-phf-shared" ,rust-phf-shared))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis "Codegen library for PHF types")
    (description "Codegen library for PHF types")
    (license license:expat)))

(define-public rust-phf-generator
  (package
    (name "rust-phf-generator")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf-generator" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qi62gxk3x3whrmw5c4i71406icqk11qmpgln438p6qm7k4lqdh9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-phf-shared" ,rust-phf-shared)
         ("rust-rand" ,rust-rand-0.6))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis "PHF generation logic")
    (description "PHF generation logic")
    (license license:expat)))

(define-public rust-phf-macros
  (package
    (name "rust-phf-macros")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dzylcy14ksy60h265l433j9ra8xhg8xlq3pd5qk658m6f1mxd5x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-phf-generator" ,rust-phf-generator)
         ("rust-phf-shared" ,rust-phf-shared)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-compiletest-rs" ,rust-compiletest-rs))
        #:tests? #f)) ; TODO: Fix tests
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis
      "Macros to generate types in the phf crate")
    (description
      "Macros to generate types in the phf crate")
    (license license:expat)))

(define-public rust-phf-shared
  (package
    (name "rust-phf-shared")
    (version "0.7.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "phf-shared" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18371fla0vsj7d6d5rlfb747xbr2in11ar9vgv5qna72bnhp2kr3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-siphasher" ,rust-siphasher-0.2)
         ("rust-unicase" ,rust-unicase-1.4))))
    (home-page
      "https://github.com/sfackler/rust-phf")
    (synopsis "Support code shared by PHF libraries")
    (description
      "Support code shared by PHF libraries")
    (license license:expat)))

(define-public rust-precomputed-hash
  (package
    (name "rust-precomputed-hash")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "precomputed-hash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "075k9bfy39jhs53cb2fpb9klfakx2glxnf28zdw08ws6lgpq6lwj"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/emilio/precomputed-hash")
    (synopsis
      "A library intending to be a base dependency to expose a precomputed hash")
    (description
      "This package provides a library intending to be a base dependency to expose a precomputed hash")
    (license license:expat)))

(define-public rust-pretty-env-logger
  (package
    (name "rust-pretty-env-logger")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty_env_logger" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x4hyjlnvvhyk9m74iypzybm22w3dl2k8img4b956239n5vf8zki"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-chrono" ,rust-chrono)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log))))
    (home-page
      "https://github.com/seanmonstar/pretty-env-logger")
    (synopsis
      "A simple logger built on top off env_logger. It is configured via an environment variable and writes to standard error with nice colored output for log levels.")
    (description
      "A simple logger built on top off env_logger. It is configured via an environment variable and writes to standard error with nice colored output for log levels.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pretty-assertions
  (package
    (name "rust-pretty-assertions")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty_assertions" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09yl14gnmpygiqrdlsa64lcl4w6ydjl9m8jri6kgam0v9rjf309z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term)
        ("rust-ctor" ,rust-ctor)
        ("rust-difference" ,rust-difference)
        ("rust-output-vt100" ,rust-output-vt100))))
    (home-page
      "https://github.com/colin-kiegel/rust-pretty-assertions")
    (synopsis
      "When writing tests in Rust, you'll probably use assert_eq!(a, b) a lot.

      If such a test fails, it will present all the details of a and b. But you have to spot the differences yourself, which is not always straightforward, like here:")
    (description
      "When writing tests in Rust, you'll probably use assert_eq!(a, b) a lot.

      If such a test fails, it will present all the details of a and b. But you have to spot the differences yourself, which is not always straightforward, like here:")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pretty-assertions-0.5
  (package
    (inherit rust-pretty-assertions)
    (name "rust-pretty-assertions")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty-assertions" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ins6swkpxmrh8q5h96h8nv0497d3dclsiyx2lyvqi6py0q980is"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ansi-term" ,rust-ansi-term)
         ("rust-difference" ,rust-difference))))))

(define-public rust-pretty-assertions-0.2
  (package
    (inherit rust-pretty-assertions)
    (name "rust-pretty-assertions")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pretty-assertions" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b3nv70i16737w3qkk1q5vqswwnb19znz8r9v2kcg1qyhh3h0l8x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-difference" ,rust-difference-1))))))

(define-public rust-proc-macro2-1
  (package
    (inherit rust-proc-macro2)
    (name "rust-proc-macro2")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k5brpv0c20whjhjvyjwksg4fgdcgnxjx53248kfsw7cfk67gp5g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid))
        #:cargo-development-inputs
        (("rust-quote" ,rust-quote-1))))))

(define-public rust-proc-macro2-0.3.5
  (package
    (inherit rust-proc-macro2)
    (name "rust-proc-macro2")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1m0ksg6hbm46zblq0dpkwrg3n1h7n90yq1zcgwc6vpbfmr9pr6bp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-xid" ,rust-unicode-xid-0.1))))))
(define-public rust-proc-macro2-0.3
               rust-proc-macro2-0.3.5)

(define-public rust-proc-macro-nested
  (package
    (name "rust-proc-macro-nested")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-nested" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bmlksm8vl44wkwihmwr7jsjznhbg0n7aibcw1cs2jgjcp86x6in"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/proc-macro-hack")
    (synopsis "Support for nested proc-macro-hack invocations")
    (description "This crate provides support for nested proc-macro-hack
invocations.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-proptest
  (package
    (name "rust-proptest")
    (version "0.9.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proptest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17sjg8isas4qk85807c4panih9k0lwa4k1mbajhciw5c5q17w56g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bit-set" ,rust-bit-set)
         ("rust-bitflags" ,rust-bitflags)
         ("rust-byteorder" ,rust-byteorder)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-quick-error" ,rust-quick-error)
         ("rust-rand" ,rust-rand)
         ("rust-rand-chacha" ,rust-rand-chacha)
         ("rust-rand-xorshift" ,rust-rand-xorshift)
         ("rust-regex-syntax" ,rust-regex-syntax)
         ("rust-rusty-fork" ,rust-rusty-fork)
         ("rust-tempfile" ,rust-tempfile))
        #:cargo-development-inputs
        (("rust-regex" ,rust-regex))))
    (home-page
      "https://altsysrq.github.io/proptest-book/proptest/index.html")
    (synopsis
      "Hypothesis-like property-based testing and shrinking.")
    (description
      "Hypothesis-like property-based testing and shrinking.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-proc-macro-hack
  (package
    (name "rust-proc-macro-hack")
    (version "0.5.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-hack" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y2ix90qdvsfks4j1g5qnpyzaa6a4j6sdi35klqvm120378kaalq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-demo-hack" ,rust-demo-hack)
         ("rust-demo-hack-impl" ,rust-demo-hack-impl))))
    (home-page
      "https://github.com/dtolnay/proc-macro-hack")
    (synopsis
      "Procedural functionlike!() macros using only Macros 1.1")
    (description
      "Procedural functionlike!() macros using only Macros 1.1")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-proc-macro-hack-0.4
  (package
    (inherit rust-proc-macro-hack)
    (name "rust-proc-macro-hack")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro-hack" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fxn3qfhw76c518dfal2qqjwj5dbf0a1f7z0r5c4wd0igygg4fs6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro-hack-impl" ,rust-proc-macro-hack-impl))
        #:cargo-development-inputs
        (("rust-demo-hack" ,rust-demo-hack)
         ("rust-demo-hack-impl" ,rust-demo-hack-impl))))))

(define-public rust-publicsuffix
  (package
    (name "rust-publicsuffix")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "publicsuffix" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "07ddqpri1xy7nxg5b7z8w49gw90rkn4qjvr423b4y7ngdnlcpzjs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-error-chain" ,rust-error-chain)
         ("rust-idna" ,rust-idna)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex)
         ("rust-url" ,rust-url)
         ("rust-native-tls" ,rust-native-tls))
        #:cargo-development-inputs
        (("rust-rspec" ,rust-rspec))))
    (home-page
      "https://github.com/rushmorem/publicsuffix")
    (synopsis
      "Robust domain name parsing and RFC compliant email address validation")
    (description
      "Robust domain name parsing and RFC compliant email address validation")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pulldown-cmark
  (package
    (name "rust-pulldown-cmark")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mq3cgjwxlrv0p1svjg93m1jkybzyfrl9p0jwa76hx1352hks13p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-getopts" ,rust-getopts)
         ("rust-memchr" ,rust-memchr)
         ("rust-unicase" ,rust-unicase))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-html5ever" ,rust-html5ever)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex)
         ("rust-tendril" ,rust-tendril))))
    (home-page
      "https://github.com/raphlinus/pulldown-cmark")
    (synopsis "A pull parser for CommonMark")
    (description
      "This package provides a pull parser for CommonMark")
    (license license:expat)))

(define-public rust-pulldown-cmark-0.4
  (package
    (inherit rust-pulldown-cmark)
    (name "rust-pulldown-cmark")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1db8vlhm3n72051bkq4am80q28rfrh88796i3y9ajf5hhk3lrdyi"))))
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags)
         ("rust-getopts" ,rust-getopts)
         ("rust-memchr" ,rust-memchr)
         ("rust-unicase" ,rust-unicase)
         ;("rust-markup5ever" ,rust-markup5ever-0.7)
         )
        #:cargo-development-inputs
        (("rust-html5ever" ,rust-html5ever-0.22)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex)
         ("rust-tendril" ,rust-tendril))))))

(define-public rust-pulldown-cmark-0.2
  (package
    (inherit rust-pulldown-cmark)
    (name "rust-pulldown-cmark")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "05gfnqa0wzix5m17jrmgj0yyr9sflqm0knn79ndppsnhcan2zxgf"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-getopts" ,rust-getopts))
       #:tests? #f)))) ; Spec tests fail because of encoding errors.

(define-public rust-pulldown-cmark-0.1
  (package
    (inherit rust-pulldown-cmark)
    (name "rust-pulldown-cmark")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ckflr6w5vfvgb2xnzbnph9b6c0k8cfncm4a8bjzmbbcv9fgizfn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-0.9)
        ("rust-getopts" ,rust-getopts))
       #:tests? #f)))) ; Spec tests fail because of encoding errors.

(define-public rust-quantiles
  (package
    (name "rust-quantiles")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quantiles" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wjp16a3d4bmldq9w2wds0q4gjz4mnsqac3g38r6ryr6zc9sh3y1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page
      "https://github.com/postmates/quantiles")
    (synopsis
      "a collection of approximate quantile algorithms")
    (description
      "a collection of approximate quantile algorithms")
    (license license:expat)))

(define-public rust-quickcheck
  (package
    (name "rust-quickcheck")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0qi67dg7cf50i23ac7n5qhg4vhhsm6xznhpl2wsqv86s5x551jnm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-rand" ,rust-rand-0.6)
         ("rust-rand-core" ,rust-rand-core))))
    (home-page
      "https://github.com/BurntSushi/quickcheck")
    (synopsis
      "Automatic property based testing with shrinking.")
    (description
      "Automatic property based testing with shrinking.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-quickcheck-0.8
  (package
    (inherit rust-quickcheck)
    (name "rust-quickcheck")
    (version "0.8.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mkl4wnvvjk4m32aq3an4ayfyvnmbxnzcybfm7n3fbsndb1xjdcw"))))
    (arguments
      `(#:cargo-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-rand" ,rust-rand-0.6)
         ("rust-rand-core" ,rust-rand-core))))))

(define-public rust-quickcheck-0.7
  (package
    (inherit rust-quickcheck)
    (name "rust-quickcheck")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "05pqzja6fwdyrs1za5vmxb9ifb993knmpdsrs1fs2wyz9qz7slyl"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-log" ,rust-log)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-rand-core" ,rust-rand-core-0.2))))))

(define-public rust-quickcheck-0.6
  (package
    (inherit rust-quickcheck)
    (name "rust-quickcheck")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dyazm2fcq0v9fscq1a7597zsvdl9f0j8c2bfj1jm2nlzz2sn6y0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-log" ,rust-log)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-quickcheck-0.5
  (package
    (inherit rust-quickcheck)
    (name "rust-quickcheck")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1jzm1ygfbn4igaq14b9nipc8yvsn6c8panpgd1qiy5r2insjllyd"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.4)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))))))

(define-public rust-quickcheck-0.4
  (package
    (inherit rust-quickcheck)
    (name "rust-quickcheck")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "01hligcv1h4pvc8ykch65qjzi7jgcq2s462v69j27slc84fl3hh2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.3)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))))))

(define-public rust-quickcheck-0.3
  (package
    (inherit rust-quickcheck)
    (name "rust-quickcheck")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "01a6s6lmnjld9lahbl54qp7h7x2hnkkzhcyr2gdhbk460sj3scqb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.3)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))
       #:skip-build? #t)))) ; Package needs 'unicode' crate.

(define-public rust-quickcheck-0.2
  (package
    (inherit rust-quickcheck)
    (name "rust-quickcheck")
    (version "0.2.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quickcheck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1vb4acppaavlnchzc1jmn5wlkgir9x9gmhgp97bavyxxqxgsg1nh"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.3)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))
       #:skip-build? #t)))) ; Package needs 'unicode' crate.

(define-public rust-quine-mc-cluskey
  (package
    (name "rust-quine-mc-cluskey")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quine-mc-cluskey" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iazdlwffhrlksb8yhhs1prgwpa68rwjwqm4v26hr9hrswarcn07"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.3))))
    (home-page
      "https://github.com/oli-obk/quine-mc_cluskey")
    (synopsis
      "Rust implementation of the Quine-McCluskey algorithm and Petrick's method")
    (description
      "Rust implementation of the Quine-McCluskey algorithm and Petrick's method")
    (license license:expat)))

(define-public rust-quote-1
  (package
    (inherit rust-quote)
    (name "rust-quote")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zkc46ryacf2jdkc6krsy2z615xbk1x8kp1830rcxz3irj5qqfh5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1))
       #:cargo-development-inputs
       (("rust-rustversion" ,rust-rustversion)
        ("rust-trybuild" ,rust-trybuild))))))

(define-public rust-quote-0.5
  (package
    (inherit rust-quote)
    (name "rust-quote")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1s01fh0jl8qv4xggs85yahw0h507nzrxkjbf7vay3zw8d3kcyjcr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.3))))))

(define-public rust-quote-0.3
  (package
    (inherit rust-quote)
    (name "rust-quote")
    (version "0.3.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0yhnnix4dzsv8y4wwz4csbnqjfh73al33j35msr10py6cl5r4vks"))))))

(define-public rust-rand
  (package
    (name "rust-rand")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0b05gwx8nnxr9bydyjxd1rszdvqnm946ky15z103ssfrhc7anznl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom)
        ("rust-libc" ,rust-libc)
        ("rust-log" ,rust-log)
        ("rust-packed-simd" ,rust-packed-simd)
        ("rust-rand-chacha" ,rust-rand-chacha)
        ("rust-rand-core" ,rust-rand-core)
        ("rust-rand-hc" ,rust-rand-hc)
        ("rust-rand-pcg" ,rust-rand-pcg))
       #:cargo-development-inputs
       (("rust-rand-hc" ,rust-rand-hc)
        ("rust-rand-isaac" ,rust-rand-isaac)
        ("rust-rand-pcg" ,rust-rand-pcg)
        ("rust-rand-xorshift" ,rust-rand-xorshift)
        ("rust-rand-xoshiro" ,rust-rand-xoshiro))))
    (home-page "https://crates.io/crates/rand")
    (synopsis
      "Random number generators and other randomness functionality.")
    (description
      "Random number generators and other randomness functionality.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-0.6
  (package
    (inherit rust-rand)
    (name "rust-rand")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1jl4449jcl4wgmzld6ffwqj5gwxrp8zvx8w573g1z368qg6xlwbd"))))
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg)
        ("rust-libc" ,rust-libc)
        ("rust-log" ,rust-log)
        ("rust-packed-simd" ,rust-packed-simd)
        ("rust-rand-chacha" ,rust-rand-chacha-0.1)
        ("rust-rand-core" ,rust-rand-core-0.4)
        ("rust-rand-hc" ,rust-rand-hc-0.1)
        ("rust-rand-isaac" ,rust-rand-isaac-0.1)
        ("rust-rand-jitter" ,rust-rand-jitter-0.1)
        ("rust-rand-os" ,rust-rand-os-0.1)
        ("rust-rand-pcg" ,rust-rand-pcg-0.1)
        ("rust-rand-xorshift" ,rust-rand-xorshift-0.1)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-average" ,rust-average-0.9)
        ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.1))))))

(define-public rust-rand-0.5
  (package
    (inherit rust-rand)
    (name "rust-rand")
    (version "0.5.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1fdcgja9167hlzkf4g5daqwp498lwiyq7aqm05whklpbsdyc8666"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cloudabi" ,rust-cloudabi)
        ("rust-fuchsia-cprng" ,rust-fuchsia-cprng)
        ("rust-libc" ,rust-libc)
        ("rust-log" ,rust-log)
        ("rust-rand-core" ,rust-rand-core-0.3)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive)
        ("rust-stdweb" ,rust-stdweb)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode))))))

(define-public rust-rand-0.4
  (package
    (inherit rust-rand)
    (name "rust-rand")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "14qjfv3gggzhnma20k0sc1jf8y6pplsaq7n1j9ls5c8kf2wl0a2m"))))
    (arguments
     `(#:cargo-inputs
       (("rust-fuchsia-cprng" ,rust-fuchsia-cprng)
        ("rust-libc" ,rust-libc)
        ("rust-rand-core" ,rust-rand-core-0.3)
        ("rust-rdrand" ,rust-rdrand-0.4)
        ("rust-winapi" ,rust-winapi))))))

(define-public rust-rand-chacha
  (package
    (name "rust-rand-chacha")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "178d36jfkc4v95s25scc2vibj2hd2hlk64cs6id4hvzg89whd4z1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-c2-chacha" ,rust-c2-chacha)
         ("rust-rand-core" ,rust-rand-core))
        #:cargo-development-inputs
        (("rust-autocfg" ,rust-autocfg))))
    (home-page
      "https://crates.io/crates/rand_chacha")
    (synopsis "ChaCha random number generator")
    (description "ChaCha random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-chacha-0.1
  (package
    (inherit rust-rand-chacha)
    (name "rust-rand-chacha")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1vxwyzs4fy1ffjc8l00fsyygpiss135irjf7nyxgq2v0lqf3lvam"))))
    (arguments
      `(#:cargo-inputs
        (("rust-autocfg" ,rust-autocfg)
         ("rust-rand-core" ,rust-rand-core-0.3))))))

(define-public rust-rand-core
  (package
    (name "rust-rand-core")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06bdvx08v3rkz451cm7z59xwwqn1rkfh6v9ay77b14f8dwlybgch"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getrandom" ,rust-getrandom)
         ("rust-serde" ,rust-serde))))
    (home-page "https://crates.io/crates/rand_core")
    (synopsis
      "Core random number generator traits and tools for implementation.")
    (description
      "Core random number generator traits and tools for implementation.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-core-0.4
  (package
    (inherit rust-rand-core)
    (name "rust-rand-core")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1p09ynysrq1vcdlmcqnapq4qakl2yd1ng3kxh3qscpx09k2a6cww"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive))))))

(define-public rust-rand-core-0.3
  (package
    (inherit rust-rand-core)
    (name "rust-rand-core")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jzdgszfa4bliigiy4hi66k7fs3gfwi2qxn8vik84ph77fwdwvvs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand-core" ,rust-rand-core-0.4))))))

(define-public rust-rand-core-0.2
  (package
    (inherit rust-rand-core)
    (name "rust-rand-core")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wikbw2a36bz8ywjyycjrd7db6ra3yzj14zs1ysxz2fiqhia8q8r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand-core" ,rust-rand-core-0.3))))))

(define-public rust-rand-distr
  (package
    (name "rust-rand-distr")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_distr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08drhcw9k4a79pri3rd1vkv7v9cbm6cf4i342nai39f527c58zn3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand" ,rust-rand))
        #:cargo-development-inputs
        (("rust-average" ,rust-average)
         ("rust-rand-pcg" ,rust-rand-pcg))))
    (home-page "https://crates.io/crates/rand_distr")
    (synopsis
      "Sampling from random number distributions")
    (description
      "Sampling from random number distributions")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-hc
  (package
    (name "rust-rand-hc")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_hc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g31sqwpmsirdlwr0svnacr4dbqyz339im4ssl9738cjgfpjjcfa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core))))
    (home-page "https://crates.io/crates/rand_hc")
    (synopsis "HC128 random number generator")
    (description "HC128 random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-hc-0.1
  (package
    (inherit rust-rand-hc)
    (name "rust-rand-hc")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_hc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i0vl8q5ddvvy0x8hf1zxny393miyzxkwqnw31ifg6p0gdy6fh3v"))))
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core-0.3))))))

(define-public rust-rand-isaac
  (package
    (name "rust-rand-isaac")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_isaac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xlb9415x518ffkazxhvk8b04i9i548nva4i5l5s34crvjrv1xld"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode))))
    (home-page "https://crates.io/crates/rand_isaac")
    (synopsis "ISAAC random number generator")
    (description "ISAAC random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-isaac-0.1
  (package
    (inherit rust-rand-isaac)
    (name "rust-rand-isaac")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_isaac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "027flpjr4znx2csxk7gxb7vrf9c7y5mydmvg5az2afgisp4rgnfy"))))
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core-0.3)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode))))))

(define-public rust-rand-jitter
  (package
    (name "rust-rand-jitter")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_jitter" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mnjbfzj97g788jslz0k77bpsg6qjhz676cibk82ibbvgqp4sy43"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-log" ,rust-log)
         ("rust-rand-core" ,rust-rand-core)
         ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/rust-random/rand")
    (synopsis
      "Random number generator based on timing jitter")
    (description
      "Random number generator based on timing jitter")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-jitter-0.1
  (package
    (inherit rust-rand-jitter)
    (name "rust-rand-jitter")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_jitter" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16z387y46bfz3csc42zxbjq89vcr1axqacncvv8qhyy93p4xarhi"))))
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-log" ,rust-log)
         ("rust-rand-core" ,rust-rand-core-0.4)
         ("rust-winapi" ,rust-winapi))))))

(define-public rust-rand-os
  (package
    (name "rust-rand-os")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_os" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06is69f8rfzs620g5b54k6cgy5yaycrsyqg55flyfrsf8g88733f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getrandom" ,rust-getrandom)
         ("rust-rand-core" ,rust-rand-core))))
    (home-page "https://crates.io/crates/rand_os")
    (synopsis "OS backed Random Number Generator")
    (description "OS backed Random Number Generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-os-0.1
  (package
    (inherit rust-rand-os)
    (name "rust-rand-os")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_os" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wahppm0s64gkr2vmhcgwc0lij37in1lgfxg5rbgqlz0l5vgcxbv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cloudabi" ,rust-cloudabi)
        ("rust-fuchsia-cprng" ,rust-fuchsia-cprng)
        ("rust-getrandom" ,rust-getrandom)
        ("rust-libc" ,rust-libc)
        ("rust-log" ,rust-log)
        ("rust-rand-core" ,rust-rand-core-0.4)
        ("rust-rdrand" ,rust-rdrand-0.4)
        ("rust-stdweb" ,rust-stdweb)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen)
        ("rust-winapi" ,rust-winapi))))))

(define-public rust-rand-pcg
  (package
    (name "rust-rand-pcg")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_pcg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dljwilv3640l1c5vlg4isiq7qz8gqa2cjbvgv3p0p5wrd36669y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-autocfg" ,rust-autocfg)
         ("rust-rand-core" ,rust-rand-core)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode))))
    (home-page "https://crates.io/crates/rand_pcg")
    (synopsis
      "Selected PCG random number generators")
    (description
      "Selected PCG random number generators")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-pcg-0.1
  (package
    (inherit rust-rand-pcg)
    (name "rust-rand-pcg")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_pcg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0i0bdla18a8x4jn1w0fxsbs3jg7ajllz6azmch1zw33r06dv1ydb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg)
        ("rust-rand-core" ,rust-rand-core-0.4)
        ("rust-serde" ,rust-serde)
        ("rust-serde-derive" ,rust-serde-derive))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode))))))

(define-public rust-rand-xorshift
  (package
    (name "rust-rand-xorshift")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_xorshift" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a6wy76lc5fimm1n9n8fzhp4cfjwfwxh4hx63bg3vlh1d2w1dm3p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode))))
    (home-page
      "https://crates.io/crates/rand_xorshift")
    (synopsis "Xorshift random number generator")
    (description
      "Xorshift random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-xorshift-0.1
  (package
    (inherit rust-rand-xorshift)
    (name "rust-rand-xorshift")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_xorshift" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0p2x8nr00hricpi2m6ca5vysiha7ybnghz79yqhhx6sl4gkfkxyb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core-0.3)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode))))))

(define-public rust-rand-xoshiro
  (package
    (name "rust-rand-xoshiro")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_xoshiro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07w3qgrac8r356lz5vqff42rly6yd9vs3g5lx5pbn13rcmb05rqb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-rand-core" ,rust-rand-core)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode))))
    (home-page
      "https://crates.io/crates/rand_xoshiro")
    (synopsis
      "Xoshiro, xoroshiro and splitmix64 random number generators")
    (description
      "Xoshiro, xoroshiro and splitmix64 random number generators")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-xoshiro-0.1
  (package
    (inherit rust-rand-xoshiro)
    (name "rust-rand-xoshiro")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_xoshiro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ac9ha6ll8b6l1930bd99k29jrjpsbpddvr6ycrnbi5rkwb1id03"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder)
        ("rust-rand-core" ,rust-rand-core-0.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.6))))))

(define-public rust-rayon
  (package
    (name "rust-rayon")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "190hkbcdfvcphyyzkdg52zdia2y9d9yanpm072bmnzbn49p1ic54"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.6)
         ("rust-either" ,rust-either)
         ("rust-rayon-core" ,rust-rayon-core))
        #:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment)
         ("rust-docopt" ,rust-docopt)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-rand" ,rust-rand-0.6)
         ("rust-rand-xorshift" ,rust-rand-xorshift)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis
      "Simple work-stealing parallelism for Rust")
    (description
      "Simple work-stealing parallelism for Rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rayon-1.0
  (package
    (inherit rust-rayon)
    (name "rust-rayon")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wq41f15y05nlarijn9c1vxscxj5sazn3lhd6mmnicj5fzr18f1p"))))
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.2)
         ("rust-either" ,rust-either)
         ("rust-rand" ,rust-rand-0.5)
         ("rust-rayon-core" ,rust-rayon-core))
        #:cargo-development-inputs
        (("rust-docopt" ,rust-docopt)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))))))

(define-public rust-rayon-0.8
  (package
    (inherit rust-rayon)
    (name "rust-rayon")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1j2l9x98ma63qkh9w8zik0vcpwqf9cvc2ynh66ibjp36nq4gw55n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-rayon-core" ,rust-rayon-core))
       #:cargo-development-inputs
       (("rust-compiletest-rs" ,rust-compiletest-rs-0.2)
        ("rust-docopt" ,rust-docopt-0.7)
        ("rust-futures" ,rust-futures)
        ("rust-rand" ,rust-rand-0.3)
        ("rust-rustc-serialize" ,rust-rustc-serialize))
       #:tests? #f)))) ; Fails to compile compiletest-rs.

(define-public rust-rayon-core
  (package
    (name "rust-rayon-core")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ljva6blaf1wmzvg77h1i9pd0hsmsbbcmdk7sjbw7h2s8gw0vgpb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.6)
         ("rust-crossbeam-queue" ,rust-crossbeam-queue)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-num-cpus" ,rust-num-cpus))
        #:cargo-development-inputs
        (("rust-libc" ,rust-libc)
         ("rust-rand" ,rust-rand)
         ("rust-rand-xorshift" ,rust-rand-xorshift)
         ("rust-scoped-tls" ,rust-scoped-tls))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "Core APIs for Rayon")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rdrand
  (package
    (name "rust-rdrand")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rdrand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02dsyibvl1iygkmljr0ld9vjyp525q4mjy5j9yazrrkyvhvw7gvp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core))))
    (home-page
      "https://github.com/nagisa/rust_rdrand/")
    (synopsis
      "An implementation of random number generator based on rdrand and rdseed instructions")
    (description
      "An implementation of random number generator based on rdrand and rdseed instructions")
    (license license:isc)))

(define-public rust-rdrand-0.4
  (package
    (inherit rust-rdrand)
    (name "rust-rdrand")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rdrand" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cjq0kwx1bk7jx3kzyciiish5gqsj7620dm43dc52sr8fzmm9037"))))
    (arguments
      `(#:cargo-inputs
        (("rust-rand-core" ,rust-rand-core-0.3))))))

(define-public rust-redox-users
  (package
    (name "rust-redox-users")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_users" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a1q5jv76vj1mwmqf2mmhknmkpw5wndx91gjfgg7vs8p79621r9z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-argon2rs" ,rust-argon2rs)
         ("rust-failure" ,rust-failure)
         ("rust-rand-os" ,rust-rand-os)
         ("rust-redox-syscall" ,rust-redox-syscall))))
    (home-page
      "https://gitlab.redox-os.org/redox-os/users")
    (synopsis
      "A Rust library to access Redox users and groups functionality")
    (description
      "This package provides a Rust library to access Redox users and groups functionality")
    (license license:expat)))

(define-public rust-ref-cast
  (package
    (name "rust-ref-cast")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ref-cast" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jgj1zxaikqm030flpifbp517fy4z21lly6ysbwyciii39bkzcf1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ref-cast-impl" ,rust-ref-cast-impl))))
    (home-page "https://github.com/dtolnay/ref-cast")
    (synopsis
      "Safely cast &T to &U where the struct U contains a single field of type T.")
    (description
      "Safely cast &T to &U where the struct U contains a single field of type T.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ref-cast-impl
  (package
    (name "rust-ref-cast-impl")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ref-cast-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hw0frpzna5rf5szix56zyzd0vackcb3svj94ndj629xi75dkb32"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/dtolnay/ref-cast")
    (synopsis
      "Derive implementation for ref_cast::RefCast.")
    (description
      "Derive implementation for ref_cast::RefCast.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-regex
  (package
    (name "rust-regex")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k6jpx0bqlg6vb8rsdnlmcw2szczbl51j047w3blpa4qzn6xl8vb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-aho-corasick" ,rust-aho-corasick)
         ("rust-memchr" ,rust-memchr)
         ("rust-regex-syntax" ,rust-regex-syntax)
         ("rust-thread-local" ,rust-thread-local)
         ("rust-utf8-ranges" ,rust-utf8-ranges))
        #:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-quickcheck" ,rust-quickcheck-0.8)
         ("rust-rand" ,rust-rand))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
      "An implementation of regular expressions for Rust. This implementation uses
      finite automata and guarantees linear time matching on all inputs.")
    (description
      "An implementation of regular expressions for Rust.  This implementation uses
      finite automata and guarantees linear time matching on all inputs.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-regex-0.2
  (package
    (inherit rust-regex)
    (name "rust-regex")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1163ir1k5zjspirfjl4wqbviwrxlhmfwy95xxb69y4irkv4snack"))))
    (arguments
      `(#:cargo-inputs
        (("rust-aho-corasick" ,rust-aho-corasick-0.6)
         ("rust-memchr" ,rust-memchr)
         ("rust-regex-syntax" ,rust-regex-syntax-0.5)
         ("rust-thread-local" ,rust-thread-local)
         ("rust-utf8-ranges" ,rust-utf8-ranges))
        #:cargo-development-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-quickcheck" ,rust-quickcheck-0.6)
         ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-regex-0.1
  (package
    (inherit rust-regex)
    (name "rust-regex")
    (version "0.1.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14wjz7s65j0rcc7625gb71f0zk6lrv44hwj5wxplzv0rjz636dyq"))))
    (arguments
      `(#:cargo-inputs
        (("rust-regex-macros" ,rust-regex-macros))))))

(define-public rust-regex-automata
  (package
    (name "rust-regex-automata")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-automata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y89vkwd9z7797lsdsizvhw4lw7i1mhfx97a8315bhkh2wm3rdwj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-regex-syntax" ,rust-regex-syntax)
         ("rust-utf8-ranges" ,rust-utf8-ranges))
        #:cargo-development-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex)
         ("rust-serde" ,rust-serde)
         ("rust-serde-bytes" ,rust-serde-bytes)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-toml" ,rust-toml))))
    (home-page
      "https://github.com/BurntSushi/regex-automata")
    (synopsis
      "Automata construction and matching using regular expressions.")
    (description
      "Automata construction and matching using regular expressions.")
    (license #f)))

(define-public rust-regex-macros
  (package
    (name "rust-regex-macros")
    (version "0.1.38")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11h11ainpmrb01wfwvbgz2ardix2w5y1j5501f9zc7p3cp970n4y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-regex" ,rust-regex)
         ("rust-regex-syntax" ,rust-regex-syntax))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis
      "An implementation of statically compiled regular expressions for Rust.

      Unless you specifically need compile time regular expressions or a matching
      engine that is guaranteed not to allocate, you should temporarily prefer using
      the plain regex crate (since it is almost always faster).
      ")
      (description
        "An implementation of statically compiled regular expressions for Rust.

        Unless you specifically need compile time regular expressions or a matching
        engine that is guaranteed not to allocate, you should temporarily prefer using
        the plain regex crate (since it is almost always faster).
        ")
        (license #f)))

(define-public rust-regex-syntax-0.5
  (package
    (inherit rust-regex-syntax)
    (name "rust-regex-syntax")
    (version "0.5.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19zp25jr3dhmclg3qqjk3bh1yrn7bqi05zgr5v52szv3l97plw3x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ucd-util" ,rust-ucd-util))))))

(define-public rust-regex-syntax-0.3
  (package
    (inherit rust-regex-syntax)
    (name "rust-regex-syntax")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ms9hgdhhsxw9w920i7gipydvagf100bb56jbs192rz86ln01v7r"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.2)
        ("rust-rand" ,rust-rand-0.3))))))

(define-public rust-rental
  (package
    (name "rust-rental")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rental" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zw4cxyqf2w67kl5v6nrab0hg6qfaf5n3n6a0kxkkcdjk2zdwic5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rental-impl" ,rust-rental-impl)
         ("rust-stable-deref-trait"
          ,rust-stable-deref-trait))))
    (home-page "https://github.com/jpernst/rental")
    (synopsis
      "A macro to generate safe self-referential structs, plus premade types for common use cases.")
    (description
      "This package provides a macro to generate safe self-referential structs, plus premade types for common use cases.")
    (license #f)))

(define-public rust-rental-impl
  (package
    (name "rust-rental-impl")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rental-impl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pj0qgmvwwsfwyjqyjxzikkwbwc3vj7hm3hdykr47dy5inbnhpj7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page "https://www.jpernst.com")
    (synopsis
      "An implementation detail of rental. Should not be used directly.")
    (description
      "An implementation detail of rental.  Should not be used directly.")
    (license #f)))

(define-public rust-reqwest
  (package
    (name "rust-reqwest")
    (version "0.9.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "reqwest" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y4wvzl3pspd8drr2hf9kk107cjw455cb6p529sh90x58dhqjv8g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64" ,rust-base64)
         ("rust-bytes" ,rust-bytes)
         ("rust-cookie" ,rust-cookie)
         ("rust-cookie-store" ,rust-cookie-store-0.7)
         ("rust-encoding-rs" ,rust-encoding-rs)
         ("rust-flate2" ,rust-flate2)
         ("rust-futures" ,rust-futures)
         ("rust-http" ,rust-http)
         ("rust-hyper" ,rust-hyper)
         ("rust-log" ,rust-log)
         ("rust-mime" ,rust-mime)
         ("rust-mime-guess" ,rust-mime-guess)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.5)
         ("rust-time" ,rust-time)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-threadpool" ,rust-tokio-threadpool)
         ("rust-tokio-timer" ,rust-tokio-timer)
         ("rust-url" ,rust-url)
         ("rust-uuid" ,rust-uuid)
         ("rust-winreg" ,rust-winreg)
         ("rust-hyper-old-types" ,rust-hyper-old-types)
         ("rust-hyper-rustls" ,rust-hyper-rustls)
         ("rust-hyper-tls" ,rust-hyper-tls)
         ("rust-native-tls" ,rust-native-tls)
         ("rust-rustls" ,rust-rustls)
         ("rust-socks" ,rust-socks)
         ("rust-tokio-rustls" ,rust-tokio-rustls)
         ("rust-trust-dns-resolver" ,rust-trust-dns-resolver)
         ("rust-webpki-roots" ,rust-webpki-roots))
        #:cargo-development-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-doc-comment" ,rust-doc-comment)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-libflate" ,rust-libflate)
         ("rust-serde" ,rust-serde)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-tcp" ,rust-tokio-tcp))))
    (home-page
      "https://github.com/seanmonstar/reqwest")
    (synopsis
      "An ergonomic, batteries-included HTTP Client for Rust.")
    (description
      "An ergonomic, batteries-included HTTP Client for Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rgb
  (package
    (name "rust-rgb")
    (version "0.8.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rgb" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nzg8809n0p7g3giq3ca8wi77kcpzv1cihzq07i2kl8l281y9290"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-json" ,rust-serde-json))
        #:tests? #f)) ; internal::ops::test::test_add_overflow fails
    (home-page "https://lib.rs/crates/rgb")
    (synopsis
      "`struct RGB/RGBA/etc.` for sharing pixels between crates + convenience methods for color manipulation.
      Allows no-copy high-level interoperability. Also adds common convenience methods and implements standard Rust traits to make `RGB`/`RGBA` pixels and slices first-class Rust objects.")
          (description
            "`struct RGB/RGBA/etc.` for sharing pixels between crates + convenience methods for color manipulation.
            Allows no-copy high-level interoperability.  Also adds common convenience methods and implements standard Rust traits to make `RGB`/`RGBA` pixels and slices first-class Rust objects.")
    (license license:expat)))

(define-public rust-ring
  (package
    (name "rust-ring")
    (version "0.16.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ring" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mzpk6zmvlvzy021fh8b3xs2zl6c8mqdqfwqn7zlvc07g8qyhskr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cc" ,rust-cc)
         ("rust-libc" ,rust-libc)
         ("rust-spin" ,rust-spin)
         ("rust-untrusted" ,rust-untrusted)
         ("rust-web-sys" ,rust-web-sys)
         ("rust-winapi" ,rust-winapi)
         ("rust-lazy-static" ,rust-lazy-static))
        #:cargo-development-inputs
        (("rust-libc" ,rust-libc)
         ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test))))
    (home-page "https://github.com/briansmith/ring")
    (synopsis
      "Safe, fast, small crypto using Rust")
    (description
      "Safe, fast, small crypto using Rust")
    (license (license:non-copyleft "file://LICENSE"))))

(define-public rust-ron
  (package
    (name "rust-ron")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ron" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mb2bavvp8jg5wx0kx9n45anrsbjwhjzddim987bjaa11hg45kif"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64" ,rust-base64)
         ("rust-bitflags" ,rust-bitflags)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-bytes" ,rust-serde-bytes)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/ron-rs/ron")
    (synopsis "Rusty Object Notation")
    (description "Rusty Object Notation")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rspec
  (package
    (name "rust-rspec")
    (version "1.0.0-beta.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rspec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1abfzwkbxlwahb243k8d3fp6i135lx1aqmbfl79w9zlpng182ndk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-colored" ,rust-colored)
         ("rust-derive-new" ,rust-derive-new)
         ("rust-derive-builder" ,rust-derive-builder-0.5)
         ("rust-expectest" ,rust-expectest-0.9)
         ("rust-rayon" ,rust-rayon-0.8))
        #:cargo-development-inputs
        (("rust-clippy" ,rust-clippy))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ddont-require-rust-clippy
           (lambda _
             (substitute* "Cargo.toml"
               (("0.0.153") "0.0"))
             #t)))))
    (home-page "https://mackwic.github.io/rspec")
    (synopsis
      "Write Rspec-like tests with stable rust")
    (description
      "Write Rspec-like tests with stable rust")
    (license license:mpl2.0)))

(define-public rust-rspec-1.0.0-beta3
  (package
    (inherit rust-rspec)
    (name "rust-rspec")
    (version "1.0.0-beta.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rspec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qkafvyg3r3h4ffhb7bhzq54mxxbirn2hk693wxdv5zhdjx68a99"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-clippy" ,rust-clippy))))))

(define-public rust-rustc-test
  (package
    (name "rust-rustc-test")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-test" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a27mlcg0ck0hgsdvwk792x9z1k1qq1wj091f1l5yggbdbcsnx5w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getopts" ,rust-getopts)
         ("rust-libc" ,rust-libc)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-term" ,rust-term)
         ("rust-time" ,rust-time)
         ("rust-rustc-version" ,rust-rustc-version))
        #:cargo-development-inputs
        (("rust-rustc-version" ,rust-rustc-version))))
    (home-page
      "https://github.com/servo/rustc-test")
    (synopsis
      "A fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
    (description
      "This package provides a fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-version
  (package
    (name "rust-rustc-version")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-semver" ,rust-semver))))
    (home-page
      "https://github.com/Kimundi/rustc-version-rs")
    (synopsis
      "A library for querying the version of a installed rustc compiler")
    (description
      "This package provides a library for querying the version of a installed rustc compiler")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustls
  (package
    (name "rust-rustls")
    (version "0.16.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17n0fx3fpkg4fhpdplrdhkissnl003kj90vzbqag11vkpyqihnmj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64" ,rust-base64)
         ("rust-log" ,rust-log)
         ("rust-ring" ,rust-ring)
         ("rust-sct" ,rust-sct)
         ("rust-webpki" ,rust-webpki))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-env-logger" ,rust-env-logger)
         ("rust-log" ,rust-log)
         ("rust-tempfile" ,rust-tempfile)
         ("rust-webpki-roots" ,rust-webpki-roots))))
    (home-page "https://github.com/ctz/rustls")
    (synopsis
      "Rustls is a modern TLS library written in Rust.")
    (description
      "Rustls is a modern TLS library written in Rust.")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-ryu
  (package
    (name "rust-ryu")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15r9z2wzgbj04pks4jz7y6wif5xqhf1wqkl2nd7qrvn08ys68969"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-no-panic" ,rust-no-panic))
        #:cargo-development-inputs
        (("rust-num-cpus" ,rust-num-cpus)
         ("rust-rand" ,rust-rand-0.5))))
    (home-page "https://github.com/dtolnay/ryu")
    (synopsis
      "Fast floating point to string conversion")
    (description
      "Fast floating point to string conversion")
    (license (list license:asl2.0
                   license:boost1.0))))

(define-public rust-same-file-0.1
  (package
    (inherit rust-same-file)
    (name "rust-same-file")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "same-file" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19qpl6j8s3ph9jm8rh1k0wp2nkyw5ah34xly00vqcfx4v97s8cfr"))))
    (arguments
      `(#:cargo-inputs
        (("rust-kernel32-sys" ,rust-kernel32-sys)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.3))))))

(define-public rust-scroll
  (package
    (name "rust-scroll")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scroll" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10q3w86bn22xrjlfg1c90dfi9c26qjkzn26nad0i9z8pxwad311g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-scroll-derive" ,rust-scroll-derive)
         ("rust-rustc-version" ,rust-rustc-version))
        #:cargo-development-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-rayon" ,rust-rayon))))
    (home-page "https://github.com/m4b/scroll")
    (synopsis
      "A suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
    (description
      "This package provides a suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
    (license license:expat)))

(define-public rust-scroll-derive
  (package
    (name "rust-scroll-derive")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scroll_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jqg5mm8nvii6avl1z1rc89agzh2kwkppgpsnwfakxg78mnaj6lg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-scroll" ,rust-scroll))))
    (home-page
      "https://github.com/m4b/scroll_derive")
    (synopsis
      "A macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
    (description
      "This package provides a macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
    (license license:expat)))

(define-public rust-sct
  (package
    (name "rust-sct")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sct" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g4dz7las43kcpi9vqv9c6l1afjkdv3g3w3s7d2w7a7w77wjl173"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ring" ,rust-ring)
         ("rust-untrusted" ,rust-untrusted))))
    (home-page "https://github.com/ctz/sct.rs")
    (synopsis
      "Certificate transparency SCT verification library")
    (description
      "Certificate transparency SCT verification library")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-seahash ; guix upstreamable
  (package
    (name "rust-seahash")
    (version "3.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "seahash" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1pr8ijnxnp68ki4m4740yc5mr01zijf86yx07wbsqzwiyhghdmhq"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.redox-os.org/redox-os/seahash")
    (synopsis "Hash function with proven statistical guarantees")
    (description "This package provides a blazingly fast, portable hash
function with proven statistical guarantees.")
    (license license:expat)))

(define-public rust-security-framework
  (package
    (name "rust-security-framework")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hmdsdj061wk76g3fajbfjnw74p0q45hy8hfngp7diwy987kvrpf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-core-foundation" ,rust-core-foundation)
         ("rust-core-foundation-sys"
          ,rust-core-foundation-sys)
         ("rust-libc" ,rust-libc)
         ("rust-security-framework-sys"
          ,rust-security-framework-sys))
        #:cargo-development-inputs
        (("rust-hex" ,rust-hex)
         ("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://lib.rs/crates/security_framework")
    (synopsis
      "Security.framework bindings for macOS and iOS")
    (description
      "Security.framework bindings for macOS and iOS")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-select-rustc
  (package
    (name "rust-select-rustc")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "select-rustc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0daqd56smi93g59nz43n4mh3d8whr6j5pa8dmwlf8bd76mdy3cpx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page
      "https://github.com/dtolnay/rustversion")
    (synopsis
      "Conditional compilation according to rustc compiler version")
    (description
      "Conditional compilation according to rustc compiler version")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-semver
  (package
    (name "rust-semver")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-semver-parser" ,rust-semver-parser-0.7)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-crates-index" ,rust-crates-index)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-tempdir" ,rust-tempdir))))
    (home-page "https://docs.rs/crate/semver/")
    (synopsis
      "Semantic version parsing and comparison.")
    (description
      "Semantic version parsing and comparison.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde
  (package
    (name "rust-serde")
    (version "1.0.97")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wlvfs82flb3di86m3nzf1m4vkc78vqcwrk865s0ldhrvgz3ssyl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://serde.rs")
    (synopsis
      "A generic serialization/deserialization framework")
    (description
      "This package provides a generic serialization/deserialization framework")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-0.9
  (package
    (inherit rust-serde)
    (name "rust-serde")
    (version "0.9.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bsla8l5xr9pp5sirkal6mngxcq6q961km88jvf339j5ff8j7dil"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde-derive" ,rust-serde-derive-0.9))
        #:cargo-development-inputs
        (("rust-serde-derive" ,rust-serde-derive-0.9))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))))

(define-public rust-serde-0.8
  (package
    (inherit rust-serde)
    (name "rust-serde")
    (version "0.8.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1j4ajipn0sf4ya0crgcb94s848qp7mfc35n6d0q2rf8rk5skzbcx"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-clippy" ,rust-clippy))
       #:tests? #f)))) ; doc tests fail

(define-public rust-serde-big-array
  (package
    (name "rust-serde-big-array")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-big-array" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gkyqxk760mp1lfcg6lhjk95ajc89nr0qdd0vl4ic0g8pyxcy9mr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/est31/serde-big-array")
    (synopsis "Big array helper for serde.")
    (description "Big array helper for serde.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-bytes
  (package
    (name "rust-serde-bytes")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_bytes" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sc5n336i7q4fiij4l8f892zcirgybrbxzl8bp51qxzqdvdlgzxa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/serde-rs/bytes")
    (synopsis
      "Optimized handling of `&[u8]` and `Vec<u8>` for Serde")
    (description
      "Optimized handling of `&[u8]` and `Vec<u8>` for Serde")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-cbor
  (package
    (name "rust-serde-cbor")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_cbor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0jcb4j637vdlqk2z38jixaqmp6f92h36r17kclv5brjay32911ii"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-half" ,rust-half)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://github.com/pyfisch/cbor")
    (synopsis "CBOR support for serde.")
    (description "CBOR support for serde.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-codegen-internals
  (package
    (name "rust-serde-codegen-internals")
    (version "0.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-codegen-internals" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0004s3wlc85vi6hq62hq84cv5b6qbbin1n6hdaqj095xhg98p25w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-syn" ,rust-syn-0.11))))
    (home-page "https://serde.rs")
    (synopsis
      "AST representation used by Serde codegen. Unstable.")
    (description
      "AST representation used by Serde codegen.  Unstable.")
    (license #f)))

(define-public rust-serde-derive
  (package
    (name "rust-serde-derive")
    (version "1.0.97")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zq2qm2gabmpa57wxfxb09jl41nxccsk454715xjabzymlh0han2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))
        #:cargo-development-inputs
        (("rust-serde" ,rust-serde))))
    (home-page "https://serde.rs")
    (synopsis
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-derive-0.9
  (package
    (inherit rust-serde-derive)
    (name "rust-serde-derive")
    (version "0.9.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fkldf0lnl6pwxs00qpyp79m30qmfpi3bk0wm22211ylyikdi3wp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-quote" ,rust-quote-0.3)
         ("rust-serde-codegen-internals"
          ,rust-serde-codegen-internals)
         ("rust-syn" ,rust-syn))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))))

(define-public rust-serde-json
  (package
    (name "rust-serde-json")
    (version "1.0.40")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "010pa89zx07aqx1cwgw2a603wcp3q5n2iy0k71ppqbr8kwi4j705"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-indexmap" ,rust-indexmap)
        ("rust-itoa" ,rust-itoa)
        ("rust-ryu" ,rust-ryu)
        ("rust-serde" ,rust-serde))
       #:cargo-development-inputs
       (("rust-automod" ,rust-automod)
        ("rust-select-rustc" ,rust-select-rustc)
        ("rust-serde-bytes" ,rust-serde-bytes)
        ("rust-serde-derive" ,rust-serde-derive)
        ("rust-serde-stacker" ,rust-serde-stacker)
        ("rust-trybuild" ,rust-trybuild))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description
      "This package provides a JSON serialization file format")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-json-0.9
  (package
    (inherit rust-serde-json)
    (name "rust-serde-json")
    (version "0.9.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "188nbf56m7p6mnh3xd71rwqxd4g95lqh8gsl7mfy3lp7gd4cz2xd"))))
    (arguments
      `(#:cargo-inputs
        (("rust-dtoa" ,rust-dtoa)
         ("rust-itoa" ,rust-itoa-0.3)
         ("rust-linked-hash-map" ,rust-linked-hash-map-0.4)
         ("rust-num-traits" ,rust-num-traits-0.1)
         ("rust-serde" ,rust-serde-0.9))
        #:cargo-development-inputs
        (("rust-serde-derive" ,rust-serde-derive-0.9))))))

(define-public rust-serde-json-0.8
  (package
    (inherit rust-serde-json)
    (name "rust-serde-json")
    (version "0.8.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0k3bclzbvzhiscjydqzym887i8mkh726xkf8isf3lln3xplx5xv7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-dtoa" ,rust-dtoa-0.2)
        ("rust-itoa" ,rust-itoa-0.1)
        ("rust-num-traits" ,rust-num-traits-0.1)
        ("rust-serde" ,rust-serde-0.8)
        ("rust-clippy" ,rust-clippy)
        ("rust-linked-hash-map" ,rust-linked-hash-map-0.3))))))

(define-public rust-serde-stacker
  (package
    (name "rust-serde-stacker")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_stacker" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jn54i5m1mlc6nm47f96k85fgjs9mhpbbqa4dvd5xjbivkdw55ic"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-stacker" ,rust-stacker))
        #:cargo-development-inputs
        (("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/dtolnay/serde-stacker")
    (synopsis
      "Serde adapter that avoids stack overflow by dynamically growing the stack")
    (description
      "Serde adapter that avoids stack overflow by dynamically growing the stack")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-test
  (package
    (name "rust-serde-test")
    (version "1.0.97")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_test" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i17jdgy18rcp19v6h4shim27l99q3vc7pzr2xaa2g0rjq9p6lr2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://serde.rs")
    (synopsis
      "Token De/Serializer for testing De/Serialize implementations")
    (description
      "Token De/Serializer for testing De/Serialize implementations")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-test-0.9
  (package
    (inherit rust-serde-test)
    (name "rust-serde-test")
    (version "0.9.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-test" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "193mf0qkhvjywd06x6hhmkixlqcyfbpfwfmr75dp2b8xwzpsvxwf"))))
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde-0.9))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))))


(define-public rust-serde-test-0.8
  (package
    (inherit rust-serde-test)
    (name "rust-serde-test")
    (version "0.8.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-test" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1m939j7cgs7i58r6vxf0ffp3nbr8advr8p9dqa9w8zk0z2yks2qi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-0.8))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path = \"../serde\"") ""))
             #t)))))))

(define-public rust-serde-urlencoded
  (package
    (name "rust-serde-urlencoded")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_urlencoded" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15rcwfkff0md5i231m2ym5756ksw1mkh5b5g2rw72wsc5mzdgicy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dtoa" ,rust-dtoa)
        ("rust-itoa" ,rust-itoa)
        ("rust-serde" ,rust-serde)
        ("rust-url" ,rust-url))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://serde.rs")
    (synopsis
      "`x-www-form-urlencoded` meets Serde")
    (description
      "`x-www-form-urlencoded` meets Serde")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-urlencoded-0.5
  (package
    (inherit rust-serde-urlencoded)
    (name "rust-serde-urlencoded")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_urlencoded" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nhnzllx5xrij4x17g351n14md691r95mxr7sbpz4sl80n8xcbb4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-dtoa" ,rust-dtoa)
        ("rust-itoa" ,rust-itoa)
        ("rust-serde" ,rust-serde)
        ("rust-url" ,rust-url-1))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive))))))

(define-public rust-serde-yaml
  (package
    (name "rust-serde-yaml")
    (version "0.8.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_yaml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10mmjpnshgrwij01a13679nxy1hnh5yfr0343kh0y9p5j2d8mc1q"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-dtoa" ,rust-dtoa)
         ("rust-linked-hash-map" ,rust-linked-hash-map)
         ("rust-serde" ,rust-serde)
         ("rust-yaml-rust" ,rust-yaml-rust))
        #:cargo-development-inputs
        (("rust-serde-derive" ,rust-serde-derive)
         ("rust-unindent" ,rust-unindent)
         ("rust-version-sync" ,rust-version-sync))))
    (home-page
      "https://github.com/dtolnay/serde-yaml")
    (synopsis "YAML support for Serde")
    (description "YAML support for Serde")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-sha-1
  (package
    (name "rust-sha-1")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha-1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0s6fdy5wp3x4h2z4fcl2d9vjvrpzr87v4h49r51xcq8nm4qj35i3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-block-buffer" ,rust-block-buffer)
         ("rust-digest" ,rust-digest)
         ("rust-fake-simd" ,rust-fake-simd)
         ("rust-opaque-debug" ,rust-opaque-debug)
         ("rust-sha1-asm" ,rust-sha1-asm))
        #:cargo-development-inputs
        (("rust-digest" ,rust-digest)
         ("rust-hex-literal" ,rust-hex-literal))))
    (home-page
      "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-1 hash function")
    (description "SHA-1 hash function")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-sha1
  (package
    (name "rust-sha1")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha1" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03gs2q4m67rn2p8xcdfxhip6mpgahdwm12bnb3vh90ahv9grhy95"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-openssl" ,rust-openssl)
         ("rust-rand" ,rust-rand)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/mitsuhiko/rust-sha1")
    (synopsis
      "Minimal implementation of SHA1 for Rust.")
    (description
      "Minimal implementation of SHA1 for Rust.")
    (license license:bsd-3)))

(define-public rust-simd
  (package
    (name "rust-simd")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "simd" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dgpmfzd4favsckd5m0p6bna1dcgw19hjigkqcgwfhc4d05hxczj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))
        #:cargo-development-inputs
        (("rust-cfg-if" ,rust-cfg-if))
        #:skip-build? #t)) ; Unsupported as of rust-1.33 nightly
    (home-page "https://github.com/hsivonen/simd")
    (synopsis
      "`simd` offers limited cross-platform access to SIMD instructions on
      CPUs, as well as raw interfaces to platform-specific instructions.
      (To be obsoleted by the `std::simd` implementation RFC 2366.)
      ")
      (description
        "`simd` offers limited cross-platform access to SIMD instructions on
        CPUs, as well as raw interfaces to platform-specific instructions.
        (To be obsoleted by the `std::simd` implementation RFC 2366.)
        ")
        (license #f)))

(define-public rust-siphasher
  (package
    (name "rust-siphasher")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "siphasher" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17cj2ynbv5zs7fa8ylrw7a6pb260q53ccj091mj9xa6ix0745nl3"))))
    (build-system cargo-build-system)
    (home-page "https://docs.rs/siphasher")
    (synopsis
      "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (description
      "SipHash-2-4, SipHash-1-3 and 128-bit variants in pure Rust")
    (license #f)))

(define-public rust-siphasher-0.2
  (package
    (inherit rust-siphasher)
    (name "rust-siphasher")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "siphasher" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1b53m53l24lyhr505lwqzrpjyq5qfnic71mynrcfvm43rybf938b"))))))

(define-public rust-skeptic
  (package
    (name "rust-skeptic")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "skeptic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0rai61hbs65nbvbhqlk1nap5hlav5qx3zmjjjzh9rhgxagc8xyyn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytecount" ,rust-bytecount-0.4)
         ("rust-cargo-metadata" ,rust-cargo-metadata-0.6)
         ("rust-error-chain" ,rust-error-chain)
         ("rust-glob" ,rust-glob)
         ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.2)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-tempdir" ,rust-tempdir)
         ("rust-walkdir" ,rust-walkdir))
        #:cargo-development-inputs
        (("rust-unindent" ,rust-unindent))))
    (home-page
      "https://github.com/budziq/rust-skeptic")
    (synopsis
      "Test your Rust markdown documentation via Cargo")
    (description
      "Test your Rust markdown documentation via Cargo")
    (license #f)))

(define-public rust-sleef-sys
  (package
    (name "rust-sleef-sys")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sleef-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1881q2yc17j2m1yvh01447c93ws1mspnrj3k2nbvwbvcm8z81kkv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-libc" ,rust-libc))
        #:cargo-development-inputs
        (("rust-bindgen" ,rust-bindgen-0.46)
         ("rust-cmake" ,rust-cmake)
         ("rust-env-logger" ,rust-env-logger))))
    (home-page "https://github.com/gnzlbg/sleef-sys")
    (synopsis
      "Rust FFI bindings to the SLEEF Vectorized Math Library")
    (description
      "Rust FFI bindings to the SLEEF Vectorized Math Library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-slog
  (package
    (name "rust-slog")
    (version "2.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slog" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "16bv6zrdn1sm315vbnia02g31xvsmbjyz5gv3z0vrgxdli0cdj8w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-erased-serde" ,rust-erased-serde))))
    (home-page "https://github.com/slog-rs/slog")
    (synopsis
      "Structured, extensible, composable logging for Rust")
    (description
      "Structured, extensible, composable logging for Rust")
    (license (list license:asl2.0
                   license:mpl2.0
                   license:expat))))

(define-public rust-smallvec
  (package
    (name "rust-smallvec")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smallvec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dyl43rgzny79jjpgzi07y0ly2ggx1xwsn64csxj0j91bsf6lq5b"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode))))
    (home-page
      "https://github.com/servo/rust-smallvec")
    (synopsis
      "'Small vector' optimization for Rust: store up to a small number of items on the stack")
    (description
      "'Small vector' optimization for Rust: store up to a small number of items on the stack")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-socks
  (package
    (name "rust-socks")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socks" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hnbw4c4j7dn9n3bd1v7ddkdzlxlzkfw3z29da1nxlj6jgx4r9p6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi)
         ("rust-ws2-32-sys" ,rust-ws2-32-sys))))
    (home-page "https://github.com/sfackler/rust-socks")
    (synopsis
      "SOCKS proxy support for Rust.")
    (description
      "SOCKS proxy support for Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-speculate
  (package
    (name "rust-speculate")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "speculate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ph01n3fqkmnfr1wd13dqsi4znv06xy6p4h3hqqdzk81r0r5vd1w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn)
         ("rust-unicode-xid" ,rust-unicode-xid))))
    (home-page
      "https://github.com/utkarshkukreti/speculate.rs")
    (synopsis
      "An RSpec inspired minimal testing framework for Rust.")
    (description
      "An RSpec inspired minimal testing framework for Rust.")
    (license license:expat)))

(define-public rust-spin-0.4
  (package
    (inherit rust-spin)
    (name "rust-spin")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spin" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "07ywqn1vrpi3c43fmvsx7pawk9h3rb77yyqbnhap2micl454kb6f"))))))

(define-public rust-spmc
  (package
    (name "rust-spmc")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spmc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1rgcqgj6b3d0cshi7277akr2xk0cx11rkmviaahy7a3pla6l5a02"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-loom" ,rust-loom))))
    (home-page
      "https://github.com/seanmonstar/spmc")
    (synopsis
      "Simple SPMC channel")
    (description
      "Simple SPMC channel")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stackvector
  (package
    (name "rust-stackvector")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stackvector" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13249bnbqpq4v0m3z0igfdxqwj0pqz6xzc13g2bii424klw7h26f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-unreachable" ,rust-unreachable))
        #:cargo-development-inputs
        (("rust-rustc-version" ,rust-rustc-version))))
    (home-page
      "https://github.com/Alexhuszagh/rust-stackvector")
    (synopsis
      "StackVec: vector-like facade for stack-allocated arrays.")
    (description
      "StackVec: vector-like facade for stack-allocated arrays.")
    (license #f)))

(define-public rust-stdweb
  (package
    (name "rust-stdweb")
    (version "0.4.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1srsjarj36f57ibr63n70bgy250yb72a73g14b80wh7piki0r356"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-discard" ,rust-discard)
         ("rust-futures-channel-preview"
          ,rust-futures-channel-preview)
         ("rust-futures-core-preview"
          ,rust-futures-core-preview)
         ("rust-futures-executor-preview"
          ,rust-futures-executor-preview)
         ("rust-futures-util-preview"
          ,rust-futures-util-preview)
         ("rust-rustc-version" ,rust-rustc-version)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-stdweb-derive" ,rust-stdweb-derive)
         ("rust-stdweb-internal-macros"
          ,rust-stdweb-internal-macros)
         ("rust-stdweb-internal-runtime"
          ,rust-stdweb-internal-runtime)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen))
        #:cargo-development-inputs
        (("rust-rustc-version" ,rust-rustc-version)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-stdweb-internal-test-macro"
          ,rust-stdweb-internal-test-macro)
         ("rust-wasm-bindgen-test"
          ,rust-wasm-bindgen-test))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis
      "A standard library for the client-side Web")
    (description
      "This package provides a standard library for the client-side Web")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stdweb-derive
  (package
    (name "rust-stdweb-derive")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0c1rxx6rqcc4iic5hx320ki3vshpi8k58m5600iqzq4x2zcyn88f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Derive macros for the `stdweb` crate")
    (description
      "Derive macros for the `stdweb` crate")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stdweb-internal-macros
  (package
    (name "rust-stdweb-internal-macros")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-macros" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1yjrmkc6sb1035avic383pa3avk2s9k3n17yjcza8yb9nw47v3z6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base-x" ,rust-base-x)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-sha1" ,rust-sha1)
         ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis
      "Internal procedural macros for the `stdweb` crate")
    (description
      "Internal procedural macros for the `stdweb` crate")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stream-cipher
  (package
    (name "rust-stream-cipher")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stream-cipher" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1g1nd8r6pph70rzk5yyvg7a9ji7pkap9ddiqpp4v9xa9ys0bqqc8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-blobby" ,rust-blobby)
         ("rust-generic-array" ,rust-generic-array))))
    (home-page
      "https://github.com/RustCrypto/traits")
    (synopsis "Stream cipher traits")
    (description "Stream cipher traits")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-string
  (package
    (name "rust-string")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "string" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vaxz85ja52fn66akgvggb29wqa5bpj3y38syykpr1pbrjzi8hfj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-bytes" ,rust-bytes))))
    (home-page
      "https://github.com/carllerche/string")
    (synopsis
      "A UTF-8 encoded string with configurable byte storage.")
    (description
      "This package provides a UTF-8 encoded string with configurable byte storage.")
    (license license:expat)))

(define-public rust-string-cache
  (package
    (name "rust-string-cache")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "string-cache" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w4ya29jy33am40drn1bmms4rggvq5paibdr5rzjvbrwbakv7k4n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-new-debug-unreachable" ,rust-new-debug-unreachable)
         ("rust-phf-shared" ,rust-phf-shared)
         ("rust-precomputed-hash" ,rust-precomputed-hash)
         ("rust-serde" ,rust-serde)
         ("rust-string-cache-shared" ,rust-string-cache-shared))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.4)
         ("rust-string-cache-codegen" ,rust-string-cache-codegen))))
    (home-page "https://github.com/servo/string-cache")
    (synopsis
      "A string interning library for Rust, developed as part of the Servo project.")
    (description
      "This package provides a string interning library for Rust, developed as part of the Servo project.")
    (license #f)))

(define-public rust-string-cache-codegen
  (package
    (name "rust-string-cache-codegen")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "string-cache-codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ik78h9gs874i24rkyh0myg6x4ni2a9cazbv5yzs9yavnv8mxx7h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-phf-generator" ,rust-phf-generator)
         ("rust-phf-shared" ,rust-phf-shared)
         ("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-string-cache-shared" ,rust-string-cache-shared))))
    (home-page
      "https://github.com/servo/string-cache")
    (synopsis
      "A codegen library for string-cache, developed as part of the Servo project.")
    (description
      "This package provides a codegen library for string-cache, developed as part of the Servo project.")
    (license #f)))

(define-public rust-string-cache-shared
  (package
    (name "rust-string-cache-shared")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "string-cache-shared" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1z7dpdix1m42x6ddshdcpjf91ml9mhvnskmiv5kd8hcpq0dlv25i"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/string-cache")
    (synopsis
      "Code share between string_cache and string_cache_codegen.")
    (description
      "Code share between string_cache and string_cache_codegen.")
    (license #f)))

(define-public rust-strsim-0.6
  (package
    (inherit rust-strsim)
    (name "rust-strsim")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "151ngha649cyybr3j50qg331b206zrinxqz7fzw1ra8r0n0mrldl"))))))

(define-public rust-structopt
  (package
    (name "rust-structopt")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "structopt" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1mvfv1l8vp3y402fkl2wcl34hi7gmr4bqha13dfz2xf3kjzwvhhn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-clap" ,rust-clap)
        ("rust-structopt-derive" ,rust-structopt-derive))
       #:tests? #f)) ; test 'flatten' fails
    (home-page
      "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct.")
    (description
      "Parse command line argument by defining a struct.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-structopt-derive
  (package
    (name "rust-structopt-derive")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "structopt-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01sis9z5kqmyhvzbnmlzpdxcry99a0b9blypksgnhdsbm1hh40ak"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-heck" ,rust-heck)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn))))
    (home-page
      "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct, derive crate.")
    (description
      "Parse command line argument by defining a struct, derive crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-syn
  (package
    (name "rust-syn")
    (version "0.15.44")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1id5g6x6zihv3j7hwrw3m1jp636bg8dpi671r7zy3jvpkavb794w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-unicode-xid" ,rust-unicode-xid-0.1))
        #:cargo-development-inputs
        (("rust-insta" ,rust-insta)
         ("rust-rayon" ,rust-rayon)
         ("rust-ref-cast" ,rust-ref-cast)
         ("rust-regex" ,rust-regex)
         ("rust-termcolor" ,rust-termcolor)
         ("rust-walkdir" ,rust-walkdir))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-syn-0.15.25
  (package
    (inherit rust-syn)
    (name "rust-syn")
    (version "0.15.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1m9s5injm7pllivqsizsxccsj00871clw79x58v5m4r6jqynkdvi"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-unicode-xid" ,rust-unicode-xid-0.1))
       #:cargo-development-inputs
       (("rust-rayon" ,rust-rayon)
        ("rust-regex" ,rust-regex)
        ("rust-walkdir" ,rust-walkdir))))))

(define-public rust-syn-1
  (package
    (inherit rust-syn)
    (name "rust-syn")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1gw03w7lzrlqmp2vislcybikgl5wkhrqi6sy70w93xss2abhx1b6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-unicode-xid" ,rust-unicode-xid))
        #:cargo-development-inputs
        (("rust-insta" ,rust-insta)
         ("rust-rayon" ,rust-rayon)
         ("rust-ref-cast" ,rust-ref-cast)
         ("rust-regex" ,rust-regex)
         ("rust-termcolor" ,rust-termcolor)
         ("rust-walkdir" ,rust-walkdir))))))

(define-public rust-syn-0.11
  (package
    (inherit rust-syn)
    (name "rust-syn")
    (version "0.11.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b8x8jdsmj6r9ck7n1pg371526n1q90kx6rv6ivcb22w06wr3f6k"))))
    (arguments
      `(#:cargo-inputs
        (("rust-quote" ,rust-quote-0.3)
         ("rust-synom" ,rust-synom)
         ("rust-unicode-xid" ,rust-unicode-xid-0.0))
        #:cargo-development-inputs
        (("rust-syntex-pos" ,rust-syntex-pos)
         ("rust-syntex-syntax" ,rust-syntex-syntax)
         ("rust-tempdir" ,rust-tempdir)
         ("rust-walkdir" ,rust-walkdir-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*,") ","))
             #t)))))))


(define-public rust-synom
  (package
    (name "rust-synom")
    (version "0.11.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dj536sh5xxhan2h0znxhv0sl6sb7lvzmsmrc3nvl3h1v5p0d4x3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-unicode-xid" ,rust-unicode-xid-0.0))
        #:cargo-development-inputs
        (("rust-syn" ,rust-syn-0.11))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               (("path =.*") ""))
             #t)))
       #:tests? #f)) ; TODO: Fix tests
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Stripped-down Nom parser used by Syn")
    (description
      "Stripped-down Nom parser used by Syn")
    (license #f)))

(define-public rust-synstructure
  (package
    (name "rust-synstructure")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synstructure" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1spqy31qcss57mciklc4nky4v778fvqs9qwdjgvnmf0hr5ichcca"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn)
         ("rust-unicode-xid" ,rust-unicode-xid-0.1))
        #:cargo-development-inputs
        (("rust-synstructure-test-traits"
          ,rust-synstructure-test-traits))))
    (home-page
      "https://github.com/mystor/synstructure")
    (synopsis
      "Helper methods and macros for custom derives")
    (description
      "Helper methods and macros for custom derives")
    (license license:expat)))

(define-public rust-synstructure-0.10
  (package
    (inherit rust-synstructure)
    (name "rust-synstructure")
    (version "0.10.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synstructure" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0grirdkgh2wl4hf9a3nbiazpgccxgq54kn52ms0xrr6njvgkwd82"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn)
        ("rust-unicode-xid" ,rust-unicode-xid-0.1))
       #:cargo-development-inputs
       (("rust-synstructure-test-traits"
         ,rust-synstructure-test-traits))))))

(define-public rust-syntex-errors
  (package
    (name "rust-syntex-errors")
    (version "0.58.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syntex-errors" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "176vma7sjv6li17q7dsilryac66b76zyis9ampmff2hlsz1caz46"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-syntex-pos" ,rust-syntex-pos)
         ("rust-term" ,rust-term)
         ("rust-unicode-xid" ,rust-unicode-xid-0.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))
    (home-page "https://github.com/serde-rs/syntex")
    (synopsis "Backport of librustc_errors")
    (description "Backport of librustc_errors")
    (license #f)))

(define-public rust-syntex-syntax
  (package
    (name "rust-syntex-syntax")
    (version "0.58.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syntex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14f74l7yzwl6fr9i23k4j23k66qn0gakvhk4jjc9ipb3w6x4s3kf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-0.8)
         ("rust-log" ,rust-log-0.3)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-syntex-errors" ,rust-syntex-errors)
         ("rust-syntex-pos" ,rust-syntex-pos)
         ("rust-unicode-xid" ,rust-unicode-xid-0.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))
    (home-page "https://github.com/serde-rs/syntex")
    (synopsis "Backport of libsyntax")
    (description "Backport of libsyntax")
    (license #f)))

(define-public rust-syntex-pos
  (package
    (name "rust-syntex-pos")
    (version "0.58.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syntex-pos" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0iqhircpr723da1g97xrrj8smqqz3gxw91cf03sckasjzri4gb8k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rustc-serialize" ,rust-rustc-serialize))))
    (home-page "https://github.com/serde-rs/syntex")
    (synopsis "Backport of libsyntax_pos")
    (description "Backport of libsyntax_pos")
    (license #f)))

(define-public rust-takeable-option
  (package
    (name "rust-takeable-option")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "takeable-option" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "182axkm8pq7cynsfn65ar817mmdhayrjmbl371yqp8zyzhr8kbin"))))
    (build-system cargo-build-system)
    (home-page "")
    (synopsis "A small wrapper around option.")
    (description
      "This package provides a small wrapper around option.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-target-build-utils
  (package
    (name "rust-target-build-utils")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "target-build-utils" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p7713x4bpbwi11l196z1mi8ym8qj1cdnab1mm2ffpm2wi516g81"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-phf" ,rust-phf)
         ("rust-serde-json" ,rust-serde-json-0.9)
         ("rust-phf-codegen" ,rust-phf-codegen))
        #:cargo-development-inputs
        (("rust-phf-codegen" ,rust-phf-codegen))))
    (home-page
      "https://github.com/nagisa/target_build_utils.rs")
    (synopsis
      "DEPRECATED: Use Cargo environment variables `CARGO_CFG_TARGET_*`")
    (description
      "DEPRECATED: Use Cargo environment variables `CARGO_CFG_TARGET_*`")
    (license #f)))

(define-public rust-target-lexicon
  (package
    (name "rust-target-lexicon")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "target-lexicon" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jrdch22pvm8r9fwx6d051l4yhac16lq6sn4q5fc6ic95fcb82hv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,rust-failure)
        ("rust-failure-derive" ,rust-failure-derive)
        ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/CraneStation/target-lexicon")
    (synopsis
      "This is a library for managing targets for compilers and related tools.")
    (description
      "This is a library for managing targets for compilers and related tools.")
    (license license:asl2.0))) ; with LLVM exception

(define-public rust-tendril
  (package
    (name "rust-tendril")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tendril" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fsx7blrrzgca8aa2yqy8zxyi8s7amskhgkk1ml5sbaqyalyszvh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-encoding" ,rust-encoding)
         ("rust-encoding-rs" ,rust-encoding-rs)
         ("rust-futf" ,rust-futf)
         ("rust-mac" ,rust-mac)
         ("rust-utf-8" ,rust-utf-8))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/servo/tendril")
    (synopsis
      "Compact buffer/string type for zero-copy parsing")
    (description
      "Compact buffer/string type for zero-copy parsing")
    (license #f)))

(define-public rust-term-0.5
  (package
    (name "rust-term")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hkgjrfisj6zjwz525639pmsvzhlc48a0h65nw87qrdp6jihdlgd"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-dirs" ,rust-dirs)
         ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/Stebalien/term")
    (synopsis "A terminal formatting library")
    (description
      "This package provides a terminal formatting library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-term-size
  (package
    (name "rust-term-size")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "term_size" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09wk3173ngmb710qs9rwgibq4w250q8lgnwjvb9cypc1vdk9lnwy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-clippy" ,rust-clippy)
         ("rust-kernel32-sys" ,rust-kernel32-sys)
         ("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/clap-rs/term_size-rs")
    (synopsis
      "functions for determining terminal sizes and dimensions")
    (description
      "functions for determining terminal sizes and dimensions")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-test-assembler
  (package
    (name "rust-test-assembler")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "test-assembler" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sdx9hk0dk3z9crm8834ysyxsi92chls8arpd0gs796kis6lik2w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder))))
    (home-page
      "https://github.com/luser/rust-test-assembler")
    (synopsis
      "A set of types for building complex binary streams.")
    (description
      "This package provides a set of types for building complex binary streams.")
    (license license:expat)))

(define-public rust-tester
  (package
    (name "rust-tester")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tester" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xkgapz2i4j977f6kh1zp6sa5llbhy5vbnr6kfj8czsrdjr2r0ay"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getopts" ,rust-getopts)
         ("rust-libc" ,rust-libc)
         ("rust-term" ,rust-term))
        #:tests? #f)) ; 2 tests fail.
    (home-page "https://github.com/messense/rustc-test")
    (synopsis
      "A fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
    (description
      "This package provides a fork of Rustâ\x80\x99s `test` crate that doesnâ\x80\x99t require unstable language features.")
    (license (list license:expat license:asl2.0))))

(define-public rust-textwrap
  (package
    (name "rust-textwrap")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-hyphenation" ,rust-hyphenation)
         ("rust-term-size" ,rust-term-size)
         ("rust-unicode-width" ,rust-unicode-width))
        #:cargo-development-inputs
        (("rust-lipsum" ,rust-lipsum)
         ("rust-rand" ,rust-rand)
         ("rust-rand-xorshift" ,rust-rand-xorshift)
         ("rust-version-sync" ,rust-version-sync))))
    (home-page
      "https://github.com/mgeisler/textwrap")
    (synopsis
      "Textwrap is a small library for word wrapping, indenting, and
      dedenting strings.

      You can use it to format strings (such as help and error messages) for
      display in commandline applications. It is designed to be efficient
      and handle Unicode characters correctly.")
    (description
      "Textwrap is a small library for word wrapping, indenting, and
      dedenting strings.

      You can use it to format strings (such as help and error messages) for
      display in commandline applications.  It is designed to be efficient
      and handle Unicode characters correctly.")
    (license license:expat)))

(define-public rust-tinytemplate
  (package
    (name "rust-tinytemplate")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tinytemplate" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "084w41m75i95sdid1wwlnav80jsl1ggyryl4nawxvb6amigvfx25"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json))
        #:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2)
         ("rust-serde-derive" ,rust-serde-derive))))
    (home-page
      "https://github.com/bheisler/TinyTemplate")
    (synopsis "Simple, lightweight template engine")
    (description
      "Simple, lightweight template engine")
    (license (list license:asl2.0 license:expat))))

(define-public rust-tokio
  (package
    (name "rust-tokio")
    (version "0.1.22")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1xhaadfmm6m37f79xv5020gc3np9wqza3bq95ymp522qpfsw02as"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-mio" ,rust-mio)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-tokio-codec" ,rust-tokio-codec)
         ("rust-tokio-current-thread"
          ,rust-tokio-current-thread)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-fs" ,rust-tokio-fs)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-reactor" ,rust-tokio-reactor)
         ("rust-tokio-sync" ,rust-tokio-sync)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-tokio-threadpool" ,rust-tokio-threadpool)
         ("rust-tokio-timer" ,rust-tokio-timer)
         ("rust-tokio-udp" ,rust-tokio-udp)
         ("rust-tokio-uds" ,rust-tokio-uds)
         ("rust-tracing-core" ,rust-tracing-core))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-flate2" ,rust-flate2)
         ("rust-futures-cpupool" ,rust-futures-cpupool)
         ("rust-http" ,rust-http)
         ("rust-httparse" ,rust-httparse)
         ("rust-libc" ,rust-libc)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-time" ,rust-time))))
    (home-page "https://tokio.rs")
    (synopsis
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed applications.")
    (description
      "An event-driven, non-blocking I/O platform for writing asynchronous I/O backed applications.")
    (license license:expat)))

(define-public rust-tokio-buf
  (package
    (name "rust-tokio-buf")
    (version "0.2.0-alpha.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-buf" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bz2yb77kxq4006j6cjdkl14n21pi0c0mdw20ywj9yd70y7lap2z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-either" ,rust-either))
        #:cargo-development-inputs
        (("rust-tokio-mock-task" ,rust-tokio-mock-task))))
    (home-page "https://tokio.rs")
    (synopsis
      "Asynchronous stream of byte buffers")
    (description
      "Asynchronous stream of byte buffers")
    (license license:expat)))

(define-public rust-tokio-codec
  (package
    (name "rust-tokio-codec")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-codec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17y3hi3dd0bdfkrzshx9qhwcf49xv9iynszj7iwy3w4nmz71wl2w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-tokio-io" ,rust-tokio-io))))
    (home-page "https://tokio.rs")
    (synopsis
      "Utilities for encoding and decoding frames.")
    (description
      "Utilities for encoding and decoding frames.")
    (license license:expat)))

(define-public rust-tokio-core
  (package
    (name "rust-tokio-core")
    (version "0.1.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wbgg59mxfvrhzv97y56nh3gmnmw3jj9dhgkmvz27410jjxzpvxf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-iovec" ,rust-iovec)
         ("rust-log" ,rust-log)
         ("rust-mio" ,rust-mio)
         ("rust-scoped-tls" ,rust-scoped-tls-0.1)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-reactor" ,rust-tokio-reactor)
         ("rust-tokio-timer" ,rust-tokio-timer))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-flate2" ,rust-flate2)
         ("rust-futures-cpupool" ,rust-futures-cpupool)
         ("rust-http" ,rust-http)
         ("rust-httparse" ,rust-httparse)
         ("rust-libc" ,rust-libc)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-time" ,rust-time))))
    (home-page "https://tokio.rs")
    (synopsis
      "Core I/O and event loop primitives for asynchronous I/O in Rust. Foundation for
      the rest of the tokio crates.")
    (description
      "Core I/O and event loop primitives for asynchronous I/O in Rust.  Foundation for
      the rest of the tokio crates.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-current-thread
  (package
    (name "rust-tokio-current-thread")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-current-thread" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hx4c8v88kk0ih8x5s564gsgwwf8n11kryvxm72l1f7isz51fqni"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-tokio-executor" ,rust-tokio-executor))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis
      "Single threaded executor which manage many tasks concurrently on the current thread.")
    (description
      "Single threaded executor which manage many tasks concurrently on the current thread.")
    (license license:expat)))

(define-public rust-tokio-executor
  (package
    (name "rust-tokio-executor")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-executor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b5n6barppmhfyb1m2cvswp7nqvyrr3lb0kk545my75hdl7fw9qg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils)
         ("rust-futures" ,rust-futures))
        #:cargo-development-inputs
        (("rust-tokio" ,rust-tokio))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Future execution primitives")
    (description "Future execution primitives")
    (license license:expat)))

(define-public rust-tokio-fs
  (package
    (name "rust-tokio-fs")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-fs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1bxp8585pi4j5g39ci2gkk99qnyilyhhila7cs8r6scdn0idrriz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-threadpool" ,rust-tokio-threadpool))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand)
         ("rust-tempdir" ,rust-tempdir)
         ("rust-tempfile" ,rust-tempfile)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-codec" ,rust-tokio-codec)
         ("rust-tokio-io" ,rust-tokio-io))))
    (home-page "https://tokio.rs")
    (synopsis "Filesystem API for Tokio.")
    (description "Filesystem API for Tokio.")
    (license license:expat)))

(define-public rust-tokio-io
  (package
    (name "rust-tokio-io")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-io" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09jrz1hh4h1vj45qy09y7m7m8jsy1hl6g32clnky25mdim3dp42h"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-log" ,rust-log))
        #:cargo-development-inputs
        (("rust-tokio-current-thread"
          ,rust-tokio-current-thread))))
    (home-page "https://tokio.rs")
    (synopsis
      "Core I/O primitives for asynchronous I/O in Rust.")
    (description
      "Core I/O primitives for asynchronous I/O in Rust.")
    (license license:expat)))

(define-public rust-tokio-io-pool
  (package
    (name "rust-tokio-io-pool")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-io-pool" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "17lrjj7lcw13wchpbvr8cynmypd29h40clf9qxabh6fxva40kwm5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-executor" ,rust-tokio-executor))
        #:cargo-development-inputs
        (("rust-tokio-current-thread"
          ,rust-tokio-current-thread))))
    (home-page
      "https://github.com/jonhoo/tokio-io-pool")
    (synopsis
      "Alternative tokio thread pool for executing short, I/O-heavy futures efficiently")
    (description
      "Alternative tokio thread pool for executing short, I/O-heavy futures efficiently")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-reactor
  (package
    (name "rust-tokio-reactor")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-reactor" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1khip64cn63xvayq1db68kxcnhgw3cb449a4n2lbw4p1qzx6pwba"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils)
         ("rust-futures" ,rust-futures)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-log" ,rust-log)
         ("rust-mio" ,rust-mio)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-parking-lot" ,rust-parking-lot-0.7)
         ("rust-slab" ,rust-slab)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-sync" ,rust-tokio-sync))
        #:cargo-development-inputs
        (("rust-num-cpus" ,rust-num-cpus)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-io-pool" ,rust-tokio-io-pool))))
    (home-page "https://tokio.rs")
    (synopsis
      "Event loop that drives Tokio I/O resources.")
    (description
      "Event loop that drives Tokio I/O resources.")
    (license license:expat)))

(define-public rust-tokio-mockstream
  (package
    (name "rust-tokio-mockstream")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-mockstream" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mg1i39cl8x32wxwbn74hlirks8a6f3g0gfzkb0n0zwbxwvc9gs1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-tokio-io" ,rust-tokio-io))
        #:cargo-development-inputs
        (("rust-bytes" ,rust-bytes))))
    (home-page
      "https://github.com/aatxe/tokio-mockstream")
    (synopsis
      "A fake stream for testing network applications backed by buffers.")
    (description
      "A fake stream for testing network applications backed by buffers.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-openssl
  (package
    (name "rust-tokio-openssl")
    (version "0.4.0-alpha.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-openssl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0zzhb720bmjkcg5q53yp9mimx8frnbrk9il6rya16zc6pwmzbfw8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-openssl" ,rust-openssl)
         ("rust-tokio-io" ,rust-tokio-io))
        #:cargo-development-inputs
        (("rust-futures-preview" ,rust-futures-preview)
         ("rust-tokio" ,rust-tokio))))
    (home-page
      "https://github.com/alexcrichton/tokio-openssl")
    (synopsis
      "An implementation of SSL streams for Tokio backed by OpenSSL")
    (description
      "An implementation of SSL streams for Tokio backed by OpenSSL")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-rustls
  (package
    (name "rust-tokio-rustls")
    (version "0.12.0-alpha.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vmjqdpvvwi5xga8lrp9pr29i7jd77zzlbbv4vi2mnsqxjafdcji"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures-core-preview"
          ,rust-futures-core-preview)
         ("rust-rustls" ,rust-rustls)
         ("rust-smallvec" ,rust-smallvec)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-webpki" ,rust-webpki))
        #:cargo-development-inputs
        (("rust-futures-util-preview"
          ,rust-futures-util-preview)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-tokio" ,rust-tokio)
         ("rust-webpki-roots" ,rust-webpki-roots))))
    (home-page
      "https://github.com/quininer/tokio-rustls")
    (synopsis
      "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description
      "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-sync
  (package
    (name "rust-tokio-sync")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-sync" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ryalh7dcmnz46xj1va8aaw3if6vd4mj87r67dqvrqhpyf7j8qi1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-futures" ,rust-futures))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-loom" ,rust-loom)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-mock-task" ,rust-tokio-mock-task))))
    (home-page "https://tokio.rs")
    (synopsis "Synchronization utilities.")
    (description "Synchronization utilities.")
    (license license:expat)))

(define-public rust-tokio-tcp
  (package
    (name "rust-tokio-tcp")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-tcp" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06a15vg8bcd33ng3h9ldzlq7wl4jsw0p9qpy7v22ls5yah3b250x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-iovec" ,rust-iovec)
         ("rust-mio" ,rust-mio)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-reactor" ,rust-tokio-reactor))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-tokio" ,rust-tokio))))
    (home-page "https://tokio.rs")
    (synopsis "TCP bindings for tokio.")
    (description "TCP bindings for tokio.")
    (license license:expat)))

(define-public rust-tokio-threadpool
  (package
    (name "rust-tokio-threadpool")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-threadpool" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06av6vdkgb48v6xaaci5agfydg9fsj9c338y01m3f7paklqh3jlh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-deque" ,rust-crossbeam-deque)
         ("rust-crossbeam-queue" ,rust-crossbeam-queue)
         ("rust-crossbeam-utils" ,rust-crossbeam-utils)
         ("rust-futures" ,rust-futures)
         ("rust-log" ,rust-log)
         ("rust-num-cpus" ,rust-num-cpus)
         ("rust-rand" ,rust-rand)
         ("rust-slab" ,rust-slab)
         ("rust-tokio-executor" ,rust-tokio-executor))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-futures-cpupool" ,rust-futures-cpupool)
         ("rust-threadpool" ,rust-threadpool))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis
      "A task scheduler backed by a work-stealing thread pool.")
    (description
      "This package provides a task scheduler backed by a work-stealing thread pool.")
    (license license:expat)))

(define-public rust-tokio-timer
  (package
    (name "rust-tokio-timer")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-timer" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03m68ainkdy3b5pf20rjyknhk2ppx35bjdc2yfj2bv80sl96h47j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-crossbeam-utils" ,rust-crossbeam-utils)
         ("rust-futures" ,rust-futures)
         ("rust-slab" ,rust-slab)
         ("rust-tokio-executor" ,rust-tokio-executor))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-mock-task" ,rust-tokio-mock-task))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Timer facilities for Tokio")
    (description "Timer facilities for Tokio")
    (license license:expat)))

(define-public rust-tokio-tls
  (package
    (name "rust-tokio-tls")
    (version "0.3.0-alpha.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z9bbxkd646lsn1fr1a5znxdz8afbpy31iq1knxd424v57lxf29p"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-native-tls" ,rust-native-tls)
         ("rust-tokio-io" ,rust-tokio-io))))
    (home-page "https://tokio.rs")
    (synopsis
      "An implementation of TLS/SSL streams for Tokio giving an implementation of TLS for nonblocking I/O streams.")
    (description
      "An implementation of TLS/SSL streams for Tokio giving an implementation of TLS for nonblocking I/O streams.")
    (license license:expat)))

(define-public rust-tokio-udp
  (package
    (name "rust-tokio-udp")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-udp" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "14kfj35s465czcspayacnzlxrazfvxzhhggq1rqlljhgp1sqa9k6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-log" ,rust-log)
         ("rust-mio" ,rust-mio)
         ("rust-tokio-codec" ,rust-tokio-codec)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-reactor" ,rust-tokio-reactor))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger))))
    (home-page "https://tokio.rs")
    (synopsis "UDP bindings for tokio.")
    (description "UDP bindings for tokio.")
    (license license:expat)))

(define-public rust-tokio-uds
  (package
    (name "rust-tokio-uds")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-uds" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0i94kxma6l7iy5hd5k7nvn7v9pnyw0s54bm9mjs0lap1l0xzqzq3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-futures" ,rust-futures)
         ("rust-iovec" ,rust-iovec)
         ("rust-libc" ,rust-libc)
         ("rust-log" ,rust-log)
         ("rust-mio" ,rust-mio)
         ("rust-mio-uds" ,rust-mio-uds)
         ("rust-tokio-codec" ,rust-tokio-codec)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-reactor" ,rust-tokio-reactor))
        #:cargo-development-inputs
        (("rust-tempfile" ,rust-tempfile)
         ("rust-tokio" ,rust-tokio))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Unix Domain sockets for Tokio")
    (description "Unix Domain sockets for Tokio")
    (license license:expat)))

(define-public rust-toml
  (package
    (name "rust-toml")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0fdhfyv20qn29cxq1f9j9wav92c3qhxdm71sxfyzhzpsfdw6vjdq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-linked-hash-map" ,rust-linked-hash-map)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/alexcrichton/toml-rs")
    (synopsis
      "A native Rust encoder and decoder of TOML-formatted files and streams. Provides implementations of the standard Serialize/Deserialize traits for TOML data to facilitate deserializing and serializing Rust structures.")
    (description
      "A native Rust encoder and decoder of TOML-formatted files and streams. Provides implementations of the standard Serialize/Deserialize traits for TOML data to facilitate deserializing and serializing Rust structures.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-toml-0.4
  (package
    (inherit rust-toml)
    (name "rust-toml")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07qilkzinn8z13vq2sss65n2lza7wrmqpvkbclw919m3f7y691km"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))))))

(define-public rust-trust-dns-https
  (package
    (name "rust-trust-dns-https")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-https" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ypkbgm5p7smjfkca3gaszhvknbr2ykf8skw8pyvpn0sq95lv5ia"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bytes" ,rust-bytes)
         ("rust-data-encoding" ,rust-data-encoding)
         ("rust-failure" ,rust-failure)
         ("rust-futures" ,rust-futures)
         ("rust-h2" ,rust-h2)
         ("rust-http" ,rust-http)
         ("rust-log" ,rust-log)
         ("rust-rustls" ,rust-rustls)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-reactor" ,rust-tokio-reactor)
         ("rust-tokio-rustls" ,rust-tokio-rustls)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-trust-dns-proto" ,rust-trust-dns-proto)
         ("rust-trust-dns-rustls" ,rust-trust-dns-rustls)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-reactor" ,rust-tokio-reactor)
         ("rust-tokio-rustls" ,rust-tokio-rustls)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-trust-dns-proto" ,rust-trust-dns-proto)
         ("rust-trust-dns-rustls" ,rust-trust-dns-rustls)
         ("rust-typed-headers" ,rust-typed-headers)
         ("rust-webpki" ,rust-webpki)
         ("rust-webpki-roots" ,rust-webpki-roots))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-tokio" ,rust-tokio))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
      "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use DNS over HTTPS.")
    (description
      "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use DNS over HTTPS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-native-tls
  (package
    (name "rust-trust-dns-native-tls")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-native-tls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dkwfqxjjmbikm3mav71zjymgy8wmqr4mca64x49qzknvc4qwy6z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-native-tls" ,rust-native-tls)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-tokio-tls" ,rust-tokio-tls)
         ("rust-trust-dns-proto" ,rust-trust-dns-proto))
        #:cargo-development-inputs
        (("rust-tokio" ,rust-tokio))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
      "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use native-tls for TLS.")
    (description
      "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use native-tls for TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-openssl
  (package
    (name "rust-trust-dns-openssl")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-openssl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "19qxi4y33wd2g55r4v9d6b06d20bdhqhvsrsmbpz5ir3i7l5psp7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-openssl" ,rust-openssl)
         ("rust-tokio-openssl" ,rust-tokio-openssl)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-trust-dns-proto" ,rust-trust-dns-proto))
        #:cargo-development-inputs
        (("rust-openssl" ,rust-openssl)
         ("rust-tokio" ,rust-tokio))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
      "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use tokio-openssl for TLS.")
    (description
      "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use tokio-openssl for TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-proto
  (package
    (name "rust-trust-dns-proto")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-proto" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f9xjyz7fsa83dj00zif7lmljd4x420c0vmniinhb7c35777wi85"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-data-encoding" ,rust-data-encoding)
         ("rust-enum-as-inner" ,rust-enum-as-inner)
         ("rust-failure" ,rust-failure)
         ("rust-futures" ,rust-futures)
         ("rust-idna" ,rust-idna)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-log" ,rust-log)
         ("rust-openssl" ,rust-openssl)
         ("rust-rand" ,rust-rand)
         ("rust-ring" ,rust-ring)
         ("rust-serde" ,rust-serde)
         ("rust-smallvec" ,rust-smallvec)
         ("rust-socket2" ,rust-socket2)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-io" ,rust-tokio-io)
         ("rust-tokio-reactor" ,rust-tokio-reactor)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-tokio-timer" ,rust-tokio-timer)
         ("rust-tokio-udp" ,rust-tokio-udp)
         ("rust-url" ,rust-url))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-tokio" ,rust-tokio))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
      "Trust-DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Trust-DNS projects.")
    (description
      "Trust-DNS is a safe and secure DNS library.  This is the foundational DNS protocol library for all Trust-DNS projects.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-resolver
  (package
    (name "rust-trust-dns-resolver")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-resolver" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cjkz3rcisk7v354l5hqb3j5x9x389pjqd6da6h8skvqxr0kl6yb"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-failure" ,rust-failure)
         ("rust-futures" ,rust-futures)
         ("rust-ipconfig" ,rust-ipconfig)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-log" ,rust-log)
         ("rust-lru-cache" ,rust-lru-cache)
         ("rust-resolv-conf" ,rust-resolv-conf)
         ("rust-rustls" ,rust-rustls)
         ("rust-serde" ,rust-serde)
         ("rust-smallvec" ,rust-smallvec)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-tokio-udp" ,rust-tokio-udp)
         ("rust-trust-dns-https" ,rust-trust-dns-https)
         ("rust-trust-dns-native-tls"
          ,rust-trust-dns-native-tls)
         ("rust-trust-dns-openssl"
          ,rust-trust-dns-openssl)
         ("rust-trust-dns-proto" ,rust-trust-dns-proto)
         ("rust-trust-dns-rustls" ,rust-trust-dns-rustls)
         ("rust-webpki-roots" ,rust-webpki-roots))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-tokio" ,rust-tokio)
         ("rust-tokio-io" ,rust-tokio-io))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
      "Trust-DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries.
      ")
         (description
           "Trust-DNS is a safe and secure DNS library.  This Resolver library  uses the Client library to perform all DNS queries.  The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types.  The Client can be used for other queries.
           ")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-rustls
  (package
    (name "rust-trust-dns-rustls")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trust-dns-rustls" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0glpggq31764q7lp19h5l6implsr7ik015qkm5rg7pqwy93krsb3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures" ,rust-futures)
         ("rust-log" ,rust-log)
         ("rust-rustls" ,rust-rustls)
         ("rust-tokio-rustls" ,rust-tokio-rustls)
         ("rust-tokio-tcp" ,rust-tokio-tcp)
         ("rust-trust-dns-proto" ,rust-trust-dns-proto)
         ("rust-webpki" ,rust-webpki))
        #:cargo-development-inputs
        (("rust-openssl" ,rust-openssl)
         ("rust-tokio" ,rust-tokio))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
      "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use rustls for TLS.")
    (description
      "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use rustls for TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trybuild
  (package
    (name "rust-trybuild")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "trybuild" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0df6ipayif05xn61iavdb0dcshm9y6wmcd140pp7dl91mirygs7j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-glob" ,rust-glob)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-termcolor" ,rust-termcolor)
         ("rust-toml" ,rust-toml))))
    (home-page "https://github.com/dtolnay/trybuild")
    (synopsis
      "Test harness for ui tests of compiler diagnostics")
    (description
      "Test harness for ui tests of compiler diagnostics")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-typed-arena
  (package
    (name "rust-typed-arena")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typed-arena" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0im62vrmyzbr8xq66bcyr86ka4z2x8psn9z4982bq4fc8v1zaw3z"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-criterion" ,rust-criterion-0.2))))
    (home-page
      "https://github.com/SimonSapin/rust-typed-arena")
    (synopsis
      "The arena, a fast but limited type of allocator")
    (description
      "The arena, a fast but limited type of allocator")
    (license license:expat)))

(define-public rust-typed-headers
  (package
    (name "rust-typed-headers")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typed-headers" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0g40nlq5iw0zxhwb7nfmfbr9m86abgwwhxwhzrm10nfq6bsmlvxx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-base64" ,rust-base64)
         ("rust-bytes" ,rust-bytes)
         ("rust-chrono" ,rust-chrono)
         ("rust-http" ,rust-http)
         ("rust-mime" ,rust-mime))))
    (home-page "https://github.com/sfackler/typed-headers")
    (synopsis
      "Typed HTTP header serialization and deserialization.")
    (description
      "Typed HTTP header serialization and deserialization.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ucd-parse
  (package
    (name "rust-ucd-parse")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-parse" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d06wcnzs1kqdc0wszlycclhvr92j9v08nkq0w7jyld69nzm4sya"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex))))
    (home-page
      "https://github.com/BurntSushi/ucd-generate")
    (synopsis
      "A library for parsing data files in the Unicode character database.
      ")
         (description
           "This package provides a library for parsing data files in the Unicode character database.
           ")
    (license #f)))

(define-public rust-unicase-1.4
  (package
    (inherit rust-unicase)
    (name "rust-unicase")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicase" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cwazh4qsmm9msckjk86zc1z35xg7hjxjykrgjalzdv367w6aivz"))))
    (arguments
      `(#:cargo-inputs
        (("rust-heapsize" ,rust-heapsize-0.3)
         ("rust-heapsize-plugin" ,rust-heapsize-plugin)
         ("rust-version-check" ,rust-version-check-0.1))
        #:cargo-development-inputs
        (("rust-version-check" ,rust-version-check-0.1))))))

(define-public rust-unicode-bidi
  (package
    (name "rust-unicode-bidi")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-bidi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1malx8ljgm7v1gbaazkn7iicy5wj0bwcyadj3l727a38ch6bvwj9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-matches" ,rust-matches)
        ("rust-flame" ,rust-flame)
        ("rust-flamer" ,rust-flamer)
        ("rust-serde" ,rust-serde))
       #:cargo-development-inputs
       (("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/servo/unicode-bidi")
    (synopsis
      "Implementation of the Unicode Bidirectional Algorithm")
    (description
      "Implementation of the Unicode Bidirectional Algorithm")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-normalization
  (package
    (name "rust-unicode-normalization")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-normalization" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09i49va90rvia1agvgni4gicnqv50y5zy1naw8mr8bcqifh3j4ql"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-smallvec" ,rust-smallvec))))
    (home-page
      "https://github.com/unicode-rs/unicode-normalization")
    (synopsis
      "This crate provides functions for normalization of Unicode strings, including Canonical and Compatible Decomposition and Recomposition, as described in Unicode Standard Annex #15.")
    (description
      "This crate provides functions for normalization of Unicode strings, including Canonical and Compatible Decomposition and Recomposition, as described in Unicode Standard Annex #15.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-segmentation
  (package
    (name "rust-unicode-segmentation")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-segmentation" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1a9jqg7rb1yq6w2xc9jgxcs111yk5vxm9afjfvykfnrmzk6z8rqr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis "Grapheme Cluster, Word and Sentence boundaries for rust")
    (description
     "This crate provides Grapheme Cluster, Word and Sentence boundaries
according to Unicode Standard Annex #29 rules.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-xid-0.0
  (package
    (inherit rust-unicode-xid)
    (name "rust-unicode-xid")
    (version "0.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p5l9h3n3i53cp95fb65p8q3vbwib79ryd9z5z5h5kr9gl6qc7wc"))))))

(define-public rust-url
  (package
    (name "rust-url")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "url" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ij81gib9z2n9r67gin545zr7xvh345pr5bydg2q2sswwr9azpbp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-idna" ,rust-idna)
         ("rust-matches" ,rust-matches)
         ("rust-percent-encoding" ,rust-percent-encoding)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-bencher" ,rust-bencher)
         ("rust-rustc-test" ,rust-rustc-test)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/servo/rust-url")
    (synopsis
      "URL library for Rust, based on the WHATWG URL Standard")
    (description
      "URL library for Rust, based on the WHATWG URL Standard")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-url-1
  (package
    (inherit rust-url)
    (name "rust-url")
    (version "1.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "url" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nim1c90mxpi9wgdw2xh8dqd72vlklwlzam436akcrhjac6pqknx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-idna" ,rust-idna-0.1)
        ("rust-matches" ,rust-matches)
        ("rust-percent-encoding" ,rust-percent-encoding-1)
        ("rust-encoding" ,rust-encoding)
        ("rust-heapsize" ,rust-heapsize)
        ("rust-rustc-serialize" ,rust-rustc-serialize)
        ("rust-serde" ,rust-serde-0.8)
        ("rust-serde-json" ,rust-serde-json-0.8))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher)
        ("rust-rustc-serialize" ,rust-rustc-serialize)
        ("rust-rustc-test" ,rust-rustc-test)
        ("rust-serde-json" ,rust-serde-json-0.8))))))

(define-public rust-urlencoded
  (package
    (name "rust-urlencoded")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "urlencoded" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qrkblcj3gpz256d5fci9g9ig20mxlavy25gj6p612qi740zalha"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bodyparser" ,rust-bodyparser)
         ("rust-iron" ,rust-iron)
         ("rust-plugin" ,rust-plugin)
         ("rust-url" ,rust-url))))
    (home-page "https://github.com/iron/urlencoded")
    (synopsis
      "URL Encoded middleware for the Iron web framework. Decode URL Encoded data from GET request queries and POST request bodies.")
    (description
      "URL Encoded middleware for the Iron web framework. Decode URL Encoded data from GET request queries and POST request bodies.")
    (license license:expat)))

(define-public rust-utf-8
  (package
    (name "rust-utf-8")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "utf-8" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1iw5rp4i3mfi9k51picbr5bgjqhjcmnxx7001clh5ydq31y2zr05"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/SimonSapin/rust-utf8")
    (synopsis
      "Incremental, zero-copy UTF-8 decoding with error handling")
    (description
      "Incremental, zero-copy UTF-8 decoding with error handling")
    (license (list license:expat license:asl2.0))))

(define-public rust-utf8-ranges
  (package
    (name "rust-utf8-ranges")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "utf8-ranges" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ppzjsxmv1p1xfid8wwn07ciikk84k30frl28bwsny6za1vall4x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment)
         ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page
      "https://github.com/BurntSushi/utf8-ranges")
    (synopsis
      "Convert ranges of Unicode codepoints to UTF-8 byte ranges.")
    (description
      "Convert ranges of Unicode codepoints to UTF-8 byte ranges.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-utf8-ranges-0.1
  (package
    (inherit rust-utf8-ranges)
    (name "rust-utf8-ranges")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "utf8-ranges" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "03xf604b2v51ag3jgzw92l97xnb10kw9zv948bhc7ja1ik017jm1"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.2))
       #:tests? #f)))) ; Tests require 'unicode' create.

(define-public rust-uuid
  (package
    (name "rust-uuid")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "uuid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ank4xk20x3nrz926w8j9mz53bi3v8bykxmhlq2pffa8xc8wdnwh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-byteorder" ,rust-byteorder)
         ("rust-md5" ,rust-md5)
         ("rust-rand" ,rust-rand-0.6)
         ("rust-serde" ,rust-serde)
         ("rust-sha1" ,rust-sha1)
         ("rust-slog" ,rust-slog)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-bincode" ,rust-bincode)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-serde-test" ,rust-serde-test))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis
      "A library to generate and parse UUIDs.")
    (description
      "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-vec-map
  (package
    (name "rust-vec-map")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vec_map" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06n8hw4hlbcz328a3gbpvmy0ma46vg1lc0r5wf55900szf3qdiq5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-serde" ,rust-serde))))
    (home-page
      "https://github.com/contain-rs/vec-map")
    (synopsis
      "A simple map based on a vector for small integer keys")
    (description
      "This package provides a simple map based on a vector for small integer keys")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-version-sync
  (package
    (name "rust-version-sync")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version-sync" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01pq0ia7ak7d69c3chjgdmaaq271yrspgbzmk6wmrwb74hx3skw4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-itertools" ,rust-itertools)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.4)
         ("rust-regex" ,rust-regex)
         ("rust-semver-parser" ,rust-semver-parser)
         ("rust-syn" ,rust-syn)
         ("rust-toml" ,rust-toml)
         ("rust-url" ,rust-url-1))))
    (home-page
      "https://github.com/mgeisler/version-sync")
    (synopsis
      "Simple crate for ensuring that version numbers in README files are
      updated when the crate version changes.")
    (description
      "Simple crate for ensuring that version numbers in README files are
      updated when the crate version changes.")
    (license license:expat)))

(define-public rust-walkdir-1
  (package
    (inherit rust-walkdir)
    (name "rust-walkdir")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "walkdir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zw8safzqpsrvfn0256cngq2fr9d4lmwv5qb8ycn1f7sf3kgj25v"))))
    (arguments
      `(#:cargo-inputs
        (("rust-kernel32-sys" ,rust-kernel32-sys)
         ("rust-same-file" ,rust-same-file-0.1)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-docopt" ,rust-docopt-0.7)
         ("rust-quickcheck" ,rust-quickcheck-0.4)
         ("rust-rand" ,rust-rand)
         ("rust-rustc-serialize" ,rust-rustc-serialize))))))

(define-public rust-want
  (package
    (name "rust-want")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "want" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "181b2zmwfq389x9n2g1n37cvcvvdand832zz6v8i1l8wrdlaks0w"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-log" ,rust-bytes)
         ("rust-try-lock" ,rust-try-lock))
        #:cargo-development-inputs
        (("rust-tokio-executor" ,rust-tokio-executor)
         ("rust-tokio-sync" ,rust-tokio-sync))))
    (home-page "https://github.com/seanmonstar/want")
    (synopsis
      "Detect when another Future wants a result.")
    (description
      "Detect when another Future wants a result.")
    (license license:expat)))

(define-public rust-wasm-bindgen
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m8vq3jkhz04fn3wjvb7ii7xql60w32nlvr10jcskcbbh2hpzsad"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-serde" ,rust-serde)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-wasm-bindgen-macro"
          ,rust-wasm-bindgen-macro))))
    (home-page "https://rustwasm.github.io/")
    (synopsis
      "Easy support for interacting between JS and Rust.")
    (description
      "Easy support for interacting between JS and Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-backend
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-backend" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1qxqkbjkjg4pphhcr91nk95c0gizx77dyq24mmijqnwzxxqc30jx"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bumpalo" ,rust-bumpalo)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-log" ,rust-log)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn)
         ("rust-wasm-bindgen-shared"
          ,rust-wasm-bindgen-shared))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Backend code generation of the wasm-bindgen tool")
    (description
      "Backend code generation of the wasm-bindgen tool")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-futures
  (package
    (name "rust-wasm-bindgen-futures")
    (version "0.3.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-futures" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "073p71skp91d9v2wczl6k7z9p0w25vn43br2v2g1ncbc6hvhnhl3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-futures" ,rust-futures)
         ("rust-futures-channel-preview"
          ,rust-futures-channel-preview)
         ("rust-futures-util-preview"
          ,rust-futures-util-preview)
         ("rust-js-sys" ,rust-js-sys)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen)
         ("rust-web-sys" ,rust-web-sys))
        #:cargo-development-inputs
        (("rust-wasm-bindgen-test"
          ,rust-wasm-bindgen-test))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Bridging the gap between Rust Futures and JavaScript Promises")
    (description
      "Bridging the gap between Rust Futures and JavaScript Promises")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-macro
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07fqzzlbncccmnxbbkg9v4n53qc1lps5g0bb9wq3i9zp9gvm0zgh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-quote" ,rust-quote)
         ("rust-wasm-bindgen-macro-support"
          ,rust-wasm-bindgen-macro-support))
        #:cargo-development-inputs
        (("rust-trybuild" ,rust-trybuild)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (description
      "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-macro-support
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-macro-support" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mxi6rj11k67sks88pfqiqylnijxmb1s0gcgpj8mzfj5gvkqzkwm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn)
         ("rust-wasm-bindgen-backend"
          ,rust-wasm-bindgen-backend)
         ("rust-wasm-bindgen-shared"
          ,rust-wasm-bindgen-shared))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate")
    (description
      "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-test
  (package
    (name "rust-wasm-bindgen-test")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-test" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gwslc2sfkghzzb3r0gvd8i5rig2nlqgpl1rn43y2w4mr1ci494k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-console-error-panic-hook"
          ,rust-console-error-panic-hook)
         ("rust-futures" ,rust-futures)
         ("rust-js-sys" ,rust-js-sys)
         ("rust-scoped-tls" ,rust-scoped-tls)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen)
         ("rust-wasm-bindgen-futures"
          ,rust-wasm-bindgen-futures)
         ("rust-wasm-bindgen-test-macro"
          ,rust-wasm-bindgen-test-macro))))
    (home-page
      "https://github.com/rustwasm/wasm-bindgen")
    (synopsis
      "Internal testing crate for wasm-bindgen")
    (description
      "Internal testing crate for wasm-bindgen")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-webidl
  (package
    (name "rust-wasm-bindgen-webidl")
    (version "0.2.50")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-webidl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "057zak44nyrawipgi37m451fjkxz6ix5rzcw11d699rgpy4x4lxy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-failure" ,rust-failure)
         ("rust-heck" ,rust-heck)
         ("rust-log" ,rust-log)
         ("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-syn" ,rust-syn)
         ("rust-wasm-bindgen-backend"
          ,rust-wasm-bindgen-backend)
         ("rust-weedle" ,rust-weedle))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
      "Support for parsing WebIDL specific to wasm-bindgen")
    (description
      "Support for parsing WebIDL specific to wasm-bindgen")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-web-sys
  (package
    (name "rust-web-sys")
    (version "0.3.27")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "web-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lccdl3ihqh70s48dzvzpm09gpwvdjyw4wksk848dm0a41vw8db4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-js-sys" ,rust-js-sys)
         ("rust-wasm-bindgen" ,rust-wasm-bindgen))
        #:cargo-development-inputs
        (("rust-env-logger" ,rust-env-logger)
         ("rust-failure" ,rust-failure)
         ("rust-futures" ,rust-futures)
         ("rust-sourcefile" ,rust-sourcefile)
         ("rust-wasm-bindgen-futures"
          ,rust-wasm-bindgen-futures)
         ("rust-wasm-bindgen-test"
          ,rust-wasm-bindgen-test)
         ("rust-wasm-bindgen-webidl"
          ,rust-wasm-bindgen-webidl))))
    (home-page
      "https://rustwasm.github.io/wasm-bindgen/web-sys/index.html")
    (synopsis
      "Bindings for all Web APIs, a procedurally generated crate from WebIDL")
    (description
      "Bindings for all Web APIs, a procedurally generated crate from WebIDL")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-webpki
  (package
    (name "rust-webpki")
    (version "0.21.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1d41gfai89q7drm92mgmh6fk57nikv2vqsa773i100dcf3kn9rnp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ring" ,rust-ring)
         ("rust-untrusted" ,rust-untrusted))
        #:cargo-development-inputs
        (("rust-base64" ,rust-base64))))
    (home-page "https://github.com/briansmith/webpki")
    (synopsis
      "webpki is a library that validates Web PKI (TLS/SSL) certificates. webpki is designed to provide a full implementation of the client side of the Web PKI to a diverse range of applications and devices, including embedded (IoT) applications, mobile apps, desktop applications, and server infrastructure. webpki is intended to not only be the best implementation of the Web PKI, but to also precisely define what the Web PKI is.")
    (description
      "webpki is a library that validates Web PKI (TLS/SSL) certificates. webpki is designed to provide a full implementation of the client side of the Web PKI to a diverse range of applications and devices, including embedded (IoT) applications, mobile apps, desktop applications, and server infrastructure. webpki is intended to not only be the best implementation of the Web PKI, but to also precisely define what the Web PKI is.")
    (license license:isc))) ; I think

(define-public rust-webpki-roots
  (package
    (name "rust-webpki-roots")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "webpki-roots" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12vi8dh0yik0h4f0b9dnlw5i3gxyky7iblbksh6zcq4xvlvswqm2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-webpki" ,rust-webpki))))
    (home-page "https://github.com/ctz/webpki-roots")
    (synopsis
      "Mozilla's CA root certificates for use with webpki")
    (description
      "Mozilla's CA root certificates for use with webpki")
    (license license:mpl2.0)))

(define-public rust-which
  (package
    (name "rust-which")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r7i793sc0xqnd2fxnqbksj7j1kx65bwn81b8z49750v4c8cnymm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-failure" ,rust-failure)
         ("rust-libc" ,rust-libc))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://github.com/harryfei/which-rs")
    (synopsis
      "A Rust equivalent of Unix command \"which\". Locate installed execuable in cross platforms.")
    (description
      "This package provides a Rust equivalent of Unix command \"which\".  Locate installed execuable in cross platforms.")
    (license license:expat)))

(define-public rust-which-1
  (package
    (inherit rust-which)
    (name "rust-which")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "which" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1cjwa57kzfgzs681a27m5pjmq580pv3hkcg23smf270bgqz60jp8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:tests? #f)))) ; TODO: Fix tests

(define-public rust-winconsole
  (package
    (name "rust-winconsole")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winconsole" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0cjhrzs6djbi7sv37yl27plkqrp7y7bncrh5h3cjvdqds6b4py1y"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cgmath" ,rust-cgmath-0.16)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-rgb" ,rust-rgb)
         ("rust-serde" ,rust-serde)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-serde-cbor" ,rust-serde-cbor)
         ("rust-serde-json" ,rust-serde-json))))
    (home-page
      "https://github.com/omarkmu/winconsole")
    (synopsis
      "A wrapper for console-related functions in the Windows API.")
    (description
      "This package provides a wrapper for console-related functions in the Windows API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-winreg
  (package
    (name "rust-winreg")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winreg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jdcqr6zmvwyrp87h48miasfdvv16gjsb60rc8dy2kqwb3mnv65j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-winapi" ,rust-winapi)
         ("rust-chrono" ,rust-chrono)
         ("rust-serde" ,rust-serde))
        #:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.3)
         ("rust-serde-derive" ,rust-serde-derive))))
    (home-page "https://github.com/gentoo90/winreg-rs")
    (synopsis
      "Rust bindings to MS Windows Registry API.")
    (description
      "Rust bindings to MS Windows Registry API.")
    (license license:expat)))

(define-public rust-x11-dl
  (package
    (name "rust-x11-dl")
    (version "2.18.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "x11-dl" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n1w837xagxqgwx2880d7c9ks6l3g1kk00yd75afdaiv58sf2rdy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-libc" ,rust-libc)
         ("rust-maybe-uninit" ,rust-maybe-uninit)
         ("rust-pkg-config" ,rust-pkg-config))
        #:cargo-development-inputs
        (("rust-pkg-config" ,rust-pkg-config))))
    (inputs
     `(("pkg-config" ,rust-pkg-config)))
    (home-page
      "https://github.com/erlepereira/x11-rs.git")
    (synopsis "X11 library bindings for Rust")
    (description "X11 library bindings for Rust")
    (license license:cc0)))

(define-public rust-xml-rs
  (package
    (name "rust-xml-rs")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xml-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1db4v716rbpgjiasaim2s17rmvsfcq1qzwg6nji6mdf5k34i46sl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/netvl/xml-rs")
    (synopsis "An XML library in pure Rust")
    (description "An XML library in pure Rust")
    (license license:expat)))

(define-public rust-xz2
  (package
    (name "rust-xz2")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xz2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0v4jb0193gx8s1kvd2ajsgh0ffmwhqhfmrrw1n1h2z7w6jgqcyf1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures)
        ("rust-lzma-sys" ,rust-lzma-sys)
        ("rust-tokio-io" ,rust-tokio-io))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-tokio-core" ,rust-tokio-core))))
    (inputs
     `(("pkg-config" ,pkg-config)
       ("xz" ,xz)))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis "Rust bindings to liblzma providing Read/Write streams")
    (description
     "Rust bindings to liblzma providing Read/Write streams as well as low-level
in-memory encoding/decoding.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-yaml-rust
  (package
    (name "rust-yaml-rust")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "yaml-rust" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ka3qhqc5lvk3hz14wmsj32jhmh44blcbfrx5hfxli2gg38kv4k5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-linked-hash-map" ,rust-linked-hash-map))
        #:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck-0.7))))
    (home-page
      "http://chyh1990.github.io/yaml-rust/")
    (synopsis "The missing YAML 1.2 parser for rust")
    (description
      "The missing YAML 1.2 parser for rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-yaml-rust-0.3
  (package
    (inherit rust-yaml-rust)
    (name "rust-yaml-rust")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "yaml-rust" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "14m9dzwb8fb05f4jjb4nqp49rxd9c5vcmwpv3a04d2y5iphncqz6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-clippy" ,rust-clippy)
        ("rust-linked-hash-map" ,rust-linked-hash-map-0.3))))))
