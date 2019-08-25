(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io))

;; Please keep these packages sorted alphabetically

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
        ("rust-object" ,rust-object)
        ("rust-rustc-demangle" ,rust-rustc-demangle)
        ("rust-smallvec" ,rust-smallvec))
       #:cargo-development-inputs
       (("rust-backtrace" ,rust-backtrace)
        ("rust-clap" ,rust-clap)
        ("rust-findshlibs" ,rust-findshlibs)
        ("rust-memmap" ,rust-memmap)
        ("rust-rustc-test" ,rust-rustc-test))))
    (home-page "https://github.com/gimli-rs/addr2line")
    (synopsis
      "A cross-platform symbolication library written in Rust, using `gimli`")
    (description
      "This package provides a cross-platform symbolication library written in Rust, using `gimli`")
    (license (list license:asl2.0
                   license:expat))))

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
    (synopsis
      "Fuzzing Rust code with american-fuzzy-lop")
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

(define-public rust-ansi-term
  (package
    (name "rust-ansi-term")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0dmvziqx1j06xbv3zx62k7w81dyaqviag1rk5a0iynjqqdk2g9za"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment)
        ("rust-serde-json" ,rust-serde-json))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis
      "Library for ANSI terminal colours and styles (bold, underline)")
    (description
      "Library for ANSI terminal colours and styles (bold, underline)")
    (license license:expat)))

(define-public rust-ansi-term-0.11
  (package
    (inherit rust-ansi-term)
    (name "rust-ansi-term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (arguments
      `(#:cargo-inputs
        (("rust-winapi" ,rust-winapi))))))

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
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mn5az4hkxgjhwy157pr1nrfdb3qjpw8jw8v91m2i8wg59b21qwi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-quickcheck" ,rust-quickcheck)
         ("rust-serde" ,rust-serde)
         ("rust-serde-test" ,rust-serde-test))))
    (home-page
      "https://github.com/tomprogrammer/rust-ascii")
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
        (("rust-fst" ,rust-fst)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-quickcheck" ,rust-quickcheck)
         ("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/tapeinosyne/atlatl")
    (synopsis "Double-array tries.")
    (description "Double-array tries.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-atty
  (package
    (name "rust-atty")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "140sswp1bwqwc4zk80bxkbnfb3g936hgrb77g9g0k1zcld3wc0qq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
      "This package provides a simple interface for querying atty")
    (license license:expat)))

(define-public rust-automod
  (package
    (name "rust-automod")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "automod" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0pld582piq2d55z0j96zcs8izw3ml46f8h9y7sdyxg093yfvxl2h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn))))
    (home-page "https://github.com/dtolnay/automod")
    (synopsis "Pull in every source file in a directory as a module.")
    (description
      "Pull in every source file in a directory as a module.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-average
  (package
    (name "rust-average")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "average" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "077wbjzn2hwdjnglp8pjvirvsjgfgbgnlirwh5g2hk14xqx7f57l"))))
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
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mfwbb6832rh1za304w8x37bvs9fjbybpmmz0iksqfzsaf108w8k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-addr2line" ,rust-addr2line)
         ("rust-backtrace-sys" ,rust-backtrace-sys)
         ("rust-cfg-if" ,rust-cfg-if)
         ("rust-compiler-builtins"
          ,rust-compiler-builtins)
         ("rust-cpp-demangle" ,rust-cpp-demangle)
         ("rust-findshlibs" ,rust-findshlibs)
         ("rust-goblin" ,rust-goblin)
         ("rust-goblin" ,rust-goblin)
         ("rust-goblin" ,rust-goblin)
         ("rust-goblin" ,rust-goblin)
         ("rust-libc" ,rust-libc)
         ("rust-memmap" ,rust-memmap)
         ("rust-rustc-demangle" ,rust-rustc-demangle)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core)
         ("rust-serde" ,rust-serde)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/rust-lang/backtrace-rs")
    (synopsis
      "A library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (description
      "This package provides a library to acquire a stack trace (backtrace) at runtime in a Rust program.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-backtrace-sys
  (package
    (name "rust-backtrace-sys")
    (version "0.1.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0as2pk77br4br04daywhivpi1ixxb8y2c7f726kj849dxys31a42"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-compiler-builtins"
          ,rust-compiler-builtins)
         ("rust-libc" ,rust-libc)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core))
        #:cargo-development-inputs
        (("rust-cc" ,rust-cc))))
    (home-page
      "https://github.com/rust-lang/backtrace-rs")
    (synopsis
      "Bindings to the libbacktrace gcc library")
    (description
      "Bindings to the libbacktrace gcc library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-base-x
  (package
    (name "rust-base-x")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base-x" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0hv4y5cdhv6bk0ghk2434clw8v4mmk5cc9lsh6qrpri92zlfmx3n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-bencher" ,rust-bencher)
         ("rust-json" ,rust-json)
         ("rust-rand" ,rust-rand))))
    (home-page "https://github.com/OrKoN/base-x-rs")
    (synopsis "Encode/decode any base")
    (description "Encode/decode any base")
    (license license:expat)))

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
        (("rust-criterion" ,rust-criterion)
         ("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/marshallpierce/rust-base64")
    (synopsis
      "encodes and decodes base64 as bytes or utf8")
    (description
      "encodes and decodes base64 as bytes or utf8")
    (license (list license:asl2.0
                   license:expat))))

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
    (synopsis
      "A binary serialization / deserialization strategy that uses Serde for transforming structs into bytes and vice versa!")
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
     `(("libclang" ,(@ (gnu packages llvm) clang))))
    (home-page
      "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
      "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

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
        (("rust-criterion" ,rust-criterion)
         ("rust-quickcheck" ,rust-quickcheck))))
    (home-page "https://github.com/fitzgen/bumpalo")
    (synopsis
      "Fast bump allocation arena for Rust")
    (description
      "This package provides a fast bump allocation arena for Rust.")
    (license (list license:asl2.0
                   license:expat))))

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
         ("rust-quickcheck" ,rust-quickcheck)
         ("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/BurntSushi/byteorder")
    (synopsis
      "Library for reading/writing numbers in big-endian and little-endian.")
    (description
      "Library for reading/writing numbers in big-endian and little-endian.")
    (license (list license:unlicense
                   license:expat))))

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

(define-public rust-cc
  (package
    (name "rust-cc")
    (version "1.0.38")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0imzcz53wg7m3gr6657yrikp7icyc2bpkvssnyd0xvj8imihqh6f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rayon" ,rust-rayon))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:tests? #f)) ; tests fail
    (home-page
      "https://github.com/alexcrichton/cc-rs")
    (synopsis
      "A build-time dependency for Cargo build scripts to assist in invoking the native
      C compiler to compile native C code into a static archive to be linked into Rust
      code.")
    (description
      "This package provides a build-time dependency for Cargo build scripts to assist in invoking the native
      C compiler to compile native C code into a static archive to be linked into Rustcode.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cexpr
  (package
    (name "rust-cexpr")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cexpr" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1by64ini3f058pwad3immx5cc12wr0m0kwgaxa8apzym03mj9ym7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-nom" ,rust-nom))
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
     `(("libclang" ,(@ (gnu packages llvm) clang))))
    (home-page
      "https://github.com/jethrogb/rust-cexpr")
    (synopsis "A C expression parser and evaluator")
    (description
      "This package provides a C expression parser and evaluator")
    (license (list license:asl2.0
                   license:expat))))

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
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01n3gxmwp765m6xg1fl8v1y12wsvbqvlcai27kdr5d2skrijyfb7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-envmnt" ,rust-envmnt)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))))
    (home-page
      "https://github.com/sagiegurari/ci_info")
    (synopsis
      "Provides current CI environment information.")
    (description
      "Provides current CI environment information.")
    (license license:asl2.0)))

(define-public rust-clang-sys
  (package
    (name "rust-clang-sys")
    (version "0.28.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m8h56yjwv19pbah4lrhmb8js9mhx6hi5gk0y4zzix89xjf2c9s2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-glob" ,rust-glob-0.2)
         ("rust-libc" ,rust-libc)
         ("rust-libloading" ,rust-libloading))
        #:cargo-development-inputs
        (("rust-glob" ,rust-glob))
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'set-environmental-variable
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((clang (assoc-ref inputs "libclang")))
                (setenv "LIBCLANG_PATH"
                        (string-append clang "/lib")))
              #t)))))
    (inputs
     `(("libclang" ,(@ (gnu packages llvm) clang))))
    (home-page
      "https://github.com/KyleMayes/clang-sys")
    (synopsis "Rust bindings for libclang.")
    (description "Rust bindings for libclang.")
    (license license:asl2.0)))

(define-public rust-clap
  (package
    (name "rust-clap")
    (version "2.33.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nf6ld3bims1n5vfzhkvcb55pdzh04bbhzf8nil5vvw05nxzarsh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ansi-term" ,rust-ansi-term-0.11)
         ("rust-atty" ,rust-atty)
         ("rust-bitflags" ,rust-bitflags)
         ("rust-clippy" ,rust-clippy)
         ("rust-strsim" ,rust-strsim-0.8)
         ("rust-term-size" ,rust-term-size)
         ("rust-textwrap" ,rust-textwrap)
         ("rust-unicode-width" ,rust-unicode-width)
         ("rust-vec-map" ,rust-vec-map)
         ("rust-yaml-rust" ,rust-yaml-rust))
        #:cargo-development-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-regex" ,rust-regex)
         ("rust-version-sync" ,rust-version-sync))))
    (home-page "https://clap.rs/")
    (synopsis
      "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
      "This package provides a simple to use, efficient, and full-featured Command Line Argument Parser")
    (license license:expat)))

(define-public rust-clicolors-control
  (package
    (name "rust-clicolors-control")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clicolors-control" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y80cgarxhrd1bz5yjm81r444v6flvy36aaxrrsac0yhfd6gvavk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-atty" ,rust-atty)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/mitsuhiko/clicolors-control")
    (synopsis
      "A common utility library to control CLI colorization")
    (description
      "This package provides a common utility library to control CLI colorization")
    (license license:expat)))

(define-public rust-clippy
  (package
    (name "rust-clippy")
    (version "0.0.302")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clippy" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1562x3sq9mgmc8j39gd34wqm7ybrdvpmj7cc1n450gwsawayw4fr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-term" ,rust-term))))
    (home-page
      "https://github.com/rust-lang/rust-clippy")
    (synopsis
      "A bunch of helpful lints to avoid common pitfalls in Rust.")
    (description
      "This package provides a bunch of helpful lints to avoid common pitfalls in Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cloudabi
  (package
    (name "rust-cloudabi")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cloudabi" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0kxcg83jlihy0phnd2g8c2c303px3l2p3pkjz357ll6llnd5pz6x"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bitflags" ,rust-bitflags))))
    (home-page "https://nuxi.nl/cloudabi/")
    (synopsis
      "Low level interface to CloudABI. Contains all syscalls and related types.")
    (description
      "Low level interface to CloudABI.  Contains all syscalls and related types.")
    (license license:bsd-2)))

(define-public rust-cmake
  (package
    (name "rust-cmake")
    (version "0.1.40")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cmake" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1w0zgqdbbhl9w6px7avc6d5p43clglrmjfdn2n26mdsli5n3i91c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-cc" ,rust-cc))))
    (home-page
      "https://github.com/alexcrichton/cmake-rs")
    (synopsis
      "A build dependency for running `cmake` to build a native library")
    (description
      "This package provides a build dependency for running `cmake` to build a native library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-compiler-builtins
  (package
    (name "rust-compiler-builtins")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compiler_builtins" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fpabpmg8paj4r5a37vmidh1jx1b7a6ilxm4s3xsxczx27ybjcjf"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core))
        #:cargo-development-inputs
        (("rust-cc" ,rust-cc))))
    (home-page
      "https://github.com/rust-lang-nursery/compiler-builtins")
    (synopsis
      "Compiler intrinsics used by the Rust compiler. Also available for other targets if necessary!")
    (description
      "Compiler intrinsics used by the Rust compiler.  Also available for other targets if necessary!")
    (license (list license:asl2.0
                   license:expat))))

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
        (("rust-quickcheck" ,rust-quickcheck)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/DanielKeep/rust-conv")
    (synopsis
      "This crate provides a number of conversion traits with more specific semantics than those provided by 'as' or 'From'/'Into'.")
    (description
      "This crate provides a number of conversion traits with more specific semantics than those provided by 'as' or 'From'/'Into'.")
    (license license:expat)))

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

(define-public rust-cpp-demangle
  (package
    (name "rust-cpp-demangle")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpp_demangle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0a4hqsfc0sfdwy7pcr0rc1fjp2j47fxbkqfc2lfrbi4zlm5hq36k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-afl" ,rust-afl)
         ("rust-cfg-if" ,rust-cfg-if))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap)
         ("rust-diff" ,rust-diff)
         ("rust-glob" ,rust-glob))))
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
         ("rust-git2" ,rust-git2)
         ("rust-glob" ,rust-glob)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://github.com/frewsxcv/rust-crates-index")
    (synopsis
      "Library for retrieving and interacting with the crates.io index")
    (description
      "Library for retrieving and interacting with the crates.io index")
    (license license:asl2.0)))

(define-public rust-criterion
  (package
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
         ("rust-libc" ,rust-libc)
         ("rust-num-traits" ,rust-num-traits)
         ("rust-rand-core" ,rust-rand-core)
         ("rust-rand-os" ,rust-rand-os)
         ("rust-rand-xoshiro" ,rust-rand-xoshiro)
         ("rust-rayon" ,rust-rayon)
         ("rust-rayon-core" ,rust-rayon-core)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive)
         ("rust-serde-json" ,rust-serde-json)
         ("rust-tinytemplate" ,rust-tinytemplate)
         ("rust-walkdir" ,rust-walkdir))
        #:cargo-development-inputs
        (("rust-approx" ,rust-approx)
         ("rust-quickcheck" ,rust-quickcheck)
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

(define-public rust-custom-derive
  (package
    (name "rust-custom-derive")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "custom_derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f81bavw1wnykwh21hh4yyzigs6zl6f6pkk9p3car8kq95yfb2pg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-rustc-serialize" ,rust-rustc-serialize))))
    (home-page
      "https://github.com/DanielKeep/rust-custom-derive/tree/custom_derive-master")
    (synopsis
      "(Note: superseded by `macro-attr`) This crate provides a macro that enables the use of custom derive attributes.")
    (description
      "(Note: superseded by `macro-attr`) This crate provides a macro that enables the use of custom derive attributes.")
    (license (list license:asl2.0
                   license:expat))))

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
        (("rust-quickcheck" ,rust-quickcheck)
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
        (("rust-quickcheck" ,rust-quickcheck)
         ("rust-term" ,rust-term))))
    (home-page
      "https://github.com/johannhof/difference.rs")
    (synopsis
      "A Rust text diffing and assertion library.")
    (description
      "This package provides a Rust text diffing and assertion library.")
    (license license:expat)))

(define-public rust-dirs
  (package
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
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-dirs-sys" ,rust-dirs-sys))))
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis
      "A tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (description
      "This package provides a tiny low-level library that provides platform-specific standard locations of directories for config, cache and other data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by the XDG base/user directory specifications on Linux, the Known Folder API on Windows, and the Standard Directory guidelines on macOS.")
    (license #f)))

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
        (("rust-lazy-static" ,rust-lazy-static))))
    (home-page
      "https://github.com/tormol/encode_unicode")
    (synopsis
      "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.")
    (description
      "UTF-8 and UTF-16 character types, iterators and related methods for char, u8 and u16.")
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
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ndpw1ny2kxqpw6k1shq8k56z4vfpk4xz9zr8ay988k0rffrxd1s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-backtrace" ,rust-backtrace))
        #:cargo-development-inputs
        (("rust-version-check" ,rust-version-check))))
    (home-page
      "https://github.com/rust-lang-nursery/error-chain")
    (synopsis
      "Yet another error boilerplate library.")
    (description
      "Yet another error boilerplate library.")
    (license (list license:asl2.0
                   license:expat))))

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
         ("rust-synstructure" ,rust-synstructure))
        #:cargo-development-inputs
        (("rust-failure" ,rust-failure))))
    (home-page
      "https://rust-lang-nursery.github.io/failure/")
    (synopsis "derives for the failure crate")
    (description "derives for the failure crate")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-findshlibs
  (package
    (name "rust-findshlibs")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "findshlibs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n2vagn0q5yim32hxkwi1cjgp3yn1dm45p7z8nw6lapywihhs9mi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-libc" ,rust-libc))))
    (home-page
      "https://github.com/gimli-rs/findshlibs")
    (synopsis
      "Find the set of shared libraries loaded in the current process with a cross platform API")
    (description
      "Find the set of shared libraries loaded in the current process with a cross platform API")
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
        (("rust-byteorder" ,rust-byteorder)
         ("rust-memmap" ,rust-memmap))
        #:cargo-development-inputs
        (("rust-fnv" ,rust-fnv)
         ("rust-fst-levenshtein" ,rust-fst-levenshtein)
         ("rust-fst-regex" ,rust-fst-regex)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-quickcheck" ,rust-quickcheck)
         ("rust-rand" ,rust-rand))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Use finite state transducers to compactly represents sets or maps of many strings (> 1 billion is possible).")
    (description
      "Use finite state transducers to compactly represents sets or maps of many strings (> 1 billion is possible). ")
    (license (list license:unlicense
                   license:expat))))

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
         ("rust-regex-syntax" ,rust-regex-syntax)
         ("rust-utf8-ranges" ,rust-utf8-ranges))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Search finite state transducers with regular expression.")
    (description
      "Search finite state transducers with regular expression.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-fuchsia-cprng ; guix upstreamable
  (package
    (name "rust-fuchsia-cprng")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-cprng" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1fnkqrbz7ixxzsb04bsz9p0zzazanma8znfdqjvh39n14vapfvx0"))))
    (build-system cargo-build-system)
    (home-page "https://fuchsia.googlesource.com/fuchsia/+/master/garnet/public/rust/fuchsia-cprng")
    (synopsis "Fuchsia cryptographically secure pseudorandom number generator")
    (description "Rust crate for the Fuchsia cryptographically secure
pseudorandom number generator")
    (license license:bsd-3)))

(define-public rust-futures-channel-preview
  (package
    (name "rust-futures-channel-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-channel-preview" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
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
    (home-page
      "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis
      "Channels for asynchronous communication using futures-rs.")
    (description
      "Channels for asynchronous communication using futures-rs.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-core-preview ; guix upstreamable
  (package
    (name "rust-futures-core-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-core-preview" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xaq8m609k6cz8xydwhwp8xxyxigabcw1w9ngycfy0bnkg7iq52b"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang-nursery.github.io/futures-rs")
    (synopsis "Core traits and types in for the @code{futures} library.")
    (description "This crate provides the core traits and types in for the
@code{futures} library.")
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

(define-public rust-futures-io-preview ; guix upstreamable
  (package
    (name "rust-futures-io-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-io-preview" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0fhvwhdb8ywjjbfng0ra1r8yyc9yzpyxg9sv3spb3f7w0lk40bh8"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis "Async read and write traits for the futures library")
    (description "This crate provides the @code{AsyncRead} and
@code{AsyncWrite} traits for the @code{futures-rs} library.")
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

(define-public rust-futures-sink-preview
  (package
    (name "rust-futures-sink-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-sink-preview" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1r4d0gy73hdxkh5g1lrhl1kjnwp6mywjgcj70v0z78b921da42a3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-futures-core-preview"
          ,rust-futures-core-preview))))
    (home-page
      "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis
      "The asynchronous `Sink` trait for the futures-rs library.")
    (description
      "The asynchronous `Sink` trait for the futures-rs library.")
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

(define-public rust-getopts
  (package
    (name "rust-getopts")
    (version "0.2.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getopts" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l74ldicw6gpkly3jdiq8vq8g597x7akvych2cgy7gr8q8apnckj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-unicode-width" ,rust-unicode-width))
        #:cargo-development-inputs
        (("rust-log" ,rust-log))))
    (home-page
      "https://github.com/rust-lang-nursery/getopts")
    (synopsis "getopts-like option parsing.")
    (description "getopts-like option parsing.")
    (license (list license:asl2.0
                   license:expat))))

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
     `(("libgit" ,(@ (gnu packages version-control) libgit2))
       ("openssl" ,(@ (gnu packages tls) openssl))))
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

(define-public rust-glob
  (package
    (name "rust-glob")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0x25wfr7vg3mzxc9x05dcphvd3nwlcmbnxrvwcvrrdwplcrrk4cv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))
    (home-page "https://github.com/rust-lang-nursery/glob")
    (synopsis
      "Support for matching file paths against Unix shell style patterns.")
    (description
      "Support for matching file paths against Unix shell style patterns.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-glob-0.2
  (package
    (inherit rust-glob)
    (name "rust-glob")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ysvi72slkw784fcsymgj4308c3y03gwjjzqxp80xdjnkbh8vqcb"))))
    (arguments
      `(#:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))))

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

(define-public rust-heapsize
  (package
    (name "rust-heapsize")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heapsize" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0q94q9ppqjgrw71swiyia4hgby2cz6dldp7ij57nkvhd6zmfcy8n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/servo/heapsize")
    (synopsis
      "Infrastructure for measuring the total runtime size of an object on the heap")
    (description
      "Infrastructure for measuring the total runtime size of an object on the heap")
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
         ("rust-quickcheck" ,rust-quickcheck)
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
         ("rust-itertools" ,rust-itertools)
         ("rust-lazy-static" ,rust-lazy-static)
         ("rust-quickcheck" ,rust-quickcheck)
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

(define-public rust-iovec
  (package
    (name "rust-iovec")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iovec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "025vi072m22299z3fg73qid188z2iip7k41ba6v5v5yhwwby9rnv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/carllerche/iovec")
    (synopsis
      "Portable buffer type for scatter/gather I/O operations")
    (description
      "Portable buffer type for scatter/gather I/O operations")
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
         ("rust-quickcheck" ,rust-quickcheck)
         ("rust-rand" ,rust-rand))))
    (home-page
      "https://github.com/bluss/rust-itertools")
    (synopsis
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:asl2.0
                   license:expat))))

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
     `(("jemalloc" ,(@ (gnu packages jemalloc) jemalloc))))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis
      "A safe wrapper over jemalloc's control and introspection APIs")
    (description
      "This package provides a safe wrapper over jemalloc's control and introspection APIs")
    (license #f)))

(define-public rust-jemalloc-sys
  (package
    (name "rust-jemalloc-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jemalloc-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ify9vlql01qhfxlj7d4p9jvcp90mj2h69nkbq7slccvbhzryfqd"))
        ;(modules '((guix build utils)))
        ;(snippet
        ; '(begin
        ;    ;; unbundle jemalloc source
        ;    (delete-file-recursively "jemalloc") #t))
        ))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-cc" ,rust-cc)
        ("rust-fs-extra" ,rust-fs-extra))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-jemalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jemalloc (assoc-ref inputs "jemalloc")))
               (delete-file-recursively "jemalloc")
               (setenv "JEMALLOC_OVERRIDE"
                       (string-append jemalloc "/lib/libjemalloc_pic.a")))
             #t)))))
    (inputs
     `(("jemalloc" ,(@ (gnu packages jemalloc) jemalloc))))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis "Rust FFI bindings to jemalloc")
    (description "Rust FFI bindings to jemalloc")
    (license (list license:asl2.0
                   license:expat))))

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
     `(("jemalloc" ,(@ (gnu packages jemalloc) jemalloc))))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis "A Rust allocator backed by jemalloc")
    (description
      "This package provides a Rust allocator backed by jemalloc")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-lazy-static
  (package
    (name "rust-lazy-static")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy_static" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "052ac27w189hrf1j3hz7sga46rp84zl2hqnzyihxv78mgzr2jmxw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-spin" ,rust-spin))))
    (home-page
      "https://github.com/rust-lang-nursery/lazy-static.rs")
    (synopsis
      "A macro for declaring lazily evaluated statics in Rust.")
    (description
      "This package provides a macro for declaring lazily evaluated statics in Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-lazycell
  (package
    (name "rust-lazycell")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazycell" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gvqycmpv7parc98i6y64ai7rvxrn1947z2a6maa02g4kvxdd55j"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-clippy" ,rust-clippy))))
    (home-page "https://github.com/indiv0/lazycell")
    (synopsis
      "A library providing a lazily filled Cell struct")
    (description
      "This package provides a library providing a lazily filled Cell struct")
    (license (list license:asl2.0
                   license:expat))))

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
         ("rust-quickcheck" ,rust-quickcheck)
         ("rust-rustc-version" ,rust-rustc-version))))
    (home-page
      "https://github.com/Alexhuszagh/rust-lexical/tree/master/lexical-core")
    (synopsis
      "Lexical, to- and from-string conversion routines.")
    (description
      "Lexical, to- and from-string conversion routines.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libgit2-sys
  (package
    (name "rust-libgit2-sys")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0y2mibmx7wy91s2kmb2gfb29mrqlqaxpy5wcwr8s1lwws7b9w5sc"))
        ;(modules '((guix build utils)))
        ;(snippet
        ; '(begin
        ;    ;; unbundle jemalloc source
        ;    (delete-file-recursively "libgit2") #t))
        ))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-libssh2-sys" ,rust-libssh2-sys)
         ("rust-libz-sys" ,rust-libz-sys)
         ("rust-openssl-sys" ,rust-openssl-sys))
        #:cargo-development-inputs
        (("rust-cc" ,rust-cc)
         ("rust-pkg-config" ,rust-pkg-config))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             (delete-file-recursively "libgit2")
             (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
             (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
             #t)))))
    (inputs
     `(("libgit" ,(@ (gnu packages version-control) libgit2))
       ("openssl" ,(@ (gnu packages tls) openssl))
       ("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))
       ("zlib" ,(@ (gnu packages compression) zlib))))
    (home-page
      "https://github.com/rust-lang/git2-rs")
    (synopsis
      "Native bindings to the libgit2 library")
    (description
      "Native bindings to the libgit2 library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libloading
  (package
    (name "rust-libloading")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libloading" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0lyply8rcqc8agajzxs7bq6ivba9dnn1i68kgb9z2flnfjh13cgj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cc" ,rust-cc)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/nagisa/rust_libloading/")
    (synopsis
      "A safer binding to platformâ\x80\x99s dynamic library loading utilities")
    (description
      "This package provides a safer binding to platformâ\x80\x99s dynamic library loading utilities")
    (license license:isc)))

(define-public rust-libssh2-sys
  (package
    (name "rust-libssh2-sys")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libssh2-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17dz3xxy5bc73sr52maa6wdqmw1a0ymznrgfzlxid2rng101yshj"))
        ;(modules '((guix build utils)))
        ;(snippet
        ; '(begin
        ;    ;; unbundle jemalloc source
        ;    (delete-file-recursively "libssh2") #t))
        ))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-libz-sys" ,rust-libz-sys)
         ("rust-openssl-sys" ,rust-openssl-sys)
         ("rust-vcpkg" ,rust-vcpkg))
        #:cargo-development-inputs
        (("rust-cc" ,rust-cc)
         ("rust-pkg-config" ,rust-pkg-config))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             (delete-file-recursively "libssh2")
             (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
             #t)))))
    (inputs
     `(("libssh2" ,(@ (gnu packages ssh) libssh2))
       ("openssl" ,(@ (gnu packages tls) openssl))
       ("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))
       ("zlib" ,(@ (gnu packages compression) zlib))))
    (home-page
      "https://github.com/alexcrichton/ssh2-rs")
    (synopsis
      "Native bindings to the libssh2 library")
    (description
      "Native bindings to the libssh2 library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libz-sys
  (package
    (name "rust-libz-sys")
    (version "1.0.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libz-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1gjycyl2283525abks98bhxa4r259m617xfm5z52p3p3c8ry9d9f"))
        ;(modules '((guix build utils)))
        ;(snippet
        ; '(begin
        ;    ;; unbundle jemalloc source
        ;    (delete-file-recursively "src/zlib") #t))
        ))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc)
        ("rust-pkg-config" ,rust-pkg-config)
        ("rust-vcpkg" ,rust-vcpkg))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-vendored-zlib
           (lambda _
             (delete-file-recursively "src/zlib")
             #t)))))
    (inputs
     `(("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))
       ("zlib" ,(@ (gnu packages compression) zlib))))
    (home-page "https://github.com/alexcrichton/libz-sys")
    (synopsis "Bindings to the system libz library (also known as zlib).")
    (description
      "Bindings to the system libz library (also known as zlib).")
    (license (list license:asl2.0
                   license:expat))))

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

(define-public rust-lock-api
  (package
    (name "rust-lock-api")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1p04271jikw69ja0ap0plrfwm9incf1iny48g0b3ma9k4mw2x4gq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-owning-ref" ,rust-owning-ref)
         ("rust-scopeguard" ,rust-scopeguard)
         ("rust-serde" ,rust-serde))))
    (home-page
      "https://github.com/Amanieu/parking_lot")
    (synopsis
      "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
      "Wrappers to create fully-featured Mutex and RwLock types.  Compatible with no_std.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-log
  (package
    (name "rust-log")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
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
    (home-page "https://github.com/rust-lang/log")
    (synopsis
      "A lightweight logging facade for Rust")
    (description
      "This package provides a lightweight logging facade for Rust")
    (license (list license:asl2.0
                   license:expat))))

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
        (("rust-quickcheck" ,rust-quickcheck))))
    (home-page
      "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-memmap
  (package
    (name "rust-memmap")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memmap" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0ns7kkd1h4pijdkwfvw4qlbbmqmlmzwlq3g2676dcl5vwyazv1b5"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-tempdir" ,rust-tempdir))))
    (home-page
      "https://github.com/danburkert/memmap-rs")
    (synopsis
      "Cross-platform Rust API for memory-mapped file IO")
    (description
      "Cross-platform Rust API for memory-mapped file IO")
    (license (list license:asl2.0
                   license:expat))))

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
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1cvm2z7dy138s302ii7wlzcxbka5a8yfl5pl5di7lbdnw9hw578g"))))))

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

(define-public rust-nodrop ; guix upstreamable
  (package
    (name "rust-nodrop")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nodrop" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0if9ifn6rvar5jirx4b3qh4sl5kjkmcifycvzhxa9j3crkfng5ig"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nodrop-union" ,rust-nodrop-union))))
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis "Wrapper type to inhibit drop (destructor)")
    (description "This package provides a wrapper type to inhibit drop
(destructor).  Use @code{std::mem::ManuallyDrop} instead!")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-nodrop-union ; guix upstreamable
  (package
    (name "rust-nodrop-union")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nodrop-union" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jsnkdn9l8jlmb9h4wssi76sxnyxwnyi00p6y1p2gdq7c1gdw2b7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis "Wrapper type to inhibit drop (destructor)")
    (description "This package provides a wrapper type to inhibit drop
(destructor).  Implementation crate for nodrop, the untagged unions
implementation (which is unstable / requires nightly).")
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
        (("rust-criterion" ,rust-criterion)
         ("rust-doc-comment" ,rust-doc-comment)
         ("rust-jemallocator" ,rust-jemallocator)
         ("rust-version-check" ,rust-version-check))))
    (home-page "https://github.com/Geal/nom")
    (synopsis
      "A byte-oriented, zero-copy, parser combinators library")
    (description
      "This package provides a byte-oriented, zero-copy, parser combinators library")
    (license license:expat)))

(define-public rust-num-cpus
  (package
    (name "rust-num-cpus")
    (version "1.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_cpus" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0wrj3zvj6h3q26sqj9zxpd59frjb54n7jhjwf307clq31ic47vxw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc))
        #:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment))))
    (home-page
      "https://github.com/seanmonstar/num_cpus")
    (synopsis "Get the number of CPUs on a machine.")
    (description
      "Get the number of CPUs on a machine.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-iter
  (package
    (name "rust-num-iter")
    (version "0.1.39")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-iter" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bhk2qbr3261r6zvfc58lz4spfqjhvdripxgz5mks5rd85r55gbn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-num-integer" ,rust-num-integer)
         ("rust-num-traits" ,rust-num-traits))
        #:cargo-development-inputs
        (("rust-autocfg" ,rust-autocfg))))
    (home-page
      "https://github.com/rust-num/num-iter")
    (synopsis
      "External iterators for generic mathematics")
    (description
      "External iterators for generic mathematics")
    (license (list license:asl2.0
                   license:expat))))

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
         ("rust-foreign-types" ,rust-foreign-types)
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
     `(("openssl" ,(@ (gnu packages tls) openssl))))
    (home-page
      "https://github.com/sfackler/rust-openssl")
    (synopsis "OpenSSL bindings")
    (description "OpenSSL bindings")
    (license license:asl2.0)))

(define-public rust-openssl-src
  (package
    (name "rust-openssl-src")
    (version "111.4.0+1.1.1c")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-src" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "10s3hvkfk6bi4ba1ssvj914rjs31vs8plssy4kbvsa0w7idkqfkq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-cc" ,rust-cc))))
    (home-page
      "https://github.com/alexcrichton/openssl-src-rs")
    (synopsis
      "Source of OpenSSL and logic to build it.")
    (description
      "Source of OpenSSL and logic to build it.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openssl-sys
  (package
    (name "rust-openssl-sys")
    (version "0.9.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1f05kxx8mai9ac16x1lk0404bymghbbj7vcbqrfwqfr52w131fmm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg)
        ("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-autocfg" ,rust-autocfg)
        ("rust-cc" ,rust-cc)
        ("rust-openssl-src" ,rust-openssl-src)
        ("rust-pkg-config" ,rust-pkg-config)
        ("rust-vcpkg" ,rust-vcpkg))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             #t)))))
    (inputs
     `(("openssl" ,(@ (gnu packages tls) openssl))
       ("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))))
    (home-page
      "https://github.com/sfackler/rust-openssl")
    (synopsis "FFI bindings to OpenSSL")
    (description "FFI bindings to OpenSSL")
    (license license:expat)))

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
        (("rust-lock-api" ,rust-lock-api)
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
    (home-page "https://pest-parser.github.io/")
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
    (home-page "https://pest-parser.github.io/")
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
    (home-page "https://pest-parser.github.io/")
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
    (home-page "https://pest-parser.github.io/")
    (synopsis "pest meta language parser and validator")
    (description
      "pest meta language parser and validator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pkg-config
  (package
    (name "rust-pkg-config")
    (version "0.3.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkg-config" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "135ia995lqzr0gxpk85h0bjxf82kj6hbxdx924sh9jdln6r8wvk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static))))
    (inputs
     `(("pkg-config" ,(@ (gnu packages pkg-config) pkg-config))))
    (home-page
      "https://github.com/alexcrichton/pkg-config-rs")
    (synopsis
      "A library to run the pkg-config system tool at build time in order to be used in Cargo build scripts.")
    (description
      "A library to run the pkg-config system tool at build time in order to be used in Cargo build scripts.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-proc-macro-nested ; guix upstreamable
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
        (("rust-criterion" ,rust-criterion)
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
        (("rust-quickcheck" ,rust-quickcheck))))
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

(define-public rust-rand-0.3
  (package
    (inherit rust-rand)
    (name "rust-rand")
    (version "0.3.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0v679h38pjjqj5h4md7v2slsvj6686qgcn7p9fbw3h43iwnk1b34"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-rand" ,rust-rand-0.4))))))

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
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jis94x9ri8xlxki2w2w5k29sjpfwgzkjylg7paganp74hrnhpk1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-getrandom" ,rust-getrandom)
         ("rust-serde" ,rust-serde)
         ("rust-serde-derive" ,rust-serde-derive))))
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
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h3dbrhi5qgflqnzzd86s48v1dn1l17bmdssi5q170whsm4sbryh"))))
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
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jzdgszfa4bliigiy4hi66k7fs3gfwi2qxn8vik84ph77fwdwvvs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand-core" ,rust-rand-core-0.4))))))

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
        ("rust-serde" ,rust-serde-derive))
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
         ("rust-serde" ,rust-serde-derive))
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

(define-public rust-redox-syscall ; guix upstreamable
  (package
    (name "rust-redox-syscall")
    (version "0.1.56")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "110y7dyfm2vci4x5vk7gr0q551dvp31npl99fnsx2fb17wzwcf94"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.redox-os.org/redox-os/syscall")
    (synopsis "Rust library to access raw Redox system calls")
    (description "This package provides a Rust library to access raw Redox
system calls.")
    (license license:expat)))

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
    (license #f)))

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
         ("rust-quickcheck" ,rust-quickcheck)
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

(define-public rust-regex-syntax
  (package
    (name "rust-regex-syntax")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p47lf38yj2g2fnmvnraccqlxwk35zr76hlnqi8yva932nzqam6d"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ucd-util" ,rust-ucd-util))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "A regular expression parser.")
    (description
      "This package provides a regular expression parser.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-remove-dir-all
  (package
    (name "rust-remove-dir-all")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "remove_dir_all" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0bkrlyg26mgizpiy1yb2hhpgscxcag8r5fnckqsvk25608vzm0sa"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment))))
    (home-page
      "https://github.com/XAMPPRocky/remove_dir_all.git")
    (synopsis
      "A safe, reliable implementation of remove_dir_all for Windows")
    (description
      "This package provides a safe, reliable implementation of remove_dir_all for Windows")
    (license (list license:asl2.0
                   license:expat))))

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

(define-public rust-rustc-demangle
  (package
    (name "rust-rustc-demangle")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1brqf2bknkxsdzn3kd3wfifvzfc33bmvdy9r1k6fp4a8dz7xrx57"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-compiler-builtins"
          ,rust-compiler-builtins)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core))))
    (home-page
      "https://github.com/alexcrichton/rustc-demangle")
    (synopsis "Rust compiler symbol demangling.")
    (description
      "Rust compiler symbol demangling.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-serialize
  (package
    (name "rust-rustc-serialize")
    (version "0.3.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-serialize" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nkg3vasg7nk80ffkazizgiyv3hb1l9g3d8h17cajbkx538jiwfw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-rand" ,rust-rand-0.3))))
    (home-page
      "https://github.com/rust-lang/rustc-serialize")
    (synopsis
      "Generic serialization/deserialization support corresponding to the `derive(RustcEncodable, RustcDecodable)` mode in the compiler. Also includes support for hex, base64, and json encoding and decoding.")
    (description
      "Generic serialization/deserialization support corresponding to the `derive(RustcEncodable, RustcDecodable)` mode in the compiler. Also includes support for hex, base64, and json encoding and decoding.")
    (license (list license:asl2.0
                   license:expat))))

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
         ("rust-time" ,rust-time))
        #:cargo-development-inputs
        (("rust-rustc-version" ,rust-rustc-version))))
    (home-page
      "https://github.com/SimonSapin/rustc-test")
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
         ("rust-rand" ,rust-rand))))
    (home-page "https://github.com/dtolnay/ryu")
    (synopsis
      "Fast floating point to string conversion")
    (description
      "Fast floating point to string conversion")
    (license (list license:asl2.0
                   license:boost1.0))))

(define-public rust-same-file
  (package
    (name "rust-same-file")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "same-file" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "08a4zy10pjindf2rah320s6shgswk13mqw7s61m8i1y1xpf8spjq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-winapi-util" ,rust-winapi-util))))
    (home-page
      "https://github.com/BurntSushi/same-file")
    (synopsis
      "A simple crate for determining whether two file paths point to the same file.")
    (description
      "This package provides a simple crate for determining whether two file paths point to the same file.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-scopeguard-0.3
  (package
    (inherit rust-scopeguard)
    (name "rust-scopeguard")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "09sy9wbqp409pkwmqni40qmwa99ldqpl48pp95m1xw8sc19qy9cl"))))))

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
      "https://github.com/dtolnay/select-rustc")
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

(define-public rust-semver-parser-0.7
  (package
    (inherit rust-semver-parser)
    (name "rust-semver-parser")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))))))

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

(define-public rust-serde-json-1.0.39
  (package
    (inherit rust-serde-json)
    (name "rust-serde-json")
    (version "1.0.39")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "03fl9l680gij0hrsr2csfm8nm858igvfy05czbdkzm54siqsl8ss"))))))

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
        (("rust-bindgen" ,rust-bindgen)
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

(define-public rust-stacker
  (package
    (name "rust-stacker")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stacker" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0js0axz5nla1mkr2dm2vrv9rj964ng1lrv4l43sqlnfgawplhygv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi)
         ("rust-cc" ,rust-cc))))
    (home-page
      "https://github.com/alexcrichton/stacker")
    (synopsis
      "A stack growth library useful when implementing deeply recursive algorithms that
      may accidentally blow the stack.")
    (description
      "This package provides a stack growth library useful when implementing deeply recursive algorithms that
      may accidentally blow the stack.")
    (license (list license:asl2.0
                   license:expat))))

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

(define-public rust-stdweb-internal-test-macro
  (package
    (name "rust-stdweb-internal-test-macro")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-test-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12rrm7p77xnm3xacgn3rgniiyyjb4gq7902wpbljsvbx045z69l2"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal crate of the `stdweb` crate")
    (description
      "Internal crate of the `stdweb` crate")
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

(define-public rust-streaming-stats
  (package
    (name "rust-streaming-stats")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "streaming-stats" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0l7xz4g6709s80zqpvlhrg0qhgz64r94cwhmfsg8xhabgznbp2px"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-num-traits" ,rust-num-traits))))
    (home-page
      "https://github.com/BurntSushi/rust-stats")
    (synopsis
      "Experimental crate for computing basic statistics on streams.")
    (description
      "Experimental crate for computing basic statistics on streams.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-strsim-0.8
  (package
    (inherit rust-strsim)
    (name "rust-strsim")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))))))

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
         ("rust-structopt-derive" ,rust-structopt-derive))))
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
    (version "0.15.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0png5pz7jjjj39xy8w7qr65y3s9qam0jpz6nbmal06m5dhq0kp7a"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote)
         ("rust-unicode-xid" ,rust-unicode-xid))
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
         ("rust-unicode-xid" ,rust-unicode-xid))
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

(define-public rust-tempdir
  (package
    (name "rust-tempdir")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempdir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1n5n86zxpgd85y0mswrp5cfdisizq2rv3la906g6ipyc03xvbwhm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-rand" ,rust-rand-0.4)
         ("rust-remove-dir-all" ,rust-remove-dir-all))))
    (home-page
      "https://github.com/rust-lang/tempdir")
    (synopsis
      "A library for managing a temporary directory and deleting all contents when it's dropped.")
    (description
      "This package provides a library for managing a temporary directory and deleting all contents when it's dropped.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tempfile
  (package
    (name "rust-tempfile")
    (version "3.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempfile" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a9cfdqw70n7bcnkx05aih9xdba8lqazmqlkjpkmn2la6gcj8vks"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-cfg-if" ,rust-cfg-if)
         ("rust-libc" ,rust-libc)
         ("rust-rand" ,rust-rand)
         ("rust-redox-syscall" ,rust-redox-syscall)
         ("rust-remove-dir-all" ,rust-remove-dir-all)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "http://stebalien.com/projects/tempfile-rs")
    (synopsis
      "A library for managing temporary files and directories.")
    (description
      "This package provides a library for managing temporary files and directories.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-term
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
    (version "1.0.0-beta1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "term_size" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13w9cqjhzh3mmx6zami8lxyf42xx53yy866zxhxqcm71k637v8d8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-clippy" ,rust-clippy)
         ;("rust-kernel32-sys" ,rust-kernel32-sys) ; windows
         ("rust-libc" ,rust-libc)
         ("rust-winapi" ,rust-winapi))))
    (home-page
      "https://github.com/kbknapp/term_size-rs.git")
    (synopsis
      "functions for determining terminal sizes and dimensions")
    (description
      "functions for determining terminal sizes and dimensions")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-termcolor
  (package
    (name "rust-termcolor")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0vjfsn1a8zvqhnrbygrz1id6yckwv1dncw3w4zj65qdx0f00kmln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-wincolor" ,rust-wincolor))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis "Library for writing colored text to a terminal")
    (description "This package provides a simple cross platform library for
writing colored text to a terminal.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-termios
  (package
    (name "rust-termios")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termios" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09any1p4jp4bphvb5ikagnvwjc3xn2djchy96nkpa782xb2j1dkj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-libc" ,rust-libc))))
    (home-page
      "https://github.com/dcuddeback/termios-rs")
    (synopsis
      "Safe bindings for the termios library.")
    (description
      "Safe bindings for the termios library.")
    (license license:expat)))

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

(define-public rust-thread-local
  (package
    (name "rust-thread-local")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread_local" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "06rzik99p8c5js8238yhc8rk6np543ylb1dy9nrw5v80j0r3xdf6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static))))
    (home-page
      "https://github.com/Amanieu/thread_local-rs")
    (synopsis "Per-object thread-local storage")
    (description "Per-object thread-local storage")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-time
  (package
    (name "rust-time")
    (version "0.1.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0vsbvsz0ryxb35dy9j4anxvy8zlaplmjmi0a4z4l64bc135cz3fv"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-libc" ,rust-libc)
         ("rust-redox-syscall" ,rust-redox-syscall)
         ("rust-rustc-serialize" ,rust-rustc-serialize)
         ("rust-winapi" ,rust-winapi))
        #:cargo-development-inputs
        (("rust-log" ,rust-log)
         ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/rust-lang/time")
    (synopsis
      "Utilities for working with time-related functions in Rust.")
    (description
      "Utilities for working with time-related functions in Rust.")
    (license (list license:asl2.0
                   license:expat))))

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
         ("rust-parking-lot" ,rust-parking-lot)
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

(define-public rust-tokio-mock-task
  (package
    (name "rust-tokio-mock-task")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-mock-task" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1y7q83qfk9ljjfvs82b453pmz9x1v3d6kr4x55j8mal01s6790dw"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-futures" ,rust-futures))))
    (home-page
      "https://github.com/carllerche/tokio-mock-task")
    (synopsis "Mock a Tokio task")
    (description "Mock a Tokio task")
    (license license:expat)))

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

(define-public rust-tracing-core
  (package
    (name "rust-tracing-core")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "01fa73wzw2m5ybi3kkd52dgrw97mgc3i6inmhwys46ab28giwnxi"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-lazy-static" ,rust-lazy-static))))
    (home-page "https://tokio.rs")
    (synopsis
      "Core primitives for application-level tracing.")
    (description
      "Core primitives for application-level tracing.")
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

(define-public rust-ucd-trie
  (package
    (name "rust-ucd-trie")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-trie" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hh6kyzh5xygwy96wfmsf8v8czlzhps2lgbcyhj1xzy1w1xys04g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-lazy-static" ,rust-lazy-static))))
    (home-page
      "https://github.com/BurntSushi/ucd-generate")
    (synopsis
      "A trie for storing Unicode codepoint sets and maps.")
    (description
      "This package provides a trie for storing Unicode codepoint sets and maps.")
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
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1a9jqg7rb1yq6w2xc9jgxcs111yk5vxm9afjfvykfnrmzk6z8rqr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-quickcheck" ,rust-quickcheck))))
    (home-page
      "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis
      "This crate provides Grapheme Cluster, Word and Sentence boundaries
      according to Unicode Standard Annex #29 rules.")
    (description
      "This crate provides Grapheme Cluster, Word and Sentence boundaries
      according to Unicode Standard Annex #29 rules.")
    (license (list license:asl2.0
                   license:expat))))

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
         ("rust-quickcheck" ,rust-quickcheck))))
    (home-page
      "https://github.com/BurntSushi/utf8-ranges")
    (synopsis
      "Convert ranges of Unicode codepoints to UTF-8 byte ranges.")
    (description
      "Convert ranges of Unicode codepoints to UTF-8 byte ranges.")
    (license (list license:unlicense
                   license:expat))))

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

(define-public rust-vcpkg
  (package
    (name "rust-vcpkg")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vcpkg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "15dzk1b96q946v9aisbd1bbhi33n93wvgziwh1shmscn1xflbp9k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-development-inputs
        (("rust-lazy-static" ,rust-lazy-static)
         ("rust-tempdir" ,rust-tempdir))))
    (home-page "https://github.com/mcgoo/vcpkg-rs")
    (synopsis
      "A library to find native dependencies in a vcpkg tree at build time in order to be used in Cargo build scripts.")
    (description
      "A library to find native dependencies in a vcpkg tree at build time in order to be used in Cargo build scripts.")
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
         ("rust-pulldown-cmark" ,rust-pulldown-cmark)
         ("rust-regex" ,rust-regex)
         ("rust-semver-parser" ,rust-semver-parser)
         ("rust-syn" ,rust-syn)
         ("rust-toml" ,rust-toml)
         ("rust-url" ,rust-url))))
    (home-page
      "https://github.com/mgeisler/version-sync")
    (synopsis
      "Simple crate for ensuring that version numbers in README files are
      updated when the crate version changes.")
    (description
      "Simple crate for ensuring that version numbers in README files are
      updated when the crate version changes.")
    (license license:expat)))

(define-public rust-walkdir
  (package
    (name "rust-walkdir")
    (version "2.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "walkdir" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "07ppalpvxkf8cnqr64np422792y4z5bs9m8b4nrflh5rm17wjn4n"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-same-file" ,rust-same-file)
         ("rust-winapi" ,rust-winapi)
         ("rust-winapi-util" ,rust-winapi-util))
        #:cargo-development-inputs
        (("rust-doc-comment" ,rust-doc-comment))))
    (home-page
      "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory.")
    (description "Recursively walk a directory.")
    (license (list license:unlicense
                   license:expat))))

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

(define-public rust-wasm-bindgen-test-macro
  (package
    (name "rust-wasm-bindgen-test-macro")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-test-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n28mr6vncf1k1qr2b5bvfxq4jvqkjdzq0z0ab6w2f5d6v8q3q3l"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2)
         ("rust-quote" ,rust-quote))))
    (home-page
      "https://github.com/rustwasm/wasm-bindgen")
    (synopsis
      "Internal testing macro for wasm-bindgen")
    (description
      "Internal testing macro for wasm-bindgen")
    (license (list license:asl2.0
                   license:expat))))

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
      "https://github.com/harryfei/which-rs.git")
    (synopsis
      "A Rust equivalent of Unix command \"which\". Locate installed execuable in cross platforms.")
    (description
      "This package provides a Rust equivalent of Unix command \"which\".  Locate installed execuable in cross platforms.")
    (license license:expat)))

(define-public rust-winapi-util
  (package
    (name "rust-winapi-util")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1j839dc6y8vszvrsb7yk0qvs0w6asnahxzbyans37vnsw6vbls3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "Dumping ground for high level safe wrappers over winapi")
    (description
     "This package provides a dumping ground for high level safe wrappers over
winapi.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-wincolor
  (package
    (name "rust-wincolor")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wincolor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1fp9sxq63kw3vjjcjrl3f7px082pplzxcr3qza2n2pa6mq0xj7jn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi)
        ("rust-winapi-util" ,rust-winapi-util))))
    (home-page "https://github.com/BurntSushi/termcolor/tree/master/wincolor")
    (synopsis
      "A simple Windows specific API for controlling text color in a Windows console.")
    (description
      "This package provides a simple Windows specific API for controlling text color in a Windows console.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-xdg
  (package
    (name "rust-xdg")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xdg" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0mws8a0fr3cqk5nh7aq9lmkmhzghvasqy4mhw6nnza06l4d6i2fh"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/whitequark/rust-xdg")
    (synopsis
      "A library for storing and retrieving files according to XDG Base Directory specification")
    (description
      "This package provides a library for storing and retrieving files according to XDG Base Directory specification")
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
        (("rust-quickcheck" ,rust-quickcheck))))
    (home-page
      "http://chyh1990.github.io/yaml-rust/")
    (synopsis "The missing YAML 1.2 parser for rust")
    (description
      "The missing YAML 1.2 parser for rust")
    (license (list license:asl2.0
                   license:expat))))
