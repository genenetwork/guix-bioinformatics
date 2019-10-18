(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo))

;; Please keep these packages sorted alphabetically

(define-public rust-ansi-term-0.11
  (package
    (name "rust-ansi-term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles")
    (description
     "This is a library for controlling colours and formatting, such as red bold
text or blue underlined text, on ANSI terminals.")
    (license license:expat)))

(define-public rust-arrayvec-0.4
  (package
    (name "rust-arrayvec")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arrayvec" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1fmhq4ljxr954mdyazaqa9kdxryl5d2ggr5rialylrd6xndkzmxq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis
      "A vector with fixed capacity, backed by an array (it can be stored on the stack too). Implements fixed capacity ArrayVec and ArrayString.")
    (description
      "This package provides a vector with fixed capacity, backed by an array (it can be stored on the stack too).  Implements fixed capacity ArrayVec and ArrayString.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-atty-0.2
  (package
    (name "rust-atty")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "140sswp1bwqwc4zk80bxkbnfb3g936hgrb77g9g0k1zcld3wc0qq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
     "This package provides a simple interface for querying atty.")
    (license license:expat)))

(define-public rust-autocfg-0.1
  (package
    (name "rust-autocfg")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0asl6fnc35yk5l2rxwhp25v128jgm45dp754h9z8x51b6n90w4r2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Rust library for build scripts to automatically configure
code based on compiler support.  Code snippets are dynamically tested to see
if the @code{rustc} will accept them, rather than hard-coding specific version
support.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bitflags-1.0
  (package
    (name "rust-bitflags")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "04nfhscc9mxwhmai5xgwh4q458rjszmwsvkpf752g1j6dyklg012"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "Macro to generate structures which behave like bitflags")
    (description "This package provides a macro to generate structures which
behave like a set of bitflags.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cfg-if-0.1
  (package
    (name "rust-cfg-if")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0csygklgz3ybpr0670rkip49zh76m43ar3k7xgypkzbzrwycx1ml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "Define an item depending on parameters")
    (description "This package provides a macro to ergonomically define an item
depending on a large number of #[cfg] parameters.  Structured like an
@code{if-else} chain, the first matching branch is the item that gets emitted.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-clap-2.33
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
    (home-page "https://clap.rs/")
    (synopsis "Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured
Command Line Argument Parser.")
    (license license:expat)))

(define-public rust-cloudabi-0.0
  (package
    (name "rust-cloudabi")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cloudabi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0kxcg83jlihy0phnd2g8c2c303px3l2p3pkjz357ll6llnd5pz6x"))))
    (build-system cargo-build-system)
    (home-page "https://nuxi.nl/cloudabi/")
    (synopsis "Low level interface to CloudABI")
    (description
     "Low level interface to CloudABI.  Contains all syscalls and related types.")
    (license license:bsd-2)))

(define-public rust-crossbeam-deque-0.2
  (package
    (name "rust-crossbeam-deque")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-deque" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1wwwbnvxh0rza38xiws8qc46klzhv19zgvarn37pijis6v2zhfgp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-epoch-0.3
  (package
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
      (build-system cargo-build-system)
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description "Epoch-based garbage collection")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-utils-0.2
  (package
    (name "rust-crossbeam-utils")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-utils" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1n8qr52sw9y6yxzyfxi1phh55rsxms7ry4iipdd8vmd16ag8jq17"))))
      (build-system cargo-build-system)
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
      "Utilities for concurrent programming")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-either-1.5
  (package
    (name "rust-either")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "either" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0yyggfd5yq9hyyp0bd5jj0fgz3rwws42d19ri0znxwwqs3hcy9sm"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/either")
    (synopsis
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fuchsia-cprng-0.1
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

(define-public rust-heck-0.3
  (package
    (name "rust-heck")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heck" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "01a2v7yvkiqxakdqz4hw3w3g4sm52ivz9cs3qcsv2arxsmw4wmi0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      "heck is a case conversion library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itertools-0.7
  (package
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
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itoa-0.4
  (package
    (name "rust-itoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0zvg2d9qv3avhf3d8ggglh6fdyw8kkwqg3r4622ly5yhxnvnc4jh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis "Fast functions for printing integer primitives")
    (description "This crate provides fast functions for printing integer
primitives to an @code{io::Write}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-lazy-static-1.3
  (package
    (name "rust-lazy-static")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy_static" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "052ac27w189hrf1j3hz7sga46rp84zl2hqnzyihxv78mgzr2jmxw"))))
    (build-system cargo-build-system)
    (home-page  "https://github.com/rust-lang-nursery/lazy-static.rs")
    (synopsis "Macro for declaring lazily evaluated statics in Rust")
    (description
     "This package provides a macro for declaring lazily evaluated statics in
Rust.  Using this macro, it is possible to have @code{static}s that require code
to be executed at runtime in order to be initialized.  This includes anything
requiring heap allocations, like vectors or hash maps, as well as anything that
requires non-const function calls to be computed.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.62")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fh69kpjg8hqff36kdczx7sax98gk4qs4ws1dwvjz0rgip0d5z1l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc")
    (description
     "libc provides all of the definitions necessary to easily
interoperate with C code (or \"C-like\" code) on each of the platforms
that Rust supports. This includes type definitions (e.g., c_int),
constants (e.g., EINVAL) as well as function headers (e.g., malloc).

This crate exports all underlying platform types, functions, and
constants under the crate root, so all items are accessible as
@samp{libc::foo}.  The types and values of all the exported APIs match
the platform that libc is compiled for.")
    (license (list license:expat
                   license:asl2.0))))

(define-public rust-matrixmultiply-0.1
  (package
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
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/matrixmultiply/")
    (synopsis "General matrix multiplication for f32 and f64 matrices.")
    (description "General matrix multiplication for f32 and f64 matrices. Operates on matrices with general layout (they can use arbitrary row and column stride). Detects and uses AVX or SSE2 on x86 platforms transparently for higher performance. Uses a microkernel strategy, so that the implementation is easy to parallelize and optimize.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-memoffset-0.2
  (package
    (name "rust-memoffset")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memoffset" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1cvm2z7dy138s302ii7wlzcxbka5a8yfl5pl5di7lbdnw9hw578g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Gilnaa/memoffset")
    (synopsis
      "offset_of functionality for Rust structs.")
    (description
      "offset_of functionality for Rust structs.")
    (license license:expat)))

(define-public rust-ndarray-0.12
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
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
      "ndarray implements an n-dimensional container for general elements and for numerics.")
    (description
      "ndarray implements an n-dimensional container for general elements and for numerics.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-nodrop-0.1
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
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis "Wrapper type to inhibit drop (destructor)")
    (description "This package provides a wrapper type to inhibit drop
(destructor).  Use @code{std::mem::ManuallyDrop} instead!")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-complex-0.2
  (package
    (name "rust-num-complex")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-complex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1z6zjdzx1g1hj4y132ddy83d3p3zvw06igbf59npxxrzzcqwzc7w"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-num/num-complex")
    (synopsis
      "Complex numbers implementation for Rust")
    (description
      "Complex numbers implementation for Rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-cpus-1.10
  (package
    (name "rust-num-cpus")
    (version "1.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_cpus" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wrj3zvj6h3q26sqj9zxpd59frjb54n7jhjwf307clq31ic47vxw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/seanmonstar/num_cpus")
    (synopsis "Get the number of CPUs on a machine")
    (description
     "Get the number of CPUs on a machine.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-traits-0.2
  (package
    (name "rust-num-traits")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-traits" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0clvrm34rrqc8p6gq5ps5fcgws3kgq5knh7nlqxf2ayarwks9abb"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description "Numeric traits for generic mathematics.")
    ;; Dual licensed.
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-numtoa-0.1
  (package
    (name "rust-numtoa")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "numtoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1vs9rhggqbql1p26x8nkha1j06wawwgb2jp5fs88b5gi7prvvy5q"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.com/mmstick/numtoa")
    (synopsis
      "Convert numbers into stack-allocated byte arrays")
    (description
      "Convert numbers into stack-allocated byte arrays")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-0.4
  (package
    (name "rust-proc-macro2")
    (version "0.4.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nd71fl24sys066jrha6j7i34nfkjv44yzw8yww9742wmc8j0gfg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/proc-macro2")
    (synopsis "Stable implementation of the upcoming new `proc_macro` API")
    (description "This package provides a stable implementation of the upcoming new
`proc_macro` API.  Comes with an option, off by default, to also reimplement itself
in terms of the upstream unstable API.")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-quote-0.6
  (package
    (name "rust-quote")
    (version "0.6.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nw0klza45hf127kfyrpxsxd5jw2l6h21qxalil3hkr7bnf7kx7s"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-rand-0.6
  (package
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
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand")
    (synopsis
      "Random number generators and other randomness functionality.")
    (description
      "Random number generators and other randomness functionality.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-chacha-0.1
  (package
    (name "rust-rand-chacha")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1vxwyzs4fy1ffjc8l00fsyygpiss135irjf7nyxgq2v0lqf3lvam"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_chacha")
    (synopsis "ChaCha random number generator")
    (description "ChaCha random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-core-0.4
  (package
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
    (home-page "https://crates.io/crates/rand_core")
    (synopsis
      "Core random number generator traits and tools for implementation.")
    (description
      "Core random number generator traits and tools for implementation.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-core-0.3
  (package
    (inherit rust-rand-core-0.4)
    (name "rust-rand-core")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jzdgszfa4bliigiy4hi66k7fs3gfwi2qxn8vik84ph77fwdwvvs")))) ))

(define-public rust-rand-hc-0.1
  (package
    (name "rust-rand-hc")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_hc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1i0vl8q5ddvvy0x8hf1zxny393miyzxkwqnw31ifg6p0gdy6fh3v"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_hc")
    (synopsis "HC128 random number generator")
    (description "HC128 random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-isaac-0.1
  (package
    (name "rust-rand-isaac")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_isaac" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "027flpjr4znx2csxk7gxb7vrf9c7y5mydmvg5az2afgisp4rgnfy"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_isaac")
    (synopsis "ISAAC random number generator")
    (description "ISAAC random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-jitter-0.1
  (package
    (name "rust-rand-jitter")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_jitter" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16z387y46bfz3csc42zxbjq89vcr1axqacncvv8qhyy93p4xarhi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-random/rand")
    (synopsis
      "Random number generator based on timing jitter")
    (description
      "Random number generator based on timing jitter")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-os-0.1
  (package
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
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_os")
    (synopsis "OS backed Random Number Generator")
    (description "OS backed Random Number Generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-pcg-0.1
  (package
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
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_pcg")
    (synopsis
      "Selected PCG random number generators")
    (description
      "Selected PCG random number generators")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-xorshift-0.1
  (package
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
    (home-page "https://crates.io/crates/rand_xorshift")
    (synopsis "Xorshift random number generator")
    (description
      "Xorshift random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rawpointer-0.1
  (package
    (name "rust-rawpointer")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rawpointer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06ghpm9y7gacks78s3maakha07kbnwrxif5q37r2l7z1sali3b7b"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/rawpointer/")
    (synopsis "Extra methods for raw pointers")
    (description "Extra methods for raw pointers.  For example
@code{.post_inc()} and @code{.pre_dec()} (c.f. @code{ptr++} and @code{--ptr})
and @code{ptrdistance}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rayon-1.0
  (package
    (name "rust-rayon")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
        (base32
        "0wq41f15y05nlarijn9c1vxscxj5sazn3lhd6mmnicj5fzr18f1p"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis
      "Simple work-stealing parallelism for Rust")
    (description
      "Simple work-stealing parallelism for Rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rayon-core-1.4
  (package
    (name "rust-rayon-core")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rayon-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0mkkabm3h4xvrkvjp675c07zcpcb7jk09rlg9mbpfs5s5blx2mdh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "Core APIs for Rayon")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rdrand-0.4
  (package
    (name "rust-rdrand")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rdrand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1cjq0kwx1bk7jx3kzyciiish5gqsj7620dm43dc52sr8fzmm9037"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nagisa/rust_rdrand/")
    (synopsis
      "An implementation of random number generator based on rdrand and rdseed instructions")
    (description
      "An implementation of random number generator based on rdrand and rdseed instructions")
    (license license:isc)))

(define-public rust-redox-syscall-0.1
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
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-redox-termios-0.1
  (package
    (name "rust-redox-termios")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox-termios" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0xhgvdh62mymgdl3jqrngl8hr4i8xwpnbsxnldq0l47993z1r2by"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/redox-os/termios")
    (synopsis
      "A Rust library to access Redox termios functions")
    (description
      "This package provides a Rust library to access Redox termios functions")
    (license license:expat)))

(define-public rust-ryu-1.0
  (package
    (name "rust-ryu")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1j0h74f1xqf9hjkhanp8i20mqc1aw35kr1iq9i79q7713mn51a5z"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/ryu")
    (synopsis
      "Fast floating point to string conversion")
    (description
      "Fast floating point to string conversion")
    (license (list license:asl2.0 license:boost1.0))))

(define-public rust-scopeguard-0.3
  (package
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
          "09sy9wbqp409pkwmqni40qmwa99ldqpl48pp95m1xw8sc19qy9cl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/scopeguard")
    (synopsis "Scope guard which will run a closure even out of scope")
    (description "This package provides a RAII scope guard that will run a
given closure when it goes out of scope, even if the code between panics
(assuming unwinding panic).  Defines the macros @code{defer!},
@code{defer_on_unwind!}, @code{defer_on_success!} as shorthands for guards
with one of the implemented strategies.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-1.0
  (package
    (name "rust-serde")
    (version "1.0.97")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wlvfs82flb3di86m3nzf1m4vkc78vqcwrk865s0ldhrvgz3ssyl"))))
    (build-system cargo-build-system)
    (home-page "https://serde.rs")
    (synopsis
      "A generic serialization/deserialization framework")
    (description
      "This package provides a generic serialization/deserialization framework")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-derive-1.0
  (package
    (name "rust-serde-derive")
    (version "1.0.97")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0zq2qm2gabmpa57wxfxb09jl41nxccsk454715xjabzymlh0han2"))))
    (build-system cargo-build-system)
    (home-page "https://serde.rs")
    (synopsis
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-json-1.0
  (package
    (name "rust-serde-json")
    (version "1.0.40")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde_json" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "010pa89zx07aqx1cwgw2a603wcp3q5n2iy0k71ppqbr8kwi4j705"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description
      "This package provides a JSON serialization file format")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-strsim-0.8
  (package
    (name "rust-strsim")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Rust implementations of string similarity metrics")
    (description "This crate includes implementations of string similarity
metrics.  It includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro,
and Jaro-Winkler.")
    (license license:expat)))

(define-public rust-structopt-0.2
  (package
    (name "rust-structopt")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "structopt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1mvfv1l8vp3y402fkl2wcl34hi7gmr4bqha13dfz2xf3kjzwvhhn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct.")
    (description
      "Parse command line argument by defining a struct.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-structopt-derive-0.2
  (package
    (name "rust-structopt-derive")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "structopt-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "01sis9z5kqmyhvzbnmlzpdxcry99a0b9blypksgnhdsbm1hh40ak"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct, derive crate.")
    (description
      "Parse command line argument by defining a struct, derive crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-syn-0.15
  (package
    (name "rust-syn")
    (version "0.15.44")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1id5g6x6zihv3j7hwrw3m1jp636bg8dpi671r7zy3jvpkavb794w"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-termion-1.5
  (package
    (name "rust-termion")
    (version "1.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termion" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0c634rg520zjjfhwnxrc2jbfjz7db0rcpsjs1qici0nyghpv53va"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.redox-os.org/redox-os/termion")
    (synopsis
      "A bindless library for manipulating terminals.")
    (description
      "This package provides a bindless library for manipulating terminals.")
    (license license:expat)))

(define-public rust-textwrap-0.11
  (package
    (name "rust-textwrap")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mgeisler/textwrap")
    (synopsis "Library for word wrapping, indenting, and dedenting strings")
    (description
     "Textwrap is a small library for word wrapping, indenting, and dedenting
strings.  You can use it to format strings (such as help and error messages)
for display in commandline applications.  It is designed to be efficient and
handle Unicode characters correctly.")
    (license license:expat)))

(define-public rust-unicode-segmentation-1.3
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
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis "Grapheme Cluster, Word and Sentence boundaries for rust")
    (description
     "This crate provides Grapheme Cluster, Word and Sentence boundaries
according to Unicode Standard Annex #29 rules.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-width-0.1
  (package
    (name "rust-unicode-width")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-width" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "09k5lipygardwy0660jhls08fsgknrazzivmn804gps53hiqc8w8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width according to Unicode rules")
    (description "This crate allows you to determine displayed width of
@code{char} and @code{str} types according to Unicode Standard Annex #11 rules.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-xid-0.1
  (package
    (name "rust-unicode-xid")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1z57lqh4s18rr4x0j4fw4fmp9hf9346h0kmdgqsqx0fhjr3k0wpw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine Unicode XID related properties")
    (description "Determine whether characters have the XID_Start
or XID_Continue properties according to Unicode Standard Annex #31.")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-vec-map-0.8
  (package
    (name "rust-vec-map")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vec_map" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06n8hw4hlbcz328a3gbpvmy0ma46vg1lc0r5wf55900szf3qdiq5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/contain-rs/vec-map")
    (synopsis
      "A simple map based on a vector for small integer keys")
    (description
      "This package provides a simple map based on a vector for small integer keys")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-0.3
  (package
    (name "rust-winapi")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ii9j9lzrhwri0902652awifzx9fpayimbp6hfhhc296xcg0k4w0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Raw FFI bindings for all of Windows API.")
    (description
     "Raw FFI bindings for all of Windows API.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-i686-pc-windows-gnu-0.4
  (package
    (name "rust-winapi-i686-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-i686-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the i686-pc-windows-gnu target")
    (description "This crate provides import libraries for the
i686-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-x86-64-pc-windows-gnu-0.4
  (package
    (name "rust-winapi-x86-64-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-x86_64-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target")
    (description "This package provides import libraries for the
x86_64-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (license (list license:asl2.0
                   license:expat))))
