(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo))

;; Please keep these packages sorted alphabetically

(define-public rust-arrayvec-0.4
  (package
    (name "rust-arrayvec")
    (version "0.4.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "arrayvec" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1fmhq4ljxr954mdyazaqa9kdxryl5d2ggr5rialylrd6xndkzmxq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis
      "A vector with fixed capacity, backed by an array (it can be stored on the stack too). Implements fixed capacity ArrayVec and ArrayString.")
    (description
      "This package provides a vector with fixed capacity, backed by an array (it can be stored on the stack too).  Implements fixed capacity ArrayVec and ArrayString.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crossbeam-deque-0.2
  (package
    (name "rust-crossbeam-deque")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crossbeam-deque" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1wwwbnvxh0rza38xiws8qc46klzhv19zgvarn37pijis6v2zhfgp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque")
    (properties '((hidden? . #t)))
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
          (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0l4igvp2i7b6dgaiq040j8kj8hygwdpr6ppzh1hrbsbx83sj2wcj"))))
      (build-system cargo-build-system)
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description "Epoch-based garbage collection")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1n8qr52sw9y6yxzyfxi1phh55rsxms7ry4iipdd8vmd16ag8jq17"))))
      (build-system cargo-build-system)
    (home-page "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
      "Utilities for concurrent programming")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0yyggfd5yq9hyyp0bd5jj0fgz3rwws42d19ri0znxwwqs3hcy9sm"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/either")
    (synopsis
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (description
      "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-heck-0.3
  (package
    (name "rust-heck")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heck" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "01a2v7yvkiqxakdqz4hw3w3g4sm52ivz9cs3qcsv2arxsmw4wmi0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/withoutboats/heck")
    (synopsis "heck is a case conversion library.")
    (description
      "heck is a case conversion library.")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "03cpsj26xmyamcalclqzr1i700vwx8hnbgxbpjvs354f8mnr8iqd"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
      "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-matrixmultiply-0.1
  (package
    (name "rust-matrixmultiply")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matrixmultiply" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "00p0fpjhm45qdzi37mgv7ggsy8b9gqvq4999yrbgyn1dxkf6gbfw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/matrixmultiply/")
    (synopsis "General matrix multiplication for f32 and f64 matrices.")
    (description "General matrix multiplication for f32 and f64 matrices. Operates on matrices with general layout (they can use arbitrary row and column stride). Detects and uses AVX or SSE2 on x86 platforms transparently for higher performance. Uses a microkernel strategy, so that the implementation is easy to parallelize and optimize.")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1cvm2z7dy138s302ii7wlzcxbka5a8yfl5pl5di7lbdnw9hw578g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Gilnaa/memoffset")
    (synopsis
      "offset_of functionality for Rust structs.")
    (description
      "offset_of functionality for Rust structs.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-ndarray-0.12
  (package
    (name "rust-ndarray")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ndarray" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0a5rfwcbqnvbwi3nw5sfz6kf0flhmjxs64s0b4kxc6lhmyl81wvw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
      "ndarray implements an n-dimensional container for general elements and for numerics.")
    (description
      "ndarray implements an n-dimensional container for general elements and for numerics.")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1z6zjdzx1g1hj4y132ddy83d3p3zvw06igbf59npxxrzzcqwzc7w"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-num/num-complex")
    (synopsis
      "Complex numbers implementation for Rust")
    (description
      "Complex numbers implementation for Rust")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
        (base32
        "0wq41f15y05nlarijn9c1vxscxj5sazn3lhd6mmnicj5fzr18f1p"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis
      "Simple work-stealing parallelism for Rust")
    (description
      "Simple work-stealing parallelism for Rust")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0mkkabm3h4xvrkvjp675c07zcpcb7jk09rlg9mbpfs5s5blx2mdh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "Core APIs for Rayon")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-structopt-0.2
  (package
    (name "rust-structopt")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "structopt" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1mvfv1l8vp3y402fkl2wcl34hi7gmr4bqha13dfz2xf3kjzwvhhn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct.")
    (description
      "Parse command line argument by defining a struct.")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "01sis9z5kqmyhvzbnmlzpdxcry99a0b9blypksgnhdsbm1hh40ak"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/TeXitoi/structopt")
    (synopsis
      "Parse command line argument by defining a struct, derive crate.")
    (description
      "Parse command line argument by defining a struct, derive crate.")
    (properties '((hidden? . #t)))
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
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1id5g6x6zihv3j7hwrw3m1jp636bg8dpi671r7zy3jvpkavb794w"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-segmentation-1.3
  (package
    (name "rust-unicode-segmentation")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-segmentation" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1a9jqg7rb1yq6w2xc9jgxcs111yk5vxm9afjfvykfnrmzk6z8rqr"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-segmentation")
    (synopsis "Grapheme Cluster, Word and Sentence boundaries for rust")
    (description
     "This crate provides Grapheme Cluster, Word and Sentence boundaries
according to Unicode Standard Annex #29 rules.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))
