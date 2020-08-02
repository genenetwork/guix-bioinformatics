(define-module (gn packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io))

;; Please keep these packages sorted alphabetically

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
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-itertools" ,rust-itertools-0.7)
        ("rust-matrixmultiply" ,rust-matrixmultiply-0.1)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ;("rust-blas-src" ,rust-blas-src-0.2)
        ("rust-cblas-sys" ,rust-cblas-sys-0.1)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-defmac" ,rust-defmac-0.1)
        ("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rawpointer" ,rust-rawpointer-0.1))))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis "n-dimensional container for general elements and for numerics")
    (description "@code{ndarray} implements an n-dimensional container for
general elements and for numerics.")
    (license (list license:asl2.0
                   license:expat))))
