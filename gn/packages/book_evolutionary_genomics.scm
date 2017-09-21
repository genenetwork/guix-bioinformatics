;; Module that goes with the 'Evolutionary Genomics' book

(define-module (gn packages genenetwork)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  ;; #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  ; #:use-module (gnu packages gcc)
  ; #:use-module (gnu packages graphviz)
  ; #:use-module (gnu packages java)
  ; #:use-module (gnu packages linux)
  ; #:use-module (gnu packages ldc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages vim)
  ; #:use-module (gnu packages web)
  ; #:use-module (gnu packages xml)
  #:use-module (gnu packages version-control)

  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages javascript)
  ; #:use-module (gn packages python)
  ; #:use-module (gn packages statistics)
  #:use-module (srfi srfi-1))

(define-public r-gener
  (package
    (name "r-gener")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       ; (uri (bioconductor-uri "GeneR" version))
       (uri "http://www.bioconductor.org/packages//2.7/bioc/src/contrib/GeneR_2.20.0.tar.gz")
       (sha256
        (base32
         "1qrrq5lrm2wvx3vlas6s84spwnlaji7jaascljcr9078ww8vmjxp"))))
    (build-system r-build-system)
    (home-page "http://bioconductor.org/packages/GeneR")
    (synopsis "Package manipulating nucleotidic sequences (Embl, Fasta, GenBank)")
    (description
     ".")
    (license license:expat))) ; CeCILL-2.0

(define-public book-evolutionary-genomics
  (let ((md5 "93e745e9c"))
    (package
    (name "book-evolutionary-genomics")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "http://files.genenetwork.org/raw_database/md5sum.txt") ; any old file
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "1cnkiwid4h0nnf93rm647ji9vhfzjl23arp1xj374la7mmic9jqs"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    (propagated-inputs
     `(("python" ,python)
       ("r" ,r)
       ))
    (inputs
     `(
       ("r-gener" ,r-gener)
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

    (home-page "http://github.com/pjotrp/")
    (synopsis "Packages for Evolutionary Genomics book")
    (description "More later...")
    (license license:agpl3+))))
