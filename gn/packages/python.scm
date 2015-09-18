(define-module (gn packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-rdflib-jsonld
  (package
    (name "python-rdflib-jsonld")
    (version "0.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/r/rdflib-jsonld/rdflib-jsonld-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "121a876k49xl85jvikyh4hzvm34456ikw66cra5dfyr15br1qjll"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; No tests.
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-rdflib-4.2" ,python-rdflib-4.2)
       ("python-pyparsing" ,python-pyparsing)
))
    (home-page
      "https://github.com/RDFLib/rdflib-jsonld")
    (synopsis
      "rdflib extension adding JSON-LD parser and serializer")
    (description
      "rdflib extension adding JSON-LD parser and serializer")
    (license license:bsd-3)))

(define-public python2-rdflib-jsonld
  (package-with-python2 python-rdflib-jsonld))


(define-public python-rdflib-4.2
  (package
    (name "python-rdflib-4.2")
    (version "4.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://pypi.python.org/packages/source/r/rdflib/rdflib-"
              version
              ".tar.gz"))
        (patches
          ;; The patch has no effect under Python 3.
          (list (search-patch "python2-rdflib-drop-sparqlwrapper.patch")))
        (sha256
          (base32
            "1h3f8yl9frjz8rsykjdjk83qsrcvld3qa7pkzh69s91h97ydl83l"))))
    (build-system python-build-system)
    (inputs
      `(("python-html5lib" ,python-html5lib)
        ("python-isodate" ,python-isodate)
        ("python-pyparsing" ,python-pyparsing)
        ("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/RDFLib/rdflib")
    (synopsis
      "Python RDF library")
    (description
      "RDFLib is a Python library for working with RDF, a simple yet
powerful language for representing information.")
    (license (license:non-copyleft "file://LICENSE"
                           "See LICENSE in the distribution."))))

(define-public python2-rdflib-4.2
  (package-with-python2 python-rdflib-4.2))

(define-public python-avro
(package
  (name "python-avro")
  (version "1.7.7")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/a/avro/avro-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0n21lfclah7bmqnnqfqmpsrimz0s86qkxyn972jynq234n1lyynf"))))
  (build-system python-build-system)
  (inputs
    `(("python-setuptools" ,python-setuptools)))
  (home-page "http://hadoop.apache.org/avro")
  (synopsis
    "Avro is a serialization and RPC framework.")
  (description
    "Avro is a serialization and RPC framework.")
  (license #f)))

(define-public python2-avro
  (package-with-python2 python-avro))

(define-public python-shellescape
(package
  (name "python-shellescape")
  (version "3.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/s/shellescape/shellescape-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0n5ky1b2vw2y0d4xl3qybyp2rk0gq5frjs8nr8ak6mgj2fyb4676"))))
  (build-system python-build-system)
  (inputs
    `(("python-setuptools" ,python-setuptools)))
  (home-page
    "https://github.com/chrissimpkins/shellescape")
  (synopsis
    "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
  (description
    "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
  (license expat))
)

(define-public python2-shellescape
  (package-with-python2 python-shellescape))
