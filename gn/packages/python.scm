(define-module (gn packages python)
  #:use-module ((guix licenses)
                #:select (asl2.0 bsd-4 bsd-3 bsd-2 non-copyleft cc0 x11 x11-style
                          gpl2 gpl2+ gpl3+ lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3+ agpl3+
                          isc psfl public-domain x11-style))
  #:use-module ((guix licenses) #:select (expat zlib) #:prefix license:)
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

(define-public python-rdflib
(package
  (name "python-rdflib")
  (version "4.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/r/rdflib/rdflib-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1j082qr0h7pzr33divq3w4n3k9jpcmbkxnhkw8k3pvq6aqivs0pb"))))
  (build-system python-build-system)
  (inputs
   `(("python-setuptools" ,python-setuptools)))
  (propagated-inputs `(("python-pyparsing" ,python-pyparsing)
                       ("python-isodate" ,python-isodate))
  (home-page "https://github.com/RDFLib/rdflib")
  (synopsis
    "RDFLib is a Python library for working with RDF, a simple yet powerful language for representing information.")
  (description
    "RDFLib is a Python library for working with RDF, a simple yet powerful language for representing information.")
  (license #f)))

