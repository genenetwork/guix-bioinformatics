;; Experimental packaging for the Common Workflow Language (started by Bruno)

(define-module (gn packages cwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gn packages python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-cwltool
  (package
    (name "python-cwltool")
    (version "1.0.20150916041152")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/c/cwltool/cwltool-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1kqxc6nvq4nr8qdv39ycdi6fhzaipgjpmbghsz94ij6jhf5r3dq2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-schema-salad" ,python-schema-salad)))
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (home-page
      "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis
      "Common workflow language reference implementation")
    (description
      "Common workflow language reference implementation")
    (license license:asl2.0)))

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
    (home-page
      "https://github.com/RDFLib/rdflib-jsonld")
    (synopsis
      "rdflib extension adding JSON-LD parser and serializer")
    (description
      "rdflib extension adding JSON-LD parser and serializer")
    (license license:bsd-3)))
    
(define-public python-schema-salad
  (package
    (name "python-schema-salad")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/s/schema-salad/schema-salad-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "09axiiirq73s1cs21n8mkdslaca2gxc2mlayyl6yiaq98cfgfh37"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; No tests.
    (inputs
      `(("python-cython" ,python-cython)
       ("python-setuptools" ,python-setuptools)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ("python-rdflib" ,python-rdflib)
       ("python-mistune" ,python-mistune)))
    (propagated-inputs
     `(("python-rdflib" ,python-rdflib)))
    (home-page
      "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis
      "Schema Annotations for Linked Avro Data (SALAD)")
    (description
      "Schema Annotations for Linked Avro Data (SALAD)")
    (license license:asl2.0)))
    
(define-public python-rdflibx
  (package
    (name "python-rdflibx")
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

