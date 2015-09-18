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
       ("python-rdflib-4.2" ,python-rdflib-4.2)
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
    
