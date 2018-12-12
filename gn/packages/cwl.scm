;; Experimental packaging for the Common Workflow Language (started by Bruno)

(define-module (gn packages cwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages rdf)
  #:use-module (gn packages python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  ; #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  ; #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-cwltool ; guix: needs work
  (package
    (name "python-cwltool")
    (version "1.0.20181012180214")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/c/cwltool/cwltool-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0pk0jlac2vl6vfihdq07agzz9dasw84yjz5ladcbwnmzzl022cg7"))))
    (build-system python-build-system)
    (inputs
     `(("python-bagit" ,python-bagit)
       ("python-setuptools" ,python-setuptools)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-rdflib" ,python-rdflib)
       ("python-typing-extensions" ,python-typing-extensions)
       ("python-pyparsing" ,python-pyparsing)
       ("python-subprocess32" ,python-subprocess32)
       ))
    (propagated-inputs
     `(("python-schema-salad" ,python-schema-salad)
       ("python-prov" ,python-prov)
       ("python-html5lib" ,python-html5lib)
       ))
    ; (arguments `(#:tests? #f)) ;; CWL includes no tests.
    (home-page
      "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis
      "Common workflow language reference implementation")
    (description
      "Common workflow language reference implementation")
    (license license:asl2.0)))

(define-public python2-cwltool
  (package-with-python2 python-cwltool))

(define-public python-schema-salad
  (package
    (name "python-schema-salad")
    (version "3.0.20181129082112")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "schema-salad" version))
        (sha256
          (base32
            "1xg70v82q053vz1sg8sc99alnkrm2kk05w6698vgmngl1767sk97"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; CWL includes no tests.
    (inputs
      `(("python-cython" ,python-cython)
       ("python-setuptools" ,python-setuptools)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ("python-mistune" ,python-mistune)))
    (propagated-inputs
     `(("python-rdflib" ,python-rdflib)
       ("python-avro" ,python-avro)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-shellescape" ,python-shellescape)
       ))
    (home-page
      "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis
      "Schema Annotations for Linked Avro Data (SALAD)")
    (description
      "Schema Annotations for Linked Avro Data (SALAD)")
    (license license:asl2.0)))

(define-public python2-schema-salad
  (package-with-python2 python-schema-salad))
