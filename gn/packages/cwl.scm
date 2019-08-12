;; Experimental packaging for the Common Workflow Language (started by Bruno)

(define-module (gn packages cwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gn packages python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  ; #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  ; #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public cwltool ; guix: needs work
  (let ((commit "15539fba76993f951af9eba913bea6d677c74005"))
  (package
    (name "cwltool")
    (version "1.0.20181012180214")
    (source
      (origin
        ; (method url-fetch)
        ; (uri (string-append
        ;        "https://pypi.python.org/packages/source/c/cwltool/cwltool-"
        ;        version
        ;       ".tar.gz"))
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/genenetwork/cwltool.git") ;; my repo for Python 3.7
               (commit commit)))
         (file-name (git-file-name name (string-append version "-" (string-take commit 7))))
        (sha256
          (base32
            "1qwfa82car7477sy0cb5bj4964w7zq7dcw2bdcls6c2i9qdp0586"))))
    (build-system python-build-system)
    (propagated-inputs ; a lot of these are used for testing
     `(("git" ,git)
       ("node" ,node)
       ("python-bagit" ,python-bagit)
       ("python-arcp" ,python-arcp)
       ("python-setuptools" ,python-setuptools)
       ("python-dateutil" ,python-dateutil)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-prov" ,python-prov)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-rdflib" ,python-rdflib)
       ("python-pyparsing" ,python-pyparsing)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-mock" ,python-mock)
       ("python-subprocess32" ,python-subprocess32)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-cachecontrol" ,python-cachecontrol)
       ("python-lxml" ,python-lxml)
       ("python-mypy-extensions" ,python-mypy-extensions)
       ("python-mistune" ,python-mistune)
       ("python-networkx" ,python-networkx)
       ("python-schema-salad" ,python-schema-salad)
       ("python-html5lib" ,python-html5lib)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ("python-scandir" ,python-scandir)
       ("python-psutil" ,python-psutil)
       ))
    ; (arguments `(#:tests? #f)) ;; CWL includes no tests.
    (arguments
     `(;#:phases
       ; (modify-phases %standard-phases
       ;   (replace 'check
       ;     (lambda* (#:key inputs outputs #:allow-other-keys)
       ;       (invoke "python" "-m" "pytest")
       ;       )))
       #:tests? #f))   ; Disable for now

    (home-page
      "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis
      "Common workflow language reference implementation")
    (description
      "Common workflow language reference implementation")
    (license license:asl2.0))))

(define-public python-cachecontrol
  (package
    (name "python-cachecontrol")
    (version "0.11.7")
    (source
     (origin
       (method url-fetch)
       ;; Pypi does not have tests.
       (uri (string-append
             "https://github.com/ionrock/cachecontrol/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yfhwihx1b1xjsx0r19va2m0r2s91im03x4d7pwzp87368f2lkkp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; Recent version breaks on cherrypy
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-redis" ,python-redis)
       ("python-webtest" ,python-webtest)
       ("python-mock" ,python-mock)))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-lockfile" ,python-lockfile)))
    (home-page "https://github.com/ionrock/cachecontrol")
    (synopsis "The httplib2 caching algorithms for use with requests")
    (description "CacheControl is a port of the caching algorithms in
@code{httplib2} for use with @code{requests} session objects.")
    (license license:asl2.0)))


(define-public python-schema-salad
  (let ((commit "eb85c3d49b99b7643e8a12248e2dc05504910c1e"))
  (package
    (name "python-schema-salad")
    (version "3.0.20181129082112")
    (source
      (origin
        ; (method url-fetch)
        ; (uri (pypi-uri "schema-salad" version))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/genenetwork/schema_salad.git") ;; my repo for Python3.7
             (commit commit)))
       (file-name (git-file-name name (string-append version "-" (string-take commit 7))))
       (sha256
        (base32
         "174f224zzjr0nbjlq3ypciyfijnibasysrgjswvx8yhan2dizlhr"))))
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
    (license license:asl2.0))))

; (define-public python2-schema-salad
;  (package-with-python2 python-schema-salad))
