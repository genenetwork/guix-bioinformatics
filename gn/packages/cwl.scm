(define-module (gn packages cwl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gn packages python)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public cwltool
  (package
    (name "cwltool")
    (version "3.0.20200710214758")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cwltool" version))
        (sha256
         (base32
          "1qbqkhinkhzg98jf24d5gnafsw23kng76rbi2hfvzl18bdsp1zz5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-version-restrictions
           (lambda _
             (substitute* "setup.py"
               (("== 1.5.1") ">=1.5.1"))
             #t))
         (add-after 'unpack 'modify-tests
           (lambda _
             ;; Tries to connect to the internet.
             (delete-file "tests/test_udocker.py")
             (substitute* "tests/test_http_input.py"
               (("https://raw.githubusercontent.com/common-workflow-language/cwltool/main")
                "."))
             (substitute* "tests/test_load_tool.py"
               (("def test_load_graph_fragment_from_packed")
                "@pytest.mark.skip(reason=\"Disabled by Guix\")\ndef test_load_graph_fragment_from_packed"))
             (substitute* "tests/test_examples.py"
               (("def test_env_filtering")
                "@pytest.mark.skip(reason=\"Disabled by Guix\")\ndef test_env_filtering"))
             ;; Tries to use cwl-runners.
             (substitute* "tests/test_examples.py"
               (("def test_v1_0_arg_empty_prefix_separate_false")
                "@pytest.mark.skip(reason=\"Disabled by Guix\")\ndef test_v1_0_arg_empty_prefix_separate_false"))
             #t)))))
    (propagated-inputs
     `(("python-bagit" ,python-bagit)
       ("python-coloredlogs" ,python-coloredlogs)
       ("python-mypy-extensions" ,python-mypy-extensions)
       ("python-prov" ,python-prov)
       ("python-psutil" ,python-psutil)
       ("python-rdflib" ,python-rdflib)
       ("python-requests" ,python-requests)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-schema-salad" ,python-schema-salad)
       ("python-setuptools" ,python-setuptools)
       ("python-shellescape" ,python-shellescape)
       ("python-typing-extensions" ,python-typing-extensions)
       ;; Not listed as needed but seems to be necessary:
       ("node" ,node)
       ("python-cachecontrol" ,python-cachecontrol-0.11)
       ("python-dateutil" ,python-dateutil)
       ("python-lxml" ,python-lxml)
       ("python-networkx" ,python-networkx)))
    (native-inputs
     `(("python-arcp" ,python-arcp)
       ("python-humanfriendly" ,python-humanfriendly)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)))
    (home-page
     "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis "Common workflow language reference implementation")
    (description
     "Common workflow language reference implementation.")
    (license license:asl2.0)))

(define-public cwl-runner
  (package
    (name "cwl-runner")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cwl_runner" version))
        (sha256
         (base32
          "0011am2xqwchysdznayrmwhg4bfjl4wlq6m4k20z1m7gccyzjgw0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("cwltool" ,cwltool)))
    (home-page
     "https://github.com/common-workflow-language/common-workflow-language")
    (synopsis "Common workflow language reference implementation")
    (description
     "Common workflow language reference implementation.")
    (license license:asl2.0)))
