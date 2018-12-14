(define-module (gn packages edash)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (gnu packages web)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages time)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gn packages web)
  #:use-module (gn packages python))

(define-public python-s3transfer-0.1.13
  (package
  (name "python-s3transfer")
  (version "0.1.13")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "s3transfer" version))
      (sha256
        (base32
          "1harvyn1s8v54n1w5h7c0lg4bgjh68aylhg28s8n174q53h1ip4h"))))
  (build-system python-build-system)
  (arguments
   `(#:tests? #f))
  (home-page "https://github.com/boto/s3transfer")
  (synopsis "Amazon S3 Transfer Manager")
  (description "S3transfer is a Python library for managing Amazon S3
transfers.")
  (license license:asl2.0)))

(define-public python-botocore-1.10.48
  (package
    (name "python-botocore")
    (version "1.10.48")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "botocore" version))
       (sha256
        (base32
	 "1432drc7482nwrppwkk1i6ars3wz9w2g9rsxkz5nlxmyf9qm260j"))))
    (build-system python-build-system)
    (arguments
    ;; FIXME: Many tests are failing.
    '(#:tests? #f))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-docutils" ,python-docutils)
       ("python-jmespath" ,python-jmespath)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("behave" ,behave)
       ("python-tox" ,python-tox)
       ("python-wheel" ,python-wheel)))
    (home-page "https://github.com/boto/botocore")
    (synopsis "Low-level interface to AWS")
    (description "Botocore is a Python library that provides a low-level
interface to the Amazon Web Services (AWS) API.")
    (license license:asl2.0)))

(define-public python-boto3
  (package
  (name "python-boto3")
  (version "1.7.48")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "boto3" version))
      (sha256
        (base32
	 "1pnm5pj70kaa8pbq39i2y70h17cazk8nkjqh12b8xqks7ywjr9f5"))))
  (build-system python-build-system)
  (arguments
   `(#:tests? #f))
  (propagated-inputs
    `(("python-botocore" ,python-botocore-1.10.48)
      ("python-jmespath" ,python-jmespath)
      ("python-s3transfer" ,python-s3transfer-0.1.13)))
  (home-page "https://github.com/boto/boto3")
  (synopsis "The AWS SDK for Python")
  (description "The AWS SDK for Python")
  (license license:asl2.0)))

(define-public python-speaklater
  (package
  (name "python-speaklater")
  (version "1.3")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "speaklater" version))
      (sha256
        (base32
          "1ab5dbfzzgz6cnz4xlwx79gz83id4bhiw67k1cgqrlzfs0va7zjr"))))
  (build-system python-build-system)
  (home-page
    "http://github.com/mitsuhiko/speaklater")
  (synopsis
    "implements a lazy string for python useful for use with gettext")
  (description
    "implements a lazy string for python useful for use with gettext")
  (license license:bsd-3)))

(define-public python-flask-mail
  (package
   (name "python-flask-mail")
   (version "0.9.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "flask-mail" version))
     (sha256
      (base32
       "0hazjc351s3gfbhk975j8k65cg4gf31yq404yfy0gx0bjjdfpr92"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-mock" ,python-mock)
      ("python-nose" ,python-nose)
      ("python-flask" ,python-flask)
      ("python-blinker" ,python-blinker)
      ("python-speaklater" ,python-speaklater)))
   (home-page
    "https://github.com/rduplain/flask-mail")
   (synopsis "Flask extension for sending email")
   (description "Flask extension for sending email")
   (license license:bsd-3)))

(define-public python-python-version
  (package
   (name "python-python-version")
   (version "0.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "python_version" version))
     (sha256
      (base32
       "0rzy0jzh1w7rx7swgjb5qv7b7dhblbsyhs7l3ii19mpjqxbxw5jw"))))
   (build-system python-build-system)
   (home-page
    "https://gitlab.com/halfak/python_version")
   (synopsis
    "Provides a simple utility for checking the python version.")
   (description
    "Provides a simple utility for checking the python version.")
   (license license:expat)))

(define-public python-ujson
  (package
    (name "python-ujson")
    (version "1.35")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ujson" version))
       (sha256
        (base32
         "11jz5wi7mbgqcsz52iqhpyykiaasila4lq8cmc2d54bfa3jp6q7n"))))
    (build-system python-build-system)
    (home-page "http://www.esn.me")
    (synopsis
     "Ultra fast JSON encoder and decoder for Python")
    (description
     "Ultra fast JSON encoder and decoder for Python")
    (license license:bsd-3)))

(define-public python-tests
  (package
    (name "python-tests")
    (version "0.007")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tests" version ".zip"))
       (sha256
        (base32
         "09nfrysnqivqhs7znync8k87v16anvnb02j9ikmr3i6hdqxjcd31"))))
    (build-system python-build-system)
    (inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.propython.com/")
    (synopsis "UNKNOWN")
    (description "UNKNOWN")
    (license #f)))

(define-public python-clickclick
  (package
    (name "python-clickclick")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "clickclick" version))
       (sha256
        (base32
         "08vd1whzr1gz3cfb9iqs200rgj1x9srlwaa6sjmwz41rkjm0m2aa"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-click" ,python-click)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pytest" ,python-pytest)
       ("python-flake8" ,python-flake8)
       ("python-pytest-cov" ,python-pytest-cov)))
    (arguments
     `(#:tests? #f))
    (home-page
     "https://github.com/zalando/python-clickclick")
    (synopsis "Click utility functions")
    (description "Utility functions (Python 3 only) for the wonderful
Click library.  Click is a Python package for creating beautiful
command line interfaces in a composable way with as little code as
necessary.")
    (license license:asl2.0)))

(define-public python-testfixtures
  (package
    (name "python-testfixtures")
    (version "6.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testfixtures" version))
       (sha256
        (base32
         "1x16xkw483nb1ngv74s7lgaj514pb1ldklal7kb7iwqbxcgnrh2k"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/Simplistix/testfixtures")
    (synopsis
     "Collection of helpers and mock objects for unit tests and doc tests")
    (description
     "A collection of helpers and mock objects for unit tests and doc tests.")
    (license license:expat)))

(define-public python-async-timeout-3.0.1
  (package
    (name "python-async-timeout")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "async-timeout" version))
       (sha256
        (base32
         "0pscbyr840m7fyfc3r8zv9kgkwdcn9f78p7zsrczciwd09m82g0c"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/aio-libs/async_timeout/")
    (synopsis
     "Timeout context manager for asyncio programs")
    (description
     "Timeout context manager for asyncio programs")
    (license license:asl2.0)))

(define-public python-pytest-timeout
  (package
    (name "python-pytest-timeout")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-timeout" version))
       (sha256
        (base32
         "1cczcjhw4xx5sjkhxlhc5c1bkr7x6fcyx12wrnvwfckshdvblc2a"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (arguments `(#:tests? #f))
    (home-page
     "http://bitbucket.org/pytest-dev/pytest-timeout/")
    (synopsis
     "py.test plugin to abort hanging tests")
    (description
     "py.test plugin to abort hanging tests")
    (license license:expat)))

(define-public python-pytest-aiohttp
  (package
    (name "python-pytest-aiohttp")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-aiohttp" version))
       (sha256
        (base32
         "0kx4mbs9bflycd8x9af0idcjhdgnzri3nw1qb0vpfyb3751qaaf9"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-aiohttp" ,python-aiohttp)
       ("python-pytest" ,python-pytest)
       ("python-pytest-timeout" ,python-pytest-timeout)))
    (home-page
     "https://github.com/aio-libs/pytest-aiohttp/")
    (synopsis "pytest plugin for aiohttp support")
    (description "pytest plugin for aiohttp support")
    (license license:asl2.0)))

(define-public python-aiohttp-jinja2
  (package
    (name "python-aiohttp-jinja2")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiohttp-jinja2" version))
       (sha256
        (base32
         "031djiddn2yg50mkw60x1ql1qwqxnmghk46902nq48cnbxcvdydf"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-jinja2" ,python-jinja2)
       ("python-aiohttp" ,python-aiohttp)))
    (arguments `(#:tests? #f))
    (home-page
     "https://github.com/aio-libs/aiohttp_jinja2/")
    (synopsis
     "jinja2 template renderer for aiohttp.web (http server for asyncio)")
    (description
     "jinja2 template renderer for aiohttp.web (http server for asyncio)")
    (license license:asl2.0)))

(define-public python-openapi-spec-validator
  (package
    (name "python-openapi-spec-validator")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openapi-spec-validator" version))
       (sha256
        (base32
         "0khzsk1dqjfj2j8z8iqn4l47j133b0yavzrj268hxwy4qjp4ls0l"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-tox" ,python-tox)
       ("python-mock" ,python-mock)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-jsonschema" ,python-jsonschema)
       ("python-pytest-pep8" ,python-pytest-pep8)
       ("python-pytest-cache" ,python-pytest-cache)
       ("python-pytest-flakes" ,python-pytest-flakes)))
    (home-page
     "https://github.com/p1c2u/openapi-spec-validator")
    (synopsis "OpenAPI Spec validator")
    (description "OpenAPI Spec Validator is a Python library that
validates OpenAPI Specs against the OpenAPI 2.0 (aka Swagger) and
OpenAPI 3.0.0 specification.  The validator aims to check for full compliance
with the Specification.")
    (license license:asl2.0)))

(define-public python-swagger-ui-bundle
  (package
    (name "python-swagger-ui-bundle")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "swagger_ui_bundle" version))
       (sha256
        (base32
         "0kk6y8p1fhmnjnxfafdw6dm1xc5wj8101y5h7hcy3rb2bxc1i94q"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-tox" ,python-tox)
       ("python-jinja2" ,python-jinja2)
       ("python-flake8" ,python-flake8)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-pep8" ,python-pytest-pep8)
       ("python-pytest-cache" ,python-pytest-cache)
       ("python-pytest-flakes" ,python-pytest-flakes)))
    (home-page
     "https://github.com/dtkav/swagger_ui_bundle")
    (synopsis
     "swagger_ui_bundle - swagger-ui files in a pip package")
    (description
     "swagger_ui_bundle - swagger-ui files in a pip package")
    (license license:asl2.0)))

(define-public edash
  (let ((md5 "93e745e9c"))
    (package
    (name "edash")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "http://biogems.info/genenetwork2-2.0-a8fcff4.svg") ; any old file
       (file-name (string-append name "-" md5))
       (sha256
        (base32 "0rir1mcn3a8i1mbw3ppgnjl7wg71mapljik7n3v5i8j5ic95mqr5"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    ;;(inputs `(("sassc" ,sassc)))
    (propagated-inputs
     `(("python" ,python)
       ("gunicorn" ,gunicorn)
       ("python-wrapper" ,python-wrapper)
       ("python-elasticsearch" ,python-elasticsearch)
       ("python-flask" ,python-flask)
       ("python-jinja2" ,python-jinja2)
       ("python-markdown" ,python-markdown)
       ("python-misaka" ,python-misaka)
       ("python-pygit2" ,python-pygit2)
       ("web-bootstrap" ,web-bootstrap)
       ;;("sassc" ,sassc)
       ("python-boto3" ,python-boto3)
       ("python-pytest" ,python-pytest)
       ("python-flask-login" ,python-flask-login)
       ("python-passlib" ,python-passlib)
       ("python-pyjwt" ,python-pyjwt)
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-flask-sqlalchemy" ,python-flask-sqlalchemy)
       ("python-flask-mail" ,python-flask-mail)
       ("python-flask-wtf" ,python-flask-wtf)
       ("sqlite" ,sqlite)
       ("python-ipython" ,python-ipython)
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

    (home-page "https://gitlab.com/unless-emtec/live-service")
    (synopsis "EMTech's edash service")
    (description "EMTech's edash service package.")
    (license license:agpl3+))))
