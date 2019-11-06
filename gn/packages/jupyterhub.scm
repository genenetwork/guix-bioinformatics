(define-module (gn packages jupyterhub)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages time)
  )

(define-public jupyterhub
  (package
    (name "jupyterhub")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jupyterhub" version))
        (sha256
         (base32
          "0zx6gw9yhgki05j21p6x1x2sf5a2mg2c2mx0ii8rl6q4b98ilm1k"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; Tests require a webserver, postgresql database and npm.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (;; From .travis.yml
                (invoke "initdb" "main")
                (invoke "pg_ctl" "-D" "main" "start")
                (invoke "psql" "--list")
                ;; From ci/init-db.sh
                (invoke "psql" "-d" "postgres" "-c"
                        "CREATE DATABASE jupyterhub_upgrade_072;")
                (invoke "psql" "-d" "postgres" "-c"
                        "CREATE DATABASE jupyterhub_upgrade_081;")
                (invoke "psql" "-d" "postgres" "-c"
                        "CREATE DATABASE jupyterhub_upgrade_094;")
                (invoke "pytest" "-v" "--maxfail=2" "jupyterhub/tests"))
               #t))))))
    (propagated-inputs
     `(("python-alembic" ,python-alembic)
       ("python-async-generator" ,python-async-generator)
       ("python-certipy" ,python-certipy)
       ("python-dateutil" ,python-dateutil)
       ("python-entrypoints" ,python-entrypoints)
       ("python-jinja2" ,python-jinja2)
       ("python-oauthlib" ,python-oauthlib)
       ("python-pamela" ,python-pamela)
       ("python-prometheus-client" ,python-prometheus-client)
       ("python-requests" ,python-requests)
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-tornado" ,python-tornado)
       ("python-traitlets" ,python-traitlets)))
    (native-inputs
     `(
       ;("postgresql" ,postgresql)
       ;("python-psycopg2" ,python-psycopg2)

       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-jupyter-client" ,python-jupyter-client)
       ("python-notebook" ,python-notebook)
       ("python-pytest" ,python-pytest)
       ("python-pyzmq" ,python-pyzmq)
       ("python-requests-mock" ,python-requests-mock)))
    (home-page "https://jupyter.org")
    (synopsis "Multi-user server for Jupyter notebooks")
    (description
     "JupyterHub is the best way to serve Jupyter notebook for multiple users.
It can be used in a classes of students, a corporate data science group or
scientific research group. It is a multi-user Hub that spawns, manages, and
proxies multiple instances of the single-user Jupyter notebook server.")
    (license license:bsd-3)))

(define-public the-littlest-jupyterhub
  (let ((commit "770b95027951c1e191690b0f65d3e2e2d72ba05f") ; Oct 30, 2019
        (revision "1"))
    (package
      (name "the-littlest-jupyterhub")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/jupyterhub/the-littlest-jupyterhub.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0dnin2rwrqr8nzf7gj6sjqchvdi24kd0sgjh655c4zq1hdbpsmc6"))))
      (build-system python-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'loosen-input-requirements
             (lambda _
               (substitute* "setup.py"
                 (("==.*'") "'"))
               #t))
           (add-after 'unpack 'patch-some-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((python (assoc-ref inputs "python")))
                 ;; Delete some tests that we're not going to run.
                 (delete-file "tests/test_conda.py")
                 (delete-file "tests/test_user.py")
                 (delete-file "tests/test_utils.py")
                 ;; No need to make node available for the test suite.
                 (substitute* "tests/test_installer.py"
                   (("installer.ensure_node.*") "")
                   (("/usr/bin/node") (string-append python "/bin/python")))
                 ;; Don't download traefik.
                 (substitute* "tests/test_traefik.py"
                   (("traefik.ensure_traefik_binary")
                    "#traefik.ensure_traefik_binary")
                   (("assert traefik") "#assert traefik")
                   (("assert \\(traefik") "#assert (traefik"))
                 #t)))
           (replace 'check
             (lambda _
               (invoke "python3" "-m" "pytest" "-v" "tests/"))))))
      (propagated-inputs
       `(("python-ruamel.yaml" ,python-ruamel.yaml)
         ("python-jinja2" ,python-jinja2)
         ("python-pluggy" ,python-pluggy)
         ("python-passlib" ,python-passlib)
         ("python-backoff" ,python-backoff)
         ("python-requests" ,python-requests)
         ("python-jupyterhub-traefik-proxy" ,python-jupyterhub-traefic-proxy)))
      (native-inputs
       `(("python-pytest" ,python-pytest)
         ("python-pytoml" ,python-pytoml)))
      (home-page "https://the-littlest-jupyterhub.readthedocs.io/en/latest/")
      (synopsis "JupyterHub server for a single server")
      (description
       "The Littlest JupyterHub (TLJH) distribution helps you provide Jupyter
Notebooks to 1-50 users on a single server.  Administrators who do not consider
themselves 'system administrators' but would like to provide hosted Jupyter
Notebooks for their students / users are the primary audience.  All users are
provided with the same environment, and administrators can easily install
libraries into this environment without any specialized knowledge.")
      (license license:bsd-3))))

(define-public python-backoff
  (package
    (name "python-backoff")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "backoff" version))
        (sha256
         (base32
          "0g2s4p34ml8dnldbj5qkg7ni5bwsq492nhry2inc2pn66qyx47iz"))))
    (build-system python-build-system)
    (home-page "https://github.com/litl/backoff")
    (synopsis "Function decoration for backoff and retry")
    (description
     "Function decoration for backoff and retry")
    (license license:expat)))

(define-public python-jupyterhub-traefic-proxy
  (package
    (name "python-jupyterhub-traefic-proxy")
    (version "0.1.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jupyterhub/traefik-proxy.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "021lqll3b84qmyw417wj15wpvs8ckvabx8ygm4fjy9bdllaxmmag"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("jupyterhub" ,jupyterhub)
       ("python-etcd3" ,python-etcd3)
       ("python-aiohttp" ,python-aiohttp)
       ("python-passlib" ,python-passlib)
       ("python-consul" ,python-consul)
       ("python-toml" ,python-toml)
       ("python-escapism" ,python-escapism)))
    (native-inputs
     `(("python-jupyter-client" ,python-jupyter-client)
       ("python-notebook" ,python-notebook)
       ("python-numpy" ,python-numpy)
       ("python-pyzmq" ,python-pyzmq)
       ("python-websockets" ,python-websockets)))
    (home-page "https://jupyterhub-traefik-proxy.readthedocs.io/")
    (synopsis "JupyterHub proxy implementation with traefik")
    (description "An implementation of the JupyterHub proxy api with traefik: an
extremely lightweight, portable reverse proxy implementation, that supports load
balancing and can configure itself automatically and dynamically.")
    (license license:bsd-3)))

(define-public python-etcd3
  (package
    (name "python-etcd3")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "etcd3" version))
        (sha256
         (base32
          "11qf9v69h5gx212p9hv3m8z290gagn834x89y0gn7iijy2wj9995"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests require running etcd.
    (propagated-inputs
     `(("python-grpcio" ,python-grpcio)
       ("python-protobuf" ,python-protobuf)
       ("python-tenacity" ,python-tenacity)))
    (home-page "https://github.com/kragniz/python-etcd3")
    (synopsis "Python client for the etcd3 API")
    (description "Python client for the etcd3 API")
    (license license:asl2.0)))

(define-public python-escapism
  (package
    (name "python-escapism")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "escapism" version))
        (sha256
         (base32
          "11lv1kqlk2brx1i2yczajga4c657z184f6fsnf2mnpx90kxc272z"))))
    (build-system python-build-system)
    (home-page "https://github.com/minrk/escapism")
    (synopsis "Simple, generic API for escaping strings")
    (description
     "Simple, generic API for escaping strings.")
    (license license:expat)))

(define-public python-tenacity
  (package
    (name "python-tenacity")
    (version "6.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tenacity" version))
        (sha256
         (base32
          "0w9miqmmi63yqp9lw7p6nf6gh5pa57vg60v6f94f11qqpg19gwvj"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest"))))))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-tornado" ,python-tornado)))
    (home-page "https://github.com/jd/tenacity")
    (synopsis "Retry code until it succeeeds")
    (description "Tenacity is a general-purpose retrying library to simplify the
task of adding retry behavior to just about anything.")
    (license license:asl2.0)))

(define-public python-certipy
  (package
    (name "python-certipy")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "certipy" version))
        (sha256
         (base32
          "0n980gqpzh0fm58h3i4mi2i10wgj606lscm1r5sk60vbf6vh8mv9"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyopenssl" ,python-pyopenssl)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/LLNL/certipy")
    (synopsis "Utility to create and sign CAs and certificates")
    (description
     "Utility to create and sign CAs and certificates")
    (license license:bsd-3)))

(define-public python-pamela
  (package
    (name "python-pamela")
    (version "1.0.0")
    (source
      (origin
        ;; Tests not distributed in pypi release.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/minrk/pamela.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0cg3w6np1fbjpvzhv54xg567hpf38szwp2d4gvzb9r736nxbv0vr"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; Tests aren't designed to be run inside a container.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hardcode-pam.so
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pam (assoc-ref inputs "linux-pam")))
               (substitute* "pamela.py"
               ;  (("\"pam\"") (string-append "\"" pam "/lib/libpam.so\"")))
                 (("find_library\\(\"pam\")") (string-append "'" pam "/lib/libpam.so'")))
               ;  (("LIBPAM =.*") (string-append "LIBPAM = \"" pam "/lib\"\n")))
               #t)))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (if (file-exists? "test_pamela.py")
                 (invoke "py.test" "--assert=plain" "test_pamela.py")
                 (invoke "python" "-m" "pamela" "-a" "`whoami`"))
               #t))))))
    (inputs
     `(("linux-pam" ,linux-pam)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/minrk/pamela")
    (synopsis "PAM interface using ctypes")
    (description "PAM interface using ctypes")
    (license license:expat)))

(define-public python-pifpaf
  (package
    (name "python-pifpaf")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pifpaf" version))
        (sha256
         (base32
          "09xixb4ddjp15spbm2788yq6rjr5fzy1p8dzcqyzp6pbkv7vwxvg"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "testr" "--slowest"
                     "--testr-args=until-failure"))))))
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-daiquiri" ,python-daiquiri)
       ("python-fixtures" ,python-fixtures)
       ("python-jinja2" ,python-jinja2)
       ("python-pbr" ,python-pbr)
       ("python-psutil" ,python-psutil)
       ("python-six" ,python-six)
       ("python-xattr" ,python-xattr)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-os-testr" ,python-os-testr)
       ("python-requests" ,python-requests)
       ("python-testrepository" ,python-testrepository)
       ("python-testtools" ,python-testtools)))
    (home-page "https://github.com/jd/pifpaf")
    (synopsis
      "Suite of tools and fixtures to manage daemons for testing")
    (description
      "Suite of tools and fixtures to manage daemons for testing")
    (license license:asl2.0)))

(define-public python-daiquiri
  (package
    (name "python-daiquiri")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "daiquiri" version))
        (sha256
         (base32
          "1hclvhfm3ix40g3m7g14w0nz2qawb7gp3fjba7d6xzxhh9mj6fn1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-json-logger" ,python-json-logger)
       ("python-pbr" ,python-pbr)
       ("python-pytest" ,python-pytest)
       ("python-six" ,python-six)))
    (home-page "https://github.com/jd/daiquiri")
    (synopsis "Library to configure Python logging easily")
    (description
      "Library to configure Python logging easily")
    (license license:asl2.0)))

(define-public python-xattr
  (package
    (name "python-xattr")
    (version "0.9.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xattr" version))
        (sha256
         (base32
          "0n3llkk488bjywzifgyhyxcfdz22851k3s2h8g69kzmlxa7b5cbw"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cffi" ,python-cffi)))
    (home-page "https://github.com/xattr/xattr")
    (synopsis "Python wrapper for extended filesystem attributes")
    (description
     "Python wrapper for extended filesystem attributes")
    (license license:expat)))

(define-public python-json-logger
  (package
    (name "python-json-logger")
    (version "0.1.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-json-logger" version))
        (sha256
         (base32
          "10g2ya6nsvn5vxzvq2wb8q4d43i3d7756i5rxyjna6d0y9i138xp"))))
    (build-system python-build-system)
    (home-page "https://github.com/madzak/python-json-logger")
    (synopsis "Python library adding a json log formatter")
    (description
     "A python library adding a json log formatter")
    (license license:bsd-3)))

(define-public python-websockets
  (package
    (name "python-websockets")
    (version "8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "websockets" version))
        (sha256
         (base32
          "03s3ml6sbki24aajllf8aily0xzrn929zxi84p50zkkbikdd4raw"))))
    (build-system python-build-system)
    (home-page "https://github.com/aaugustin/websockets")
    (synopsis "Implementation of the WebSocket Protocol")
    (description
     "An implementation of the WebSocket Protocol (RFC 6455 & 7692).")
    (license license:bsd-3)))

(define-public python-pytoml
  (package
    (name "python-pytoml")
    (version "0.1.21")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytoml" version))
        (sha256
         (base32
          "1rv1byiw82k7mj6aprcrqi2vdabs801y97xhfnrz7kxds34ggv4f"))))
    (build-system python-build-system)
    (home-page "https://github.com/avakar/pytoml")
    (synopsis "A parser for TOML-0.4.0")
    (description "A parser for TOML-0.4.0")
    (license license:expat)))
