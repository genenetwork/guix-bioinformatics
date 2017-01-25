(define-module (gn packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
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

(define-public python-flask-sqlalchemy
  (package
   (name "python-flask-sqlalchemy")
   (version "2.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "Flask-SQLAlchemy" version))
     (sha256
      (base32
       "1i9ps5d5snih9xlqhrvmi3qfiygkmqzxh92n25kj4pf89kj4s965"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-flask" ,python-flask)
      ("python-sqlalchemy" ,python-sqlalchemy)
      ))
   (home-page
    "http://github.com/mitsuhiko/flask-sqlalchemy")
   (synopsis
    "Adds SQLAlchemy support to your Flask application")
   (description
    "Adds SQLAlchemy support to your Flask application")
   (license license:bsd-3)))

(define-public python2-flask-sqlalchemy
  (package-with-python2 python-flask-sqlalchemy))

(define-public python-xlsxwriter
(package
  (name "python-xlsxwriter")
  (version "0.8.4")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "XlsxWriter" version))
      (sha256
        (base32
          "0hv6bknnj9mlvvkdnlzycs0s97vrakmyh91ddb7ynjaqp8gl434z"))))
  (build-system python-build-system)
  (inputs
    `(("python-setuptools" ,python-setuptools)))
  (home-page
    "https://github.com/jmcnamara/XlsxWriter")
  (synopsis
    "A Python module for creating Excel XLSX files.")
  (description
    "A Python module for creating Excel XLSX files.")
  (license license:bsd-3)))

(define-public python2-xlsxwriter
  (package-with-python2 python-xlsxwriter))

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
    (inputs
      `(("python-setuptools" ,python-setuptools)))
    (propagated-inputs
     `(("python-rdflib-4.2" ,python-rdflib-4.2)
       ("python-pyparsing" ,python-pyparsing)
       ("python-html5lib" ,python-html5lib)
       ("python-nose" ,python-nose)
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
    (arguments `(#:tests? #f)) ;; No tests.
    (home-page "https://github.com/RDFLib/rdflib")
    (synopsis
      "Python RDF library")
    (description
      "RDFLib is a Python library for working with RDF, a simple yet
powerful language for representing information.")
    (license (license:non-copyleft "file://LICENSE"
                           "See LICENSE in the distribution."))))

(define-public python2-rdflib-4.2
  (let ((base (package-with-python2 python-rdflib-4.2)))
    (package
      (inherit base)
      (inputs
        (append (package-inputs base)
                `(("python-nose" ,python-nose))))
      (arguments
        `(#:python ,python-2
          #:tests? #f))))) ; 3 tests fail, also outside Guix

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

(define-public python-mysqlclient
(package
  (name "python-mysqlclient")
  (version "1.3.7")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "mysqlclient" version))
      (sha256
        (base32
          "06qzgwk7j66k8ggx51i6wxx0f0zsppp7w4bh6gjd0cr9rfs86jn7"))))
  (build-system python-build-system)
  (propagated-inputs
   `(
     ("mysql" ,mysql)
     ("python-nose" ,python-nose)
     ("zlib" ,zlib)
     ("openssl" ,openssl)
     ("libgcrypt" ,libgcrypt)
     ("python-setuptools" ,python-setuptools)
     ))
  (arguments
   `(#:tests? #f))   ; wants a running MySQL server
  (home-page
    "https://github.com/PyMySQL/mysqlclient-python")
  (synopsis "Python interface to MySQL")
  (description "Python interface to MySQL")
  (license license:gpl3)))

(define-public python2-mysqlclient
  (package-with-python2 python-mysqlclient))


(define-public python2-htmlgen-gn
(package
  (name "python2-htmlgen-gn")
  (version "2.2.2")
  (source (origin
           (method url-fetch)
           ;; http://files.genenetwork.org/software/contrib/htmlgen-2.2.2-gn.tar.gz
           (uri (string-append
                 "http://files.genenetwork.org/software/contrib/htmlgen-"
version "-gn.tar.gz"))
           (sha256
            (base32
             "1lwsk56rymhrma46cbyh3g64ksmq1vsih3qkrc2vh0lpba825y7r"))
           ;;(patches (list
           ;;          (search-patch "python2-htmlgen-Applied-Deb-patch.patch")
           ;;          (search-patch "python2-htmlgen-Fix-test-for-random.patch")
            ))
  (build-system python-build-system)
  (outputs '("out"))
  (native-inputs
   `(("make" ,gnu-make)
     ))
  (propagated-inputs
   `(("python2" ,python-2)))
  (arguments
   `(#:phases (modify-phases %standard-phases
     (replace 'build
              (lambda _
                (system* "python2" "-m" "compileall" ".")))
     (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (include (string-append out "/include"))
                              (lib2 (string-append out "/lib/htmlgen"))
                              (lib (string-append (assoc-ref %outputs "out") "/lib/python2.7/site-packages/htmlgen"))
                              (pkgconfig (string-append out "/lib/pkgconfig"))
                              (doc (string-append out "/share/doc")))
                         ;; Install libs and headers.
                         ;; (copy-file "HTMLgen.pyc" "HTMLgen2.pyc")
                         (install-file "HTMLgen.pyc" lib)
                         (install-file "HTMLgen2.pyc" lib)
                         (install-file "imgsize.pyc" lib)
                         (install-file "ImageH.pyc" lib)
                         (install-file "ImagePaletteH.pyc" lib)
                         (install-file "__init__.pyc" lib)
              ))) ; install
     ) ; phases
     #:tests? #f))
  (home-page
    "https://packages.debian.org/unstable/python/python-htmlgen")
  (synopsis "Genenetwork version of Python2 HTMLgen (defunkt
project)")
  (description #f)
  (license #f)))

(define-public python2-pil
  (package
    (name "python2-pil")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "http://effbot.org/downloads/Imaging-"
              version ".tar.gz"))
        (sha256
          (base32
            "141zidl3s9v4vfi3nsbg42iq1lc2a932gprqr1kij5hrnn53bmvx"))
       (modules '((guix build utils)))
       (snippet
        ;; Adapt to newer freetype. As the package is unmaintained upstream,
        ;; there is no use in creating a patch and reporting it.
        '(substitute* "_imagingft.c"
           (("freetype/")
            "freetype2/freetype/")))))
    (build-system python-build-system)
    (inputs
      `(("freetype" ,freetype)
        ("libjpeg" ,libjpeg)
        ("libtiff" ,libtiff)
        ("python2-setuptools" ,python2-setuptools)
        ("zlib" ,zlib)))
    (arguments
     ;; Only the fork python-pillow works with Python 3.
     `(#:python ,python-2
       #:tests? #f ; no check target
       #:phases
         (alist-cons-before
          'build 'configure
          ;; According to README and setup.py, manual configuration is
          ;; the preferred way of "searching" for inputs.
          ;; lcms is not found, TCL_ROOT refers to the unavailable tkinter.
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((jpeg (assoc-ref inputs "libjpeg"))
                  (zlib (assoc-ref inputs "zlib"))
                  (tiff (assoc-ref inputs "libtiff"))
                  (freetype (assoc-ref inputs "freetype")))
              (substitute* "setup.py"
                (("JPEG_ROOT = None")
                 (string-append "JPEG_ROOT = libinclude(\"" jpeg "\")"))
                (("ZLIB_ROOT = None")
                 (string-append "ZLIB_ROOT = libinclude(\"" zlib "\")"))
                (("TIFF_ROOT = None")
                 (string-append "TIFF_ROOT = libinclude(\"" tiff "\")"))
                (("FREETYPE_ROOT = None")
                 (string-append "FREETYPE_ROOT = libinclude(\""
                                freetype "\")")))))
          %standard-phases)))
    (home-page "http://www.pythonware.com/products/pil/")
    (synopsis "Python Imaging Library")
    (description "The Python Imaging Library (PIL) adds image processing
capabilities to the Python interpreter.")
    (license (license:x11-style
               "file://README"
               "See 'README' in the distribution."))))

(define-public python2-piddle-gn
  (package
    (name "python2-piddle")
    (version "1.0.15-gn")
    (source (origin
     (method url-fetch)
     (uri (string-append
           "http://files.genenetwork.org/software/contrib/piddle-"
version ".tgz"))
     (sha256
      (base32
       "05gjnn31v7p0kh58qixrpcizcxqf3b7zv4a5kk8nsmqwgxh0c6gq"))))

    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (propagated-inputs
     `(("python2-pil" ,python2-pil)))
    (arguments
     `(
       #:python ,python-2
       #:tests? #f   ; no 'setup.py test' really!
    ))
    (home-page #f)
    (synopsis "Canvas drawing library for python2 (old!)")
    (description #f)
    (license #f)))

(define-public python2-parallel
  (package
    (name "python2-parallel")
    (version "1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.parallelpython.com/downloads/pp/pp-" version ".zip"
             ))
       (sha256
        (base32
         "1bw3j0zn7bj56636vp1vx4m91p2mlp661gn2nfhpbph3prgxzv82"))))
    (native-inputs
     `(("unzip" ,unzip)))

    (build-system python-build-system)
    ;; (native-inputs
    ;; `(("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:python ,python-2
       #:tests? #f
       ))   ; no 'setup.py test' really!
    (home-page #f)
    (synopsis "Parallel python lib")
    (description #f)
    (license #f)))

(define-public python2-numarray
  (package
    (name "python2-numarray")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/numpy/numarray-" version ".tar.gz"
             ))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x1i4j7yni7k4p9kjxs1lgln1psdmyrz65wp2yr35yn292iw2vbg"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://www.numpy.org/")
    (synopsis "Numerical library array processing of numbers, strings, records and objects")
    (description
     "Numarray is an array processing package designed to efficiently manipulate large multi-dimensional arrays. Numarray is modelled after Numeric and features c-code generated from python template scripts, the capacity to operate directly on arrays in files, and improved type promotions. Numarray provides support for manipulating arrays consisting of numbers, strings, records, or objects using the same basic infrastructure and syntax.  Numarray is now part of the
numpy package, though some legacy software still uses the older versions.")
    (license license:gpl2))) ; actualy PyRAF http://www.stsci.edu/resources/software_hardware/pyraf/LICENSE

(define-public python-rst2ansi
  (package
    (name "python-rst2ansi")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/3c/19/b29bc04524e7d1dbde13272fbb67e45a8eb2"
             "4bb6d112cf10c46162b350d7/rst2ansi-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vzy6gd60l79ff750scl0sz48r1laalkl6md6dwzah4dcadgn5qv"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-docutils" ,python-docutils)))
    (home-page
     "https://github.com/Snaipe/python-rst-to-ansi")
    (synopsis
     "Python rst converter to ansi-decorated console output")
    (description
     "Python module dedicated to rendering RST (reStructuredText) documents to
 ansi-escaped strings suitable for display in a terminal")
    (license license:expat)))

(define-public python-mando
  (package
    (name "python-mando")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/2b/52/684d9ab8c2ccfb611275f2e44d3ebc76a6a6"
             "c56f4afacd2e91237fa07ec3/mando-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1bicmnxzxi9bxz9bfgv2rk7297f5rbwc9v2hg2rqfqr6h27zjgw5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-rst2ansi" ,python-rst2ansi)))
    (home-page "https://mando.readthedocs.org/")
    (synopsis
     "Wrapper around argparse, allowing creation of complete CLI applications")
    (description
     "This package is a wrapper around argparse, allowing you to write complete CLI
 applications in seconds while maintaining all the flexibility")
    (license license:expat)))

(define-public python-flake8-polyfill
  (package
    (name "python-flake8-polyfill")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/71/6e/dd7e0f0ddf146213d0cc0b963b3d4c643482"
             "3ebe3992c29b523182bbf785/flake8-polyfill-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "02gn2wxvh9vnf7m7dld7ca4l60mg5c370hv3swwppkngwaqmcw67"))))
    (build-system python-build-system)
    (inputs
     `(("python-flake8" ,python-flake8)))
    (home-page "https://gitlab.com/pycqa/flake8")
    (synopsis "Polyfill package for Flake8 plugins")
    (description
     "This package that provides some compatibility helpers for Flake8 plugins that
 intend to support Flake8 2.x and 3.x simultaneously")
    (license license:expat)))

(define-public python-radon
  (package
    (name "python-radon")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/91/48/5853fa60811d6cec1043bd60b057aafc2270"
             "6e19ab90b33d8df8155e6b8d/radon-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15xyzavfj1zwb5rn07fs2wfi6ccys9b5q0s8hmnpqz712mifl92g"))))
    (build-system python-build-system)
    (inputs
     `(("python-colorama" ,python-colorama)
       ("python-flake8-polyfill"
        ,python-flake8-polyfill)
       ("python-mando" ,python-mando)))
    (home-page "https://radon.readthedocs.org/")
    (synopsis "Code Metrics in Python")
    (description "Radon is a Python tool which computes various code metrics.  Supported
 metrics are:
 * raw metrics: SLOC, comment lines, blank lines, &c.
 * Cyclomatic Complexity (i.e.  McCabe’s Complexity)
 * Halstead metrics (all of them)
 * the Maintainability Index (a Visual Studio metric)")
    (license license:expat)))

(define-public python-xenon
  (package
    (name "python-xenon")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/0d/60/649ef1567dac76bf680d5d7498e37d9bf745"
             "f6a6214da7e5dba530c25481/xenon-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1c03nis488ls50cgcq7ghbj55nxsi6a9683lsvg6z6vaj4smc8g8"))))
    (build-system python-build-system)
    (inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-radon" ,python-radon)
       ("python-requests" ,python-requests)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page "https://xenon.readthedocs.org/")
    (synopsis
     "Monitor code metrics for Python on your CI server")
    (description
     "Xenon is a monitoring tool based on Radon.  It monitors code’s complexity.  Ideally,
 Xenon is run every time code is committed.  Through command line options, various
 thresholds can be set for the complexity of code.  It will fail (i.e.  it will exit with
 a non-zero exit code) when any of these requirements is not met")
    (license license:expat)))

(define-public python-ddt
  (package
    (name "python-ddt")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/83/96/21a2cef2962a07768854d411a97366292669"
             "3173887560895e962cf952c9/ddt-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1c00ikkxr7lha97c81k938bzhgd4pbwamkjn0h4nkhr3xk00zp6n"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)
       ("python-pyyaml" ,python-pyyaml)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/txels/ddt")
    (synopsis "Data-Driven/Decorated Tests")
    (description "DDT (Data-Driven Tests) allows you to multiply one test case by running
 it with different test data, and make it appear as multiple test cases")
    (license (license:non-copyleft
               "https://github.com/txels/ddt/blob/master/LICENSE.md"))))

(define-public python-auxlib
  (package
    (name "python-auxlib")
    (version "0.0.42")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/92/c7/304b651594ebc31fbe1aa201369ab7bd7e71"
             "8928543d8cc1063d46319c30/auxlib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0pczs3a8ck3z6qhl2fldhm2dl2czxl4yj9kkhx47qlpwhy0726xj"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build
                     'set-source-date-epoch
                     (lambda* (set-source-date-epoch #:rest _)
                       ;; guix sets environment variable SOURCE_DATE_EPOCH=1 in
                       ;; guix/build/gnu-build-system.scm file
                       ;; The build for auxlib, however, calls the make_wheelfile_inner
                       ;; function which reads from SOURCE_DATE_EPOCH, giving a date in
                       ;; 1970, which is earlier than 1980, causing the error
                       ;; 'ZIP does not support timestamps before 1980'
                       ;; This phase fixes the issue, borrowing from phase
                       ;; ensure-no-mtimes-pre-1980 in guix/build/python-build-system.scm
                       (setenv "SOURCE_DATE_EPOCH" "315619200")
                       #t)))))
    (inputs
     `(("python-enum34" ,python-enum34)
       ("python-tox" ,python-tox)
       ("python-flake8" ,python-flake8)
       ("python-radon" ,python-radon)
       ("python-xenon" ,python-xenon)
       ("python-wheel" ,python-wheel)
       ("python-ddt" ,python-ddt)
       ("python-testtools" ,python-testtools)
       ("python-pycrypto" ,python-pycrypto)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://github.com/kalefranz/auxlib")
    (synopsis
     "Auxiliary library to the python standard library")
    (description
     "Auxlib is an auxiliary library to the python standard library.  The aim is to
 provide core generic features for app development in python.  Auxlib fills in some
 python stdlib gaps much like pytoolz has for functional programming, pyrsistent has for
 data structures, or boltons has generally")
    (license license:isc)))

(define-public python-pycosat
  (package
    (name "python-pycosat")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/76/0f/16edae7bc75b79376f2c260b7a459829785f"
             "08e463ecf74a8ccdef62dd4a/pycosat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1kl3wh1f47rc712n4bmwplbx3fqz3x9i1b587jrbpmvdva4c8f6l"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/ContinuumIO/pycosat")
    (synopsis "Bindings to picosat (a SAT solver)")
    (description
     "This package provides efficient Python bindings to picosat on the C level, i.e.
 when importing pycosat, the picosat solver becomes part of the Python process itself")
    (license license:expat)))
