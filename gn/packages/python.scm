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

(define-public python-flask
(package
  (name "python-flask")
  (version "0.10.1")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "Flask" version))
      (sha256
        (base32
          "0wrkavjdjndknhp8ya8j850jq7a1cli4g5a93mg8nh1xz2gq50sc"))))
  (build-system python-build-system)
  (inputs
   `(("python-setuptools" ,python-setuptools)
     ; ("python-itsdangerous" ,python-itsdangerous)
     ; ("python-jinja2" ,python-jinja2)
     ; ("python-werkzeug" ,python-werkzeug)
     ))
  (propagated-inputs
   `(
     ("python-itsdangerous" ,python-itsdangerous)
     ("python-jinja2" ,python-jinja2)
     ("python-werkzeug" ,python-werkzeug)
     ))
  
  (arguments
   `(#:tests? #f)) ; No tests
  (home-page "http://github.com/mitsuhiko/flask/")
  (synopsis
    "A microframework based on Werkzeug, Jinja2 and good intentions")
  (description
    "A microframework based on Werkzeug, Jinja2 and good intentions")
  (license license:bsd-3)))

(define-public python2-flask
  (package-with-python2 python-flask))

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
  (inputs
   `(
     ("mysql" ,mysql)
     ("python-nose" ,python-nose)
     ("zlib" ,zlib)
     ("openssl" ,openssl)
     ("libgcrypt" ,libgcrypt)
     ))
  (propagated-inputs
   `(("python-setuptools" ,python-setuptools)
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

(define-public python2-htmlgen-old
(package
  (name "python2-htmlgen-old")
  (version "2.2.2")
  (source (origin
           (method url-fetch)
           (uri (string-append
                 "http://ftp.nl.debian.org/debian/pool/main/h/htmlgen/htmlgen_"
version ".orig.tar.gz"))
           (sha256
            (base32
             "186kb434q6z84g31ysvzi4kcfcvl3pmm57k4jlng4ccgls94x6wb"))
           (patches (list
                     (search-patch "python2-htmlgen-Applied-Deb-patch.patch")
                     (search-patch "python2-htmlgen-Fix-test-for-random.patch")
            ))))
  (build-system gnu-build-system)
  (outputs
   '("out"))
  (native-inputs `(("tar" ,tar)
                   ("gzip" ,gzip)))
  (inputs
   `(("make" ,gnu-make)))
  (propagated-inputs
   `(("python2" ,python-2)))
  (arguments
   `(#:modules ((guix build utils))
     #:builder
     (let* ((source (assoc-ref %build-inputs "source"))
            (tar    (assoc-ref %build-inputs "tar"))
            (gzip   (assoc-ref %build-inputs "gzip"))
            (output (assoc-ref %outputs "out"))
            (srcdir (string-append output "/src"))
            (lib (string-append (assoc-ref %outputs "out") "/lib/htmlgen"))
            (python (string-append (assoc-ref %build-inputs "python2") "/bin/")))
       (begin
         (use-modules (guix build utils))
         (setenv "PATH" (string-append gzip "/bin")) ;; for tar
         ;; (setenv "PATH" (string-append python "/bin"))
         (system* (string-append tar "/bin/tar") "xzf"
                  source)
         (chdir ,(string-append name "-" version))
         (mkdir-p srcdir)
         (lambda _
           (system* "python" "-m" "compileall" "."))
         (lambda _
           (system* "make" "test"))
         (mkdir-p lib)
                                        ; (copy-file "." lib)
         (copy-file "HTMLgen.pyc" lib)
         ))))
  (home-page
    "https://packages.debian.org/unstable/python/python-htmlgen")
  (synopsis "Python2 HTMLgen (somewhat defunkt project)")
  (description #f)
  (license #f)))

(define-public python2-htmlgen
(package
  (name "python2-htmlgen")
  (version "2.2.2")
  (source (origin
           (method url-fetch)
           (uri (string-append
                 "http://ftp.nl.debian.org/debian/pool/main/h/htmlgen/htmlgen_"
version ".orig.tar.gz"))
           (sha256
            (base32
             "186kb434q6z84g31ysvzi4kcfcvl3pmm57k4jlng4ccgls94x6wb"))
           (patches (list
                     (search-patch "python2-htmlgen-Applied-Deb-patch.patch")
                     (search-patch "python2-htmlgen-Fix-test-for-random.patch")
            ))))
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
                         (install-file "__init__.pyc" lib)
              ))) ; install 
     ) ; phases
     #:tests? #f))
  (home-page
    "https://packages.debian.org/unstable/python/python-htmlgen")
  (synopsis "Python2 HTMLgen (somewhat defunkt project)")
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
             ;; http://sourceforge.net/projects/numpy/files/Old%20Numarray/1.5.2/numarray-1.5.2.tar.gz/download
             "mirror://sourceforge/numpy/numarray-" version ".tar.gz"
             ))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x1i4j7yni7k4p9kjxs1lgln1psdmyrz65wp2yr35yn292iw2vbg"))))
    (build-system python-build-system)
    ;; (native-inputs
    ;; `(("python-setuptools" ,python-setuptools)))
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://www.numpy.org/")
    (synopsis "Numerical library array processing of numbers, strings, records and objects")
    (description
     "Numarray is an array processing package designed to efficiently manipulate large multi-dimensional arrays. Numarray is modelled after Numeric and features c-code generated from python template scripts, the capacity to operate directly on arrays in files, and improved type promotions. Numarray provides support for manipulating arrays consisting of numbers, strings, records, or objects using the same basic infrastructure and syntax.  Numarray is now part of the
numpy package, though some legacy software still uses the older versions.")
    (license license:gpl2))) ; actualy PyRAF http://www.stsci.edu/resources/software_hardware/pyraf/LICENSE
