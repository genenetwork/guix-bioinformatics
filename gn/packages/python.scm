(define-module (gn packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-pyvcf
  (package
   (name "python-pyvcf")
   (version "0.6.8")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pyvcf" version))
     (sha256
      (base32
       "1ngryr12d3izmhmwplc46xhyj9i7yhrpm90xnsd2578p7m8p5n79"))))
   (build-system python-build-system)
   (arguments
     `(#:tests? #f))
   (home-page
    "https://github.com/jamescasbon/PyVCF")
   (synopsis
    "Variant Call Format (VCF) parser for Python")
   (description
    "Variant Call Format (VCF) parser for Python")
   (license #f)))

(define-public python2-rpy2
  (package
    (name "python2-rpy2")
    (version "2.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rpy2" version))
       (sha256
        (base32
         "0nhan2qvrw7b7gg5zddwa22kybdv3x1g26vkd7q8lvnkgzrs4dga"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ; apparently incompatible with Python 3
       #:tests? #f))
    (propagated-inputs
     `(("python2-six" ,python2-six)
       ("python2-singledispatch" ,python2-singledispatch)
     ))
    (inputs
     `(("readline" ,readline)
       ("icu4c" ,icu4c)
       ("pcre" ,pcre)
       ("r-minimal" ,r-minimal)
       ("r-survival" ,r-survival)))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "http://rpy.sourceforge.net/")
    (synopsis "Python interface to the R language")
    (description "rpy2 is a redesign and rewrite of rpy.  It is providing a
low-level interface to R from Python, a proposed high-level interface,
including wrappers to graphical libraries, as well as R-like structures and
functions.")
    (license license:gpl3+)))


(define-public python-plotly ; guix candidate
   ; python-plotly, python-requests, python-pytz
(package
  (name "python-plotly")
  (version "2.1.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "plotly" version))
      (sha256
        (base32
          "1r4y9l5z62sqyd2r205vchlvlc1f7dcgjyagjkxiwywh27f9js7z"))))
  (build-system python-build-system)
  (arguments `(#:tests? #f)) ;; No tests.
  (home-page "https://plot.ly/python/")
  (synopsis
    "Python plotting library for collaborative, interactive, publication-quality graphs.")
  (description
    "Python plotting library for collaborative, interactive, publication-quality graphs.")
  (license license:expat))
)

(define-public python-plotly-3.2.1
  (package
    (inherit python-plotly)
    (name "python-plotly")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "plotly" version))
        (sha256
         (base32
          "1ay1plgsckfi7fddl99kvbcx5nifh48ahvszkqvb4d7r008m8sxk"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-decorator" ,python-decorator)
       ("python-nbformat" ,python-nbformat)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-retrying" ,python-retrying)
       ("python-six" ,python-six)))))

(define-public python2-plotly-3.2.1
  (package-with-python2 python-plotly-3.2.1))

(define-public python-retrying
  (package
    (name "python-retrying")
    (version "1.3.3")
    (source
      (origin
        ;; pypi release doesn't have test library
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rholder/retrying")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1kqipkbdaw5s1xg0gi29awm03vp1x8dz24pjidgxagvkvrjpzhi7"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/rholder/retrying")
    (synopsis "General-purpose retrying library")
    (description "Retrying is a general-purpose retrying library, written in
Python, to simplify the task of adding retry behavior to just about anything.
The simplest use case is retrying a flaky function whenever an Exception occurs
until a value is returned.")
    (license license:asl2.0)))

(define-public python-bagit; guix candidate
  (package
    (name "python-bagit")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "bagit" version))
        (sha256
         (base32
          "1m6y04qmig0b5hzb35lnaw3d2yfydb7alyr1579yblvgs3da6j7j"))))
    (build-system python-build-system)
    (inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)))
    (arguments `(#:tests? #f)) ;; No tests.
    (home-page "https://pypi.python.org/pypi/bagit")
    (synopsis "Create and validate BagIt packages")
    (description
      "Create and validate BagIt packages")
    (license license:gpl2)))

(define-public python-prov ; guix candidate
(package
  (name "python-prov")
  (version "1.5.3")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "prov" version))
      (sha256
        (base32
          "1a9h406laclxalmdny37m0yyw7y17n359akclbahimdggq853jd0"))))
  (build-system python-build-system)
  (inputs
       `(("python-rdflib" ,python-rdflib)
       ("python-lxml" ,python-lxml)
       ("python-networkx" ,python-networkx)
       ("python-dateutil" ,python-dateutil)
       ("python-pydot" ,python-pydot)
       ("graphviz" ,graphviz) ; for testing
       ))
  (home-page "https://github.com/trungdong/prov")
  (synopsis
    "A library for W3C Provenance Data Model supporting PROV-JSON, PROV-XML and PROV-O (RDF)")
  (description
    "A library for W3C Provenance Data Model supporting PROV-JSON, PROV-XML and PROV-O (RDF)")
  (license license:expat)))

(define-public python-subprocess32 ; guix candidate
  (package
    (name "python-subprocess32")
    (version "3.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "subprocess32" version))
        (sha256
         (base32
          "1hr5fan8i719hmlmz73hf8rhq74014w07d8ryg7krvvf6692kj3b"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;; No tests.
    (home-page "https://pypi.python.org/pypi/subprocess32")
    (synopsis
      "Python subprocess32.")
    (description
      "Python subprocess32.")
    (license license:gpl2))
)

(define-public python-inotify ; guix candidate
(package
  (name "python-inotify")
  (version "0.2.9")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "inotify" version))
      (sha256
        (base32
          "043sbm9q8ca4fhn19knwpsxgzfgm5ik75m0bl7dp9hjy6p3v3zzn"))))
  (build-system python-build-system)
  (propagated-inputs
      `(("inotify-tools" ,inotify-tools)))
  (arguments `(#:tests? #f)) ;; No tests.
  (home-page "https://pypi.python.org/pypi/inotify")
  (synopsis
    "Python inotify.")
  (description
    "Python inotify.")
  (license license:gpl2))
)

(define-public python2-inotify
  (package-with-python2 python-inotify))

(define-public python2-flask-sqlalchemy
  (package-with-python2 python-flask-sqlalchemy))

(define-public python-xlsxwriter ; guix ready
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

(define-public python-rserve
  (package
   (name "python-rserve")
   (version "0.91")
   (source
    (origin
     (method url-fetch)
                                        ; https://pypi.python.org/packages/27/e8/b23e0e3d38dadc721947bc9f4b6f1b3e5e1e6c26ac67d8ad88376c5555a0/pyRserve-0.9.1.tar.gz#md5=6da1978f908dd7bdc2d22ee5c29238c0
     (uri (string-append
           "https://pypi.python.org/packages/27/e8/b23e0e3d38dadc721947bc9f4b6f1b3e5e1e6c26ac67d8ad88376c5555a0/pyRserve-0.9.1.tar.gz"))
     (sha256
      (base32
       "162dg7d0ni035b75qskrjdzd1yyxwnvybcv115aiapcvyfw2vbsm"))))
   (build-system python-build-system)
   (propagated-inputs
    `(
      ("python-numpy" ,python-numpy)
      ))
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-pytest" ,python-pytest)
      ))
   (home-page "https://pypi.python.org/pypi/pyRserve")
   (synopsis
    "RServe.")
   (description
    ".")
   (license license:expat)))

(define-public python2-rserve
  (package-with-python2 python-rserve))

(define-public python-setuptools-old
  (package
    (name "python-setuptools-old")
    (version "40.6.0")
    (source
     (origin
      (method url-fetch)
      (uri "https://files.pythonhosted.org/packages/37/1b/b25507861991beeade31473868463dad0e58b1978c209de27384ae541b0b/setuptools-40.6.3.zip"
             )
      (sha256
       (base32
        "1y085dnk574sxw9aymdng9gijvrsbw86hsv9hqnhv7y4d6nlsirv"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Remove included binaries which are used to build self-extracting
          ;; installers for Windows.
          ;; TODO: Find some way to build them ourself so we can include them.
          (for-each delete-file (find-files "setuptools" "^(cli|gui).*\\.exe$"))
          #t))))
    (build-system python-build-system)
    ;; FIXME: Tests require pytest, which itself relies on setuptools.
    ;; One could bootstrap with an internal untested setuptools.
    (arguments
     `(#:tests? #f))
    (home-page "https://pypi.python.org/pypi/setuptools")
    (synopsis
     "Library designed to facilitate packaging Python projects")
    (description
     "Setuptools is a fully-featured, stable library designed to facilitate
packaging Python projects, where packaging includes:
Python package and module definitions,
distribution package metadata,
test hooks,
project installation,
platform-specific details,
Python 3 support.")
    ;; TODO: setuptools now bundles the following libraries:
    ;; packaging, pyparsing, six and appdirs. How to unbundle?
    (license (list license:psfl        ; setuptools itself
                   license:expat       ; six, appdirs, pyparsing
                   license:asl2.0      ; packaging is dual ASL2/BSD-2
                   license:bsd-2))))


(define-public python-avro ; guix ready - used by CWL
(package
  (name "python-avro")
  (version "1.8.2")
  (source
    (origin
      (method url-fetch)
        (uri (pypi-uri "avro" version))
      (sha256
        (base32
          "0nabn1hzj1880qsp7fkg7923c0xdqk4i35s15asmy2xp604f97lg"))))
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

(define-public python-shellescape ; guix ready
  (package
    (name "python-shellescape")
    (version "3.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "shellescape" version))
        (sha256
         (base32
          "0n5ky1b2vw2y0d4xl3qybyp2rk0gq5frjs8nr8ak6mgj2fyb4676"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page "https://github.com/chrissimpkins/shellescape")
    (synopsis
      "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
    (description
      "Shell escape a string to safely use it as a token in a shell command (backport of Python shlex.quote for Python versions 2.x & < 3.3)")
    (license license:expat)))

(define-public python2-shellescape
  (package-with-python2 python-shellescape))


; env IPFS_PATH=/export/ipfs/ ipfs add -r htmlgen/
; added QmUD9LMJTE8q5wYkUyAwLdz2QCGXWt457iFnyYQAGfsN3j htmlgen/htmlgen-2.2.2-gn.tar.gz
; added QmZLWsPHLFTU3hWAfdTwj3RXFrS8Ma7KEixne1suWuYqeG htmlgen
; penguin2:~/tmp$ env IPFS_PATH=/export/ipfs/ ipfs pin add -r QmZLWsPHLFTU3hWAfdTwj3RXFrS8Ma7KEixne1suWuYqeG
; pinned QmZLWsPHLFTU3hWAfdTwj3RXFrS8Ma7KEixne1suWuYqeG recursively

(define-public python2-htmlgen-gn ; guix obsolete
  (package
    (name "python2-htmlgen-gn")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              ;; http://files.genenetwork.org/software/contrib/htmlgen-2.2.2-gn.tar.gz
              (uri (string-append
                     "http://ipfs.genenetwork.org/ipfs/QmZLWsPHLFTU3hWAfdTwj3RXFrS8Ma7KEixne1suWuYqeG/htmlgen-" version "-gn.tar.gz"))
              (sha256
               (base32
                "1lwsk56rymhrma46cbyh3g64ksmq1vsih3qkrc2vh0lpba825y7r"))
              ;;(patches (list
              ;;          (search-patch "python2-htmlgen-Applied-Deb-patch.patch")
              ;;          (search-patch "python2-htmlgen-Fix-test-for-random.patch")
              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'modernize-imports
           (lambda _
             (substitute* "HTMLgen.py"
               (("whrandom") "random"))
             (substitute* "HTMLcalendar.py"
               (("import regex") "import re as regex"))
             (substitute* "HTMLutil.py"
               (("import string, regex") "import re as regex\nimport string"))
             (substitute* "HTMLtest.py"
               (("import string, regex, regsub") "import re as regex\nimport string")
               (("regsub.split") "re.split"))
             #t))
         (replace 'build
           (lambda _
             (invoke "python" "-m" "compileall" ".")))
         (replace 'check
           (lambda* (#:key (tests? '()) #:allow-other-keys)
             (if tests?
               (invoke "python" "HTMLtest.py")
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/python2.7/site-packages/htmlgen/")))
               ;; Install libs and headers.
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "." "\\.py[c]?$"))
               #t))))))
    (home-page "https://packages.debian.org/unstable/python/python-htmlgen")
    (synopsis "Genenetwork version of Python2 HTMLgen (defunkt project)")
    (description #f)
    (license #f)))

(define-public python2-htmlgen-2.2
  (package
    (name "python2-htmlgen-2.2")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/h/htmlgen/htmlgen_" version ".orig.tar.gz"))
              (sha256
               (base32
                 "186kb434q6z84g31ysvzi4kcfcvl3pmm57k4jlng4ccgls94x6wb"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'modernize-imports
           (lambda _
             (substitute* "HTMLgen.py"
               (("whrandom") "random"))
             (substitute* "HTMLcalendar.py"
               (("import regex") "import re as regex"))
             (substitute* "HTMLutil.py"
               (("import string, regex") "import re as regex\nimport string"))
             (substitute* "HTMLtest.py"
               (("import string, regex, regsub") "import re as regex\nimport string")
               (("regsub.split") "re.split"))
             #t))
         (replace 'build
           (lambda _
             (invoke "python" "-m" "compileall" ".")))
         (replace 'check
           (lambda _
             (invoke "python" "HTMLtest.py")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/python2.7/site-packages/htmlgen/")))
               ;; Install libs and headers.
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "." "\\.py[c]?$"))
               #t))))))
    (home-page "https://packages.debian.org/unstable/python/python-htmlgen")
    (synopsis "Genenetwork version of Python2 HTMLgen (defunkt project)")
    (description #f)
    (license #f)))

; penguin2:~/tmp$ env IPFS_PATH=/export/ipfs/ ipfs add -r Imaging/
; added QmV8Rew1re8gBTLsaqMU4bd7euFUPEpjiD572mtoz6KhPn Imaging/Imaging-1.1.6-gn.tar.gz
; added QmdkzQpVMLZVtywpYesynt9c7H8w7hHZRYKq8woN7stfpD Imaging
; env IPFS_PATH=/export/ipfs/ ipfs pin add -r QmdkzQpVMLZVtywpYesynt9c7H8w7hHZRYKq8woN7stfpD
; pinned QmdkzQpVMLZVtywpYesynt9c7H8w7hHZRYKq8woN7stfpD recursively


(define-public python2-pil1-gn ; guix obsolete
  (package
    (name "python2-pil1") ; works with GN2
    (version "1.1.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   ; "http://files.genenetwork.org/software/contrib/Imaging-"
                   "http://effbot.org/downloads/Imaging-1.1.6.tar.gz"))
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

;  agrigento:~/izip/git/opensource/genenetwork$ scp ./contrib/piddle-1.0.15-gn.tgz penguin2.genenetwork.org
; penguin2:~$ env IPFS_PATH=/export/ipfs/ ipfs add piddle-1.0.15-gn.tgz
; added QmSMptV2VALL2s7igqRqKJ8ALNvhqFRUYVG54kEF7ac6ve piddle-1.0.15-gn.tgz
; penguin2:~$ env IPFS_PATH=/export/ipfs/ ipfs pin add -r QmSMptV2VALL2s7igqRqKJ8ALNvhqFRUYVG54kEF7ac6ve
; pinned QmSMptV2VALL2s7igqRqKJ8ALNvhqFRUYVG54kEF7ac6ve recursively

(define-public python2-piddle-gn ; guix obsolete
  (package
    (name "python2-piddle-gn")
    (version "1.0.15-gn")
    (source (origin
     (method url-fetch)
     (uri (string-append
           "http://ipfs.genenetwork.org/ipfs/QmeKcMb8AdwZNUcAaTASVpZ39ipwJn8eBoqqDfoCzQYmNk/piddle-" version ".tgz"))
     (sha256
      (base32
       "05gjnn31v7p0kh58qixrpcizcxqf3b7zv4a5kk8nsmqwgxh0c6gq"))))

    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (propagated-inputs
     `(("python2-pil1" ,python2-pil1-gn)))
    (arguments
     `(
       #:python ,python-2
       #:tests? #f   ; no 'setup.py test' really!
    ))
    (home-page #f)
    (synopsis "Canvas drawing library for python2 (old!)")
    (description #f)
    (license #f)))


(define-public python2-pil1 ; guix obsolete
  (package
    (name "python2-pil1") ; this version is NOT used by genenetwork
    (version "1.1.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://effbot.org/downloads/Imaging-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "04aj80jhfbmxqzvmq40zfi4z3cw6vi01m3wkk6diz3lc971cfnw9"))
    (modules '((guix build utils)))
    (snippet
     ;; Adapt to newer freetype and lcms.  As the package is unmaintained
     ;; upstream, there is no use in creating a patch and reporting it.
     '(begin (substitute* "_imagingft.c"
               (("freetype/")
                "freetype2/freetype/"))
             (substitute* '("setup.py"
                            "_imagingcms.c")
               (("lcms.h") "lcms2.h"))))))
    (build-system python-build-system)
    (inputs
     `(("freetype" ,freetype)
       ("lcms" ,lcms) ; not fully supported
       ("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("tcl" ,tcl)
       ("zlib" ,zlib)))
    (arguments
     ;; Only the fork python-pillow works with Python 3.
     `(#:python ,python-2
       #:tests? #f ; no check target
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((freetype (assoc-ref inputs "freetype"))
                   (jpeg (assoc-ref inputs "libjpeg"))
                   (lcms (assoc-ref inputs "lcms"))
                   (tcl (assoc-ref inputs "tcl"))
                   (tiff (assoc-ref inputs "libtiff"))
                   (zlib (assoc-ref inputs "zlib")))
               (substitute* "setup.py"
                 (("FREETYPE_ROOT .*")
                  (string-append "FREETYPE_ROOT = libinclude(\"" freetype "\")\n"))
                 (("JPEG_ROOT .*")
                  (string-append "JPEG_ROOT = libinclude(\"" jpeg "\")\n"))
                 (("LCMS_ROOT .*")
                  (string-append "LCMS_ROOT = libinclude(\"" lcms "\")\n"))
                 (("^TCL_ROOT .*")
                  (string-append "TCL_ROOT = libinclude(\"" tcl "\")\n"))
                 (("TIFF_ROOT .*")
                  (string-append "TIFF_ROOT = libinclude(\"" tiff "\")\n"))
                 (("ZLIB_ROOT .*")
                  (string-append "ZLIB_ROOT = libinclude(\"" zlib "\")\n"))))
             #t)))))
    (home-page "http://www.pythonware.com/products/pil/")
    (synopsis "Python Imaging Library")
    (description "The @dfn{Python Imaging Library} (PIL) adds image processing
capabilities to your Python interpreter.  This library supports many file
formats, and provides powerful image processing and graphics capabilities.

NOTE: This package is superseded by python-pillow")
    (license (license:x11-style
               "file://README"
               "See 'README' in the distribution."))))

; upstream version won't work with gn2
(define-public python2-piddle
  (package
    (name "python2-piddle")
    (version "1.0.15")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/piddle/piddle/"
                            version "/piddle-" version ".zip"))
        (sha256
         (base32
          "0jaxfsrcgqb5cf2wznxnpdws5khlrdixmg85lrhq2zl9cy6dfdya"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f
       #:tests? #f)) ; tests are interactive
    (native-inputs
     `(("unzip" ,unzip)))
    (propagated-inputs
     `(("python2-pil" ,python2-pil1-gn)))
    (home-page "http://www.strout.net/info/coding/python/piddle/")
    (synopsis "Plug-In Drawing, Does Little Else")
    (description "PIDDLE is designed for vector graphics -- i.e., drawing of
primitives such as lines and ellipses, rather than manipulation of individual
pixels.")
    (license license:gpl2+)))

(define-public python2-parallel ; guix fix number of things
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

(define-public python2-numarray ; guix: obsolete lib
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
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "python" "setup.py" "build"))))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Build and install the Python bindings.  The underlying
                    ;; C++ library is apparently not meant to be installed.
                    (let ((out (assoc-ref outputs "out")))
                      (system* "python" "setup.py" "install"
                               (string-append "--prefix=" out))))))
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "http://www.numpy.org/")
    (synopsis "Numerical library array processing of numbers, strings, records and objects")
    (description
     "Numarray is an (OBSOLETE) array processing package designed to
efficiently manipulate large multi-dimensional arrays. Numarray is
modelled after Numeric and features c-code generated from python
template scripts, the capacity to operate directly on arrays in files,
and improved type promotions. Numarray provides support for
manipulating arrays consisting of numbers, strings, records, or
objects using the same basic infrastructure and syntax.  Numarray is
now part of the numpy package, though some legacy software still uses
the older versions.")
    (license license:gpl2))) ; actualy PyRAF http://www.stsci.edu/resources/software_hardware/pyraf/LICENSE

(define-public python-htmlgen
  (package
    (name "python-htmlgen")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://github.com/srittau/python-htmlgen/archive/v"
	     version ".tar.gz"))
       (sha256
	(base32
	 "0w3pcvm68jfdhnsxwwwifwhqqj9kqg3195b52hqd23qza480xilx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-typing" ,python-typing)))
    (arguments
     `(#:tests? #f))
    (synopsis "Python HTML 5 Generator")
    (description "This is a python library for generating html from classes.")
    (home-page "https://github.com/srittau/python-htmlgen")
    (license license:expat)))

(define-public python2-htmlgen
  (package-with-python2 python-htmlgen))

(define-public python-version
(let ((commit "e5aadc720bb74c535f29e5a2de5cd9697efe8d7c"))
(package
  (name "python-version")
  (version "0.1.2")
  (source
    (origin
      (method git-fetch)
      (uri (git-reference
      ; (url "https://github.com/genenetwork/pylmm.git")
        (url "https://github.com/keleshev/version.git") ; version not in pypi
        (commit commit)))
      (file-name (string-append name "-" commit))
      (sha256
        (base32
          "1rc8kf72v180qlygkh1y0jwv2fxqpx7n97bqfhbwgnn31iwai9g3"))))
  (build-system python-build-system)
  (propagated-inputs
    `(
    ("python-more-itertools" ,python-more-itertools)
    ("python-pytest" ,python-pytest)))
  (home-page "http://github.com/halst/version")
  (synopsis "Implementation of semantic version")
  (description
    "Implementation of semantic version")
  (license license:expat)
)))

(define-public python-mypy-extensions
(package
  (name "python-mypy-extensions")
  (version "0.4.1")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "mypy_extensions" version))
      (sha256
        (base32
          "04h8brrbbx151dfa2cvvlnxgmb5wa00mhd2z7nd20s8kyibfkq1p"))))
  (build-system python-build-system)
  (inputs
    `(("python-version" ,python-version)))
  (home-page "http://www.mypy-lang.org/")
  (synopsis
    "Experimental type system extensions for programs checked with the mypy typechecker.")
  (description
    "Experimental type system extensions for programs checked with the mypy typechecker.")
  (license #f))
)


(define-public python-arcp
(package
  (name "python-arcp")
  (version "0.2.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "arcp" version))
      (sha256
        (base32
          "0h8sn0mlb6vb8wqqnqc4pxdklrkyx3p72afdhm7b9kyalrqzd7dd"))))
  (build-system python-build-system)
  (home-page "http://arcp.readthedocs.io/")
  (synopsis
    "arcp (Archive and Package) URI parser and generator")
  (description
    "arcp (Archive and Package) URI parser and generator")
  (license license:asl2.0))
)

(define-public python2-pp
  (package
    (name "python2-pp")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
             "http://www.parallelpython.com/downloads/pp/pp-" version ".zip"))
        (sha256
         (base32
          "0qkxcyclz3vgwpl6xvsrg76q59dj0wwy8qx15567bafv659ypyb1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f
       #:tests? #f)) ; no tests
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.parallelpython.com")
    (synopsis "Parallel and distributed programming for Python")
    (description "PP is a python module which provides mechanism for parallel
execution of python code on SMP (systems with multiple processors or cores) and
clusters (computers connected via network).")
    (license license:bsd-3)))

(define GN1-thirdparty-sources
  (origin
    (method url-fetch/tarbomb)
    ;; ipfs get QmTPwYT2pehdxdG1TiHEzVzLgbeuhJ4utXShuz3twA84AB
    (uri "file:///gnu/store/p33a2sh3x2nhiiphdw9nly80njg6p8fi-thirdparty.tgz")
    (file-name "GN1-thirdparty")
    (sha256
     (base32
      "0nnp6g412hjfrcn3k2yrfb14sxv06k0149whc7qmv678nyj5zhfa"))))

(define-public python2-json-GN1
  (package
    (name "python2-json-GN1")
    (version "GN1")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.7/site-packages/json/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/json" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public python2-pyx-GN1
  (package
    (name "python2-pyx-GN1")
    (version "0.8")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.7/site-packages/pyx/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/pyx" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public python2-pyxlwriter
  (package
    (name "python2-pyxlwriter")
    (version "0.4a3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/pyxlwriter/pyxlwriter/"
                            version "/pyXLWriter-" version ".zip"))
        (sha256
         (base32
          "1kfsi6la9y53rwayszgayfmkjfknpp650v69a0hwd1fcfk1df735"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f
       #:tests? #f)) ; no tests
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://sourceforge.net/projects/pyxlwriter/")
    (synopsis "Python library for generating Excel compatible spreadsheets")
    (description "PyXLWriter is a Python library for generating Excel compatible
spreadsheets.  It's a port of John McNamara's Perl @code{Spreadsheet::WriteExcel}
module version 1.01 to Python.  It allows writing of Excel compatible
spreadsheets without the need for COM objects.")
    (license license:lgpl2.1+)))

(define-public python2-svg-GN1
  (package
    (name "python2-svg-GN1")
    (version "1.0")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.7/site-packages/svg/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/svg" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-4)))

(define-public python-admiral
  (package
    (name "python-admiral")
    (version "0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "admiral" version))
        (sha256
         (base32
          "1b2zjgyz94ld5wr7s4cm4x5sxijx3w0dmd7r2cq1s8iqjzz6rd1x"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))      ; No tests
    (propagated-inputs
     `(("python-humanfriendly" ,python-humanfriendly)))
    (home-page "https://github.com/nspies/admiral")
    (synopsis
     "Simple python high-performance computing cluster batch submission")
    (description
     "Simple python high-performance computing cluster batch submission.")
    (license #f)))      ; No license in repository.

(define-public python2-admiral
  (package-with-python2 python-admiral))

(define-public python-cachecontrol-0.11
  (package
    (inherit python-cachecontrol)
    (name "python-cachecontrol")
    (version "0.11.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "CacheControl" version))
        (sha256
         (base32
          "07jsfhlbcwgqg6ayz8nznzaqg5rmxqblbzxz1qvg5wc44pcjjy4g"))))))

(define-public python-ruamel.yaml-0.15
  (package
    (inherit python-ruamel.yaml)
    (name "python-ruamel.yaml")
    (version "0.15.77")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ruamel.yaml" version))
        (sha256
         (base32
          "1mhzxkkiv3xmr9izrgk78x7f1r5gi8kd5ac7z3vn7j00q1ydn6da"))))))

(define-public python-pbr-1.6.0
  (package
    (inherit python-pbr)
    (name "python-pbr")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pbr" version))
        (sha256
         (base32
          "1lg1klrczvzfan89y3bl9ykrknl3nb01vvai37fkww24apzyibjf"))))))

(define-public python-arvados-python-client
  (package
    (name "python-arvados-python-client")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "arvados-python-client" version))
        (sha256
         (base32
          "19l4w6m5426x5k2kick630dh2jx26j16ycs2nhbfgr4cd43d29y4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))    ; tests not included?
    (propagated-inputs
     `(("python-ciso8601" ,python-ciso8601)
       ("python-future" ,python-future)
       ;("python-google-api-python-client" ,python-google-api-python-client)
       ("python-google-api-client" ,python-google-api-client)
       ("python-httplib2" ,python-httplib2)
       ("python-pycurl" ,python-pycurl)
       ("python-ruamel.yaml" ,python-ruamel.yaml-0.15)
       ("python-setuptools" ,python-setuptools)
       ("python-ws4py" ,python-ws4py)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-oauth2client" ,python-oauth2client)
       ("python-pbr" ,python-pbr-1.6.0)
       ("python-pyyaml" ,python-pyyaml)
       ("python-uritemplate" ,python-uritemplate)))
    (home-page "https://arvados.org")
    (synopsis "Arvados client library")
    (description "This package provides the arvados module, an API client for
Arvados.  It also includes higher-level functions to help you write Crunch
scripts, and command-line tools to store and retrieve data in the Keep storage
server.")
    (license license:asl2.0)))

(define-public python-schema-salad
  (package
    (name "python-schema-salad")
    (version "5.0.20200416112825")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "schema-salad" version))
        (sha256
         (base32
          "1pm6q266qrw4r0w0vnzhsvqgk5j8b3q61hxg99awhgpjsmcvkmsz"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cachecontrol" ,python-cachecontrol-0.11)
       ("python-lockfile" ,python-lockfile)
       ("python-mistune" ,python-mistune)
       ("python-rdflib" ,python-rdflib)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ("python-requests" ,python-requests)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-setuptools" ,python-setuptools)
       ("python-typing-extensions" ,python-typing-extensions)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/common-workflow-language/schema_salad")
    (synopsis "Schema Annotations for Linked Avro Data (SALAD)")
    (description
     "Salad is a schema language for describing JSON or YAML structured linked
data documents.  Salad schema describes rules for preprocessing, structural
validation, and hyperlink checking for documents described by a Salad schema.
Salad supports rich data modeling with inheritance, template specialization,
object identifiers, object references, documentation generation, code
generation, and transformation to RDF.  Salad provides a bridge between document
and record oriented data modeling and the Semantic Web.")
    (license license:asl2.0)))

(define-public python-cfgraph
  (package
   (name "python-cfgraph")
   (version "0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "CFGraph" version))
     (sha256
      (base32
       "0x7yz0lvqb6mkhl5fbml27sppmscgpf8v2ism9jzzf0h982ffzxm"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-rdflib" ,python-rdflib)))
   (home-page "http://github.com/hsolbrig/CFGraph")
   (synopsis "rdflib collections flattening graph")
   (description
    "rdflib collections flattening graph")
   (license license:asl2.0)))

(define-public python-pbr
  (package
   (name "python-pbr")
   (version "5.4.5")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pbr" version))
     (sha256
      (base32
       "0p7mkfgjvqmkn93qpp8kr8mmc06cvxka6x3lhnpmrc1krvz5ix87"))))
    (arguments
     `(#:tests? #f))    ; tests not working
   (build-system python-build-system)
   (home-page
    "https://docs.openstack.org/pbr/latest/")
   (synopsis "Python Build Reasonableness")
   (description "Python Build Reasonableness")
   (license #f)))


(define-public python-pyshex
  (package
   (name "python-pyshex")
   (version "0.7.14")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "PyShEx" version))
     (sha256
      (base32
       "1fy664bh6hpmr4cf49fwwxng36kv7s6b2986hbv0cqcypc4ri2cs"))))
    (arguments
     `(#:tests? #f))    ; tests not working
   (build-system python-build-system)
   (propagated-inputs
    `(("python-cfgraph" ,python-cfgraph)
      ; ("python-pyshexc" ,python-pyshexc)
      ("python-rdflib" ,python-rdflib)
      ("python-pbr" ,python-pbr)
      ("python-rdflib-jsonld" ,python-rdflib-jsonld)
      ("python-requests" ,python-requests)
      ; ("python-shexjsg" ,python-shexjsg)
      ; ("python-sparql-slurper" ,python-sparql-slurper)
      ; ("python-sparqlwrapper" ,python-sparqlwrapper)
      ("python-urllib3" ,python-urllib3)))
   (home-page "https://github.com/hsolbrig/PyShEx")
   (synopsis "Python ShEx Implementation")
   (description "Python ShEx Implementation")
   (license license:asl2.0)))

(define-public python-pyshexc
  (package
    (name "python-pyshexc")
    (version "0.3.4")
    (source
      (origin
        ;; Release doesn't include the test suite.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/shexSpec/grammar")
               ;; Releases aren't tagged.
               (commit "033f1a60954d39c88ac516179dbaa780d6db585d")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07zjizq5lcgx581cznl9mzfmbyxr9h93r9sy23f1xifsafr3s2xh"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; Test suite isn't well supported :/
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (chdir "parsers/python")
             (substitute* "tests/test_basic_parser.py"
               (("BasicParserTestCase.repo_url.*")
                (string-append "BasicParserTestCase.repo_url = \""
                               (assoc-ref inputs "test-suite")
                               "/shemas\"\n")))
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (if tests?
               (begin (add-installed-pythonpath inputs outputs)
                      (with-directory-excursion "tests"
                        (invoke "python" "build_test_harness.py")
                        (invoke "python" "test_basic_parser.py")
                        (invoke "python" "test_issue_2.py")
                        (invoke "python" "test_shexr.py")))
               #t))))))
    (propagated-inputs
     `(("python-antlr4-python3-runtime" ,python-antlr4-python3-runtime)
       ("python-jsonasobj" ,python-jsonasobj)
       ("python-pyjsg" ,python-pyjsg)
       ("python-rdflib" ,python-rdflib)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ("python-requests" ,python-requests)
       ("python-shexjsg" ,python-shexjsg)))
    (native-inputs
     `(("python-yadict-compare" ,python-yadict-compare)
       ("test-suite"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/shexSpec/shexTest")
                  (commit "v2.0.2")))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "1x788nyrwycfr55wbg0ay6mc8mi6wwsg81h614rx9pw6rvrsppps"))))))
    (home-page "https://github.com/shexSpec/grammar/tree/master/parsers/python")
    (synopsis "Python ShExC Parser")
    (description "This package converts the @dfn{Shape Expression Compact}
(ShExC) into @dfn{Python JSON Schema Binding} (pyjsg) objects.")
    (license license:asl2.0)))

(define-public python-antlr4-python3-runtime
  (package
    (name "python-antlr4-python3-runtime")
    (version "4.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "antlr4-python3-runtime" version))
        (sha256
         (base32
          "0cyv8l216i921gxhbc96f797vkijmmc40a6jwys758qj0mfkyy8m"))))
    (build-system python-build-system)
    (home-page "https://www.antlr.org/")
    (synopsis "ANTLR runtime for Python")
    (description "This package provides the ANTLR runtime for Python.")
    (license license:bsd-3)))

(define-public python-jsonasobj
  (package
    (name "python-jsonasobj")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jsonasobj" version))
        (sha256
         (base32
          "1yj8y3k3fb7lk043f1zhmhb2lzjlfpnxajb92rpxmjzja93yxx0y"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "setup.py"
               (("yadict_compare") "['dict_compare"))
             #t)))))
    (native-inputs
     `(("python-yadict-compare" ,python-yadict-compare)))
    (home-page "https://github.com/hsolbrig/jsonasobj")
    (synopsis "JSON as python objects")
    (description
     "This package provides an extension to the core python json library that
treats name/value pairs as first class attributes whenever possible.")
    (license license:asl2.0)))

(define-public python-shexjsg
  (package
    (name "python-shexjsg")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ShExJSG" version))
        (sha256
         (base32
          "04pb32g1y024rdyp5h7kq3pkgyp7jadwa737y6zfpdvqf25d3jvi"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; Skip tests, they fail due to pyshexc missing.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("requirements.txt"
                            "requirements-dev.txt")
              (("pyshexc.*") "") ; no loops
               (("==.*") "\n"))
             #t)))))
    (propagated-inputs
     `(("python-antlr4-python3-runtime" ,python-antlr4-python3-runtime)
       ("python-certifi" ,python-certifi)
       ("python-chardet" ,python-chardet)
       ("python-idna" ,python-idna)
       ("python-isodate" ,python-isodate)
       ("python-requests" ,python-requests)
       ("python-urllib3" ,python-urllib3)))
    (native-inputs
     `(("python-jsonasobj" ,python-jsonasobj)
       ("python-pbr" ,python-pbr)
       ("python-pyjsg" ,python-pyjsg)
       ("python-pyparsing" ,python-pyparsing)
       ;("python-pyshexc" ,python-pyshexc) ; yay loops!
       ("python-rdflib" ,python-rdflib)
       ("python-rdflib-jsonld" ,python-rdflib-jsonld)
       ("python-six" ,python-six)
       ("python-yadict-compare" ,python-yadict-compare)))
    (home-page "https://github.com/hsolbrig/ShExJSG")
    (synopsis "Astract Syntax Tree for the ShEx 2.0 language")
    (description "This package provides an astract syntax tree for the
ShEx 2.0 language.")
    (license license:cc0)))

(define-public python-yadict-compare
  (package
    (name "python-yadict-compare")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "yadict_compare" version))
        (sha256
         (base32
          "1kkcw82cp6mf3jailckd9gya4r7wjyz4gc5azsj2njj4wqn081rw"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/hsolbrig/dict_compare")
    (synopsis "Dictionary comparison tool with filtering and reporting")
    (description
     "A dictionary comparison tool that allows the injection of filters and
handles recursion and lists.")
    (license license:bsd-3)))

(define-public python-pyjsg
  (package
    (name "python-pyjsg")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "PyJSG" version))
        (sha256
         (base32
          "1zpd34svis4pc2hdry02mavz699i2dmaxir9q59ldxdy40jfrhq0"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f      ; tests not included
       #:phases
       (modify-phases %standard-phases
         ;; From tox.ini
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (for-each
                 (lambda (dir)
                   (invoke "python" "-m" "unittest" "discover" "-s" dir))
                 '("tests/test_issues"
                   "tests/test_basics"
                   "tests/test_jsglib"
                   "tests/test_parser_impl"
                   "tests/test_python_generator"
                   "tests_standalone"
                   "tests_standalone_2"))
               #t))))))
    (propagated-inputs
     `(("python-antlr4-python3-runtime" ,python-antlr4-python3-runtime)
       ("python-jsonasobj" ,python-jsonasobj)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-unittest2" ,python-unittest2)
       ("python-yadict-compare" ,python-yadict-compare)))
    (home-page "https://github.com/hsolbrig/pyjsg")
    (synopsis
     "Python JSON Schema Grammar bindings")
    (description
     "A tool to create Python classes that represent JSON objects defined in JSG.")
    (license license:asl2.0)))
