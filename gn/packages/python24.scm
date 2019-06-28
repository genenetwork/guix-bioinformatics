(define-module (gn packages python24)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tcl)
  #:use-module (srfi srfi-1))

;; TODO: Check against 'guix lint -c cve python2.4' list:
;; CVE-2019-9740, CVE-2019-9947, CVE-2019-9948, CVE-2018-1060, CVE-2018-1061,
;; CVE-2014-9365, CVE-2012-0845, CVE-2012-1150, CVE-2011-1521, CVE-2011-4940,
;; CVE-2010-3492, CVE-2008-5031, CVE-2008-5983
(define-public python-2.4
  (package
    (inherit python-2)
    (name "python2.4")
    (version "2.4.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.bz2"))
      (sha256
       (base32
        "021y88a4ki07dgq19yhg6zfvmncfiz7h5b2255438i9zmlwl246s"))))
    (outputs '("out"))
    (arguments
      (substitute-keyword-arguments (package-arguments python-2)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'create-setup-local
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((zlib (assoc-ref inputs "zlib"))
                      (tcl  (assoc-ref inputs "tcl"))
                      (tk   (assoc-ref inputs "tk"))
                      (gdbm (assoc-ref inputs "gdbm"))
                      (read (assoc-ref inputs "readline"))
                      (ssl  (assoc-ref inputs "openssl")))
                  (with-output-to-file "Modules/Setup.local"
                    (lambda _
                      (format #t "readline readline.c -I~a/include -L~a/lib -lreadline~@
                              _ssl _ssl.c -DUSE_SSL -I$~a/include/openssl -L~a/lib -lssl -lcrypto~@
                              _tkinter _tkinter.c tkappinit.c -DWITH_APPINIT -L~a/lib -I~a/include -L~a/lib -I~a/include -ltk~a -ltcl~a~@
                              gdbm gdbmmodule.c -I~a/include -L~a/lib -lgdbm~@
                              zlib zlibmodule.c -I~a/include -L~a/lib -lz~%"
read read ssl ssl tcl tcl tk tk ,(version-major+minor (package-version tcl)) ,(version-major+minor (package-version tcl)) gdbm gdbm zlib zlib))))
                  #t))
            (add-before 'check 'delete-failing-tests
              (lambda _
                (for-each
                  (lambda (file)
                    (delete-file (string-append "Lib/test/" file)))
                  '("test_socket.py" "test_anydbm.py" "test_whichdb.py" "test_zlib.py"))
                #t))
            (add-after 'check 'find-netinet-in-h
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((glibc (assoc-ref inputs "libc")))
                  (substitute* (find-files "Lib/plat-generic" ".*")
                    (("/usr/include/netinet/in.h")
                     (string-append glibc "/include/netinet/in.h")))
                  #t)))
            (delete 'move-tk-inter)))))
    (native-search-paths
      (list (search-path-specification
              (variable "PYTHONPATH")
              (files '("lib/python2.4/site-packages")))))
    (properties '((cpe-name . "python")))))

(define (default-python2.4)
    "Return the default Python-2.4 package."
      ;; Lazily resolve the binding.
        (let ((python (resolve-interface '(gn packages python24))))
              (module-ref python 'python-2.4)))

(define package-with-python24
  ((@@ (guix build-system python) package-with-explicit-python) (delay (default-python2.4))
                                                                "python-" "python24-"
                                                                #:variant-property 'python24-variant))

(define (strip-python24-variant p)
  (package
    (inherit p)
    (properties (alist-delete 'python24-variant (package-properties p)))))

(define-public python24-htmlgen
  (package
    (name "python24-htmlgen")
    (version "0.9")
    ;(version "0.99")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "htmlgen" version))
        (sha256
         (base32
          "14xzjgwdqgs1vs5mq7mg3w48snvgb77yywv64mg8k6qhapmnafdw"))))
          ;"1kbn6jcbf2mpb9f8hm5gcsipy7habqrq4794lpdbzm5mqxlclmnl"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("python24-asserts" ,python24-asserts)))
    (home-page "https://github.com/srittau/python-htmlgen")
    (synopsis "Python HTML 5 Generator")
    (description "Python-htmlgen is a library to generate HTML from classes.")
    (license license:expat)))

(define-public python24-setuptools
  (package
    (inherit python-setuptools)
    (name "python24-setuptools")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "setuptools" version))
        (sha256
         (base32
          "1gfvalhvzcskwj85r3lh9sx190f8k807vz5zln8agaw31ak8cf96"))))
    (arguments
     `(#:python ,python-2.4
       #:tests? #f)))) ; skip the tests

(define-public python24-asserts
  (package
    (name "python24-asserts")
    (version "0.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "asserts" version))
        (sha256
         (base32
          "05ffy111giwv6sqx97vzzsvcra0gxzx2ilv16gyw135v583frxbn"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (home-page "https://github.com/srittau/python-asserts")
    (synopsis "Stand-alone Assertions for Python")
    (description "Stand-alone Assertions for Python")
    (license license:expat)))

(define-public python24-pyx
  (package
    (name "python24-pyx")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyx" version))
        (sha256
         (base32
          "13kyhqx19rw7dlv2xapdb68j8l9laq6nrpgkyd6549qwidmb4dz8"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (home-page "http://pyx.sourceforge.net/")
    (synopsis "Python package for the generation of PostScript, PDF, and SVG files")
    (description "Python package for the generation of PostScript, PDF, and SVG files")
    (license license:gpl2+)))

(define-public python24-pyxlwriter
  (package
    (name "python24-pyxlwriter")
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
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)
       ("unzip" ,unzip)))
    (home-page "https://sourceforge.net/projects/pyxlwriter/")
    (synopsis "Python library for generating Excel compatible spreadsheets")
    (description "PyXLWriter is a Python library for generating Excel compatible
spreadsheets.  It's a port of John McNamara's Perl @code{Spreadsheet::WriteExcel}
module version 1.01 to Python.  It allows writing of Excel compatible
spreadsheets without the need for COM objects.")
    (license license:lgpl2.1+)))

;; TKINTER and LITTLECMS are not found
(define-public python24-pil
  (package
    (name "python24-pil")
    (version "1.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://effbot.org/downloads/Imaging-"
                            version ".tar.gz"))
        (sha256
         (base32
          "04aj80jhfbmxqzvmq40zfi4z3cw6vi01m3wkk6diz3lc971cfnw9"))
        (modules '((guix build utils)))
        (snippet
          ;; Adapt to newer freetype. As the package is unmaintained upstream,
          ;; there is no use in creating a patch and reporting it.
          '(substitute* "_imagingft.c"
                        (("freetype/")
                         "freetype2/freetype/")))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
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
                  (string-append "ZLIB_ROOT = libinclude(\"" zlib "\")\n")))
               (substitute* '("setup.py"
                              "_imagingcms.c")
                 (("lcms.h") "lcms2.h")))
             #t)))))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (inputs
     `(("freetype" ,freetype)
       ("lcms" ,lcms)
       ("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("tcl" ,tcl)
       ("zlib" ,zlib)))
    (home-page "http://www.pythonware.com/products/pil/")
    (synopsis "Python Imaging Library")
    (description "The @dfn{Python Imaging Library} (PIL) adds image processing
capabilities to your Python interpreter.  This library supports many file
formats, and provides powerful image processing and graphics capabilities.")
    (license (license:x11-style
               "file://README"
               "See 'README' in the distribution."))))

(define-public python24-piddle
  (package
    (name "python24-piddle")
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
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)
       ("unzip" ,unzip)))
    (propagated-inputs
     `(("python24-pil" ,python24-pil)))
    (home-page "http://www.strout.net/info/coding/python/piddle/")
    (synopsis "Plug-In Drawing, Does Little Else")
    (description "PIDDLE is designed for vector graphics -- i.e., drawing of
primitives such as lines and ellipses, rather than manipulation of individual
pixels.")
    (license license:gpl2+)))

;; Apparently this is the library which mimics python-2.6+'s json library
(define-public python24-simplejson
  (let ((base (package-with-python24 python-simplejson)))
    (package
      (inherit base)
      (version "2.0.9") ; last version to officially support python2.4
      (source
        (origin
          (method url-fetch)
          (uri (pypi-uri "simplejson" version))
          (sha256
           (base32
            "1vlkxibal9ljabybawnqr3jh6f6g21c5pbrzl65z9vwbfxhg9kdb"))))
      (native-inputs
       `(("python24-setuptools" ,python24-setuptools)
         ,@(package-native-inputs base))))))

(define-public python24-numarray
  (package
    (name "python24-numarray")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/numpy/Old Numarray/" version
             "/numarray-" version ".tar.gz"))
       (sha256
        (base32
         "0x1i4j7yni7k4p9kjxs1lgln1psdmyrz65wp2yr35yn292iw2vbg"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "python" "setup.py" "config" "build"
                     "--gencode" "--use_lapack")))
         (add-after 'unpack 'find-lapack-and-openblas
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lapack (assoc-ref inputs "lapack"))
                   (blas   (assoc-ref inputs "openblas")))
               (substitute* "cfg_packages.py"
                 (("lapack_libs = .*'m']")
                  "lapack_libs = ['lapack', 'openblas', 'm']\n")
                 (("lapack_dirs = .*")
                  (string-append "lapack_dirs = ['"
                                 lapack "/lib', '" blas "/lib']\n"))
                 (("lapack_include_dirs = .*")
                  (string-append "lapack_include_dirs = ['"
                                 lapack "/include', '" blas "/include']\n")))
               #t)))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (invoke "python" "setup.py" "config"
                              "install" "--use_lapack"
                              (string-append "--prefix=" out))))))
       #:tests? #f))   ; no test target
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)))
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (home-page "http://www.numpy.org/")
    (synopsis "Array processing of numbers, strings, records and objects")
    (description "Numarray is an array processing package designed to
efficiently manipulate large multi-dimensional arrays.  Numarray is modelled
after Numeric and features c-code generated from python template scripts, the
capacity to operate directly on arrays in files, and improved type promotions.
Numarray provides support for manipulating arrays consisting of numbers,
strings, records, or objects using the same basic infrastructure and syntax.")
    (license license:bsd-3)))

(define-public python24-pp
  (package
    (name "python24-pp")
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
     `(#:python ,python-2.4))
    (native-inputs
     `(("python24-setuptools" ,python24-setuptools)
       ("unzip" ,unzip)))
    (home-page "http://www.parallelpython.com")
    (synopsis "Parallel and distributed programming for Python")
    (description "PP is a python module which provides mechanism for parallel
execution of python code on SMP (systems with multiple processors or cores) and
clusters (computers connected via network).")
    (license license:bsd-3)))
