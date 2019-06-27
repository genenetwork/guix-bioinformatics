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
