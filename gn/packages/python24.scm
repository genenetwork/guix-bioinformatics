(define-module (gn packages python24)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gn packages databases)
  #:use-module (gn packages python)
  #:use-module (gn packages statistics)
  #:use-module (past packages python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

(define (default-python2.4)
    "Return the default Python-2.4 package."
      ;; Lazily resolve the binding.
        (let ((python (resolve-interface '(past packages python))))
              (module-ref python 'python-2.4)))

;; We borrow this from (guix build-system python) since we cannot refer to it
;; with the magic '@@' symbol since Guix has switched to guile-3.0.
(define* (package-with-explicit-python python old-prefix new-prefix
                                       #:key variant-property)
  "Return a procedure of one argument, P.  The procedure creates a package with
the same fields as P, which is assumed to use PYTHON-BUILD-SYSTEM, such that
it is compiled with PYTHON instead.  The inputs are changed recursively
accordingly.  If the name of P starts with OLD-PREFIX, this is replaced by
NEW-PREFIX; otherwise, NEW-PREFIX is prepended to the name.

When VARIANT-PROPERTY is present, it is used as a key to search for
pre-defined variants of this transformation recorded in the 'properties' field
of packages.  The property value must be the promise of a package.  This is a
convenient way for package writers to force the transformation to use
pre-defined variants."
  (define package-variant
    (if variant-property
        (lambda (package)
          (assq-ref (package-properties package)
                    variant-property))
        (const #f)))

  (define (transform p)
    (cond
     ;; If VARIANT-PROPERTY is present, use that.
     ((package-variant p)
      => force)

     ;; Otherwise build the new package object graph.
     ((eq? (package-build-system p) python-build-system)
      (package
        (inherit p)
        (location (package-location p))
        (name (let ((name (package-name p)))
                (string-append new-prefix
                               (if (string-prefix? old-prefix name)
                                   (substring name
                                              (string-length old-prefix))
                                   name))))
        (arguments
         (let ((python (if (promise? python)
                           (force python)
                           python)))
           (ensure-keyword-arguments (package-arguments p)
                                     `(#:python ,python))))))
     (else p)))

  (define (cut? p)
    (or (not (eq? (package-build-system p) python-build-system))
        (package-variant p)))

  (package-mapping transform cut?))

(define package-with-python24
  (package-with-explicit-python (delay (default-python2.4))
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

(define-public python24-pil
  (package
    (inherit python2-pil1)
    (name "python24-pil")
    (arguments
     (substitute-keyword-arguments (package-arguments python2-pil1)
       ((#:python _) python-2.4)))))

(define-public python24-piddle
  (package
    (inherit python2-piddle-gn)
    (name "python24-piddle")
    (arguments
     (substitute-keyword-arguments (package-arguments python2-piddle-gn)
       ((#:python _) python-2.4)))
    (native-inputs `(("python24-setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("python24-pil" ,python24-pil)))))

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

(define-public python24-pp
  (package
    (name "python24-pp")
    (version "1.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://www.parallelpython.com/downloads/pp/pp-" version ".zip"))
        (sha256
         (base32
          "0qkxcyclz3vgwpl6xvsrg76q59dj0wwy8qx15567bafv659ypyb1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:tests? #f)) ; no tests
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.parallelpython.com/")
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

(define-public python24-json-GN1
  (package
    (name "python24-json-GN1")
    (version "GN1")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/json/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/json" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public python24-svg-GN1
  (package
    (name "python24-svg-GN1")
    (version "1.0")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/svg/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/svg" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-4)))

(define-public python24-htmlgen-GN1
  (package
    (name "python24-htmlgen-GN1")
    (version "2.5")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/htmlgen/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/htmlgen" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-2))) ; I'm not actually sure, checked HTMLgen.py

(define-public python24-pyx-GN1
  (package
    (name "python24-pyx-GN1")
    (version "0.8")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/pyx/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/pyx" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public python24-pyxlwriter-GN1
  (package
    (name "python24-pyxlwriter-GN1")
    (version "0.4a3")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (sitedir (string-append out "/lib/python2.4/site-packages/pyXLWriter/")))
               (mkdir-p sitedir)
               (copy-recursively "thirdparty/pyXLWriter" sitedir)
               #t))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public python24-pp-GN1
  (package
    (name "python24-pp-GN1")
    (version "1.5.7")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "thirdparty/pp-1.5.7") #t)))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public python24-numarray-GN1
  (package
    (name "python24-numarray-GN1")
    (version "1.5.2")
    (source GN1-thirdparty-sources)
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
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
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "thirdparty/numarray-1.5.2")
             (for-each make-file-writable (find-files "."))
             #t))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (invoke "python" "setup.py" "config"
                              "install" "--use_lapack"
                              (string-append "--prefix=" out))))))
       #:tests? #f))   ; no test target
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public python24-mysqlclient
  (package
    (name "python24-mysqlclient")
    (version "1.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "MySQL-python" version ".zip"))
        (sha256
         (base32
          "0x0c2jg0bb3pp84njaqiic050qkyd7ymwhfvhipnimg58yv40441"))
        ;(patches
        ;  (list
        ;    (origin
        ;      (method url-fetch)
        ;      (uri "https://sources.debian.org/data/main/p/python-mysqldb/1.2.3-2.1/debian/patches/03_converters_set2str.patch")
        ;      (file-name "mysqlclient-converters_set2str.patch")
        ;      (sha256
        ;       (base32
        ;        "0xkbfscy6kqc84lij1ml7d8vxf5xqi99vx5ha75cg8yyx6cvv34i")))
        ;    (origin
        ;      (method url-fetch)
        ;      (uri "https://github.com/PyMySQL/mysqlclient-python/commit/d663649f851794dffa84010db6290e693d4baab8.patch")
        ;      (file-name "mysqlclient-mariadb-10.2-compat.patch")
        ;      (sha256
        ;       (base32
        ;        "0pz2r8l7299yhl58w0zp0kplg1h9zi3qv9ynpcidxm8mf8884284")))
        ;    ))
        ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         ;(add-after 'unpack 'work-with-newer-mysql
         ;  (lambda _
         ;    (substitute* '("_mysql.c"
         ;                   "MySQLdb/connections.py")
         ;      (("unix_socket") "socket"))
             ;; ProgrammingError: (1064, "You have an error in your SQL syntax; check the manual that corresponds to your MariaDB server version for the right syntax to use near '%s' at line 1")
             ;(substitute* "MySQLdb/cursors.py"
             ;  (("query % tuple.*")
             ;   "query = query.encode(db.unicode_literal.charset)\n"))
         ;    #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; All of these tests require a running mysql server.
             ;; When they are all deleted there are no tests.
             (delete-file "tests/test_MySQLdb_dbapi20.py") ; 35 tests, 54 errors
             (delete-file "tests/test_MySQLdb_capabilities.py") ; 20 tests, 20 errors
             (delete-file "tests/test_MySQLdb_nonstandard.py") ; 14 tests, 6 errors
             (mkdir-p "/tmp/mysqld")
             (call-with-output-file "/tmp/my.cnf"
               (lambda (p)
                 (format p
                         "[mysqld]~@
                         datadir = /tmp/mysqld~@
                         port = 3306~@
                         user = nixbld~@
                         #character-set-server = utf8mb4~@
                         socket = /tmp/mysqld/mysql.sock~%")))
             (setenv "TESTDB" "/tmp/my.cnf")
             ;(system "mysqld --defaults-file=/tmp/my.cnf --initialize &") ; mariadb
             ;(system (string-append (assoc-ref %build-inputs "mysql") "/libexec/mysqld --defaults-file=/tmp/my.cnf &")) ; mysql-5.0
             (system "mysqld --defaults-file=/tmp/my.cnf &") ; mysql-5.5
             (sleep 5)
             (system "mysqladmin -S /tmp/mysqld/mysql.sock variables")
             ;(invoke "mysqladmin" "-S" "/tmp/mysqld/mysql.sock" "variables")
             (system "mysql -S /tmp/mysqld/mysql.sock -e 'create database mysqldb_test charset utf8;'")
             ;(invoke "mysql" "-S" "/tmp/mysqld/mysql.sock"
             ;        "-e" "'create database mysqldb_test charset utf8;'")
             #t))
         )
       #:tests? #t))    ; TODO: Run the test suite
    (native-inputs
     `(("mysql" ,mysql-5.5)
       ("python-nose" ,python24-nose)
       ("python-setuptools" ,python24-setuptools)
       ("unzip" ,unzip)))
    (inputs
     `(("openssl" ,openssl-1.0)
       ("zlib" ,zlib)))
    (home-page "http://mysql-python.sourceforge.net/")
    (synopsis "Python interface to MySQL")
    (description "MySQLdb is an interface to the popular MySQL database server
for Python.  The design goals are:
@itemize
@item with Python database API version 2.0
@item Thread-safety
@item Thread-friendliness (threads will not block each other)
@item Compatibility with MySQL-3.23 and later
@end itemize")
    (license license:gpl2+)))

(define-public python24-rpy2
  (package
    (inherit python-rpy2)
    (name "python24-rpy2")
    (version "2.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rpy2" version))
        (sha256
         (base32
          "0g6vmv4pxc9bwb756z1vfdlzviib84afjmp4l5cw22x8qqvw1s9s"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (with-directory-excursion "rpy"
               ;(invoke "python" "tests.py")
               (invoke "python" "tests_rpy_classic.py")))))))
    (inputs `())
    (propagated-inputs
     `(("python-numpy" ,python24-numpy-1.2)
       ("r-minimal" ,r-minimal-2)
       ("r-survival" ,r-2-survival)))))
