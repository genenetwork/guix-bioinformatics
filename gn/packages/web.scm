(define-module (gn packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gn packages python24)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

; <script src="https://code.jquery.com/jquery-3.2.1.slim.min.js" integrity="sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN" crossorigin="anonymous"></script>

(define-public web-jquery
  (package
    (name "web-jquery")
    (version "3.4.1") ; May 1, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jquery/jquery.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "03pasw49xshwvcaa0iailv43jlyhkwhf19fnwmwlhyn4701kpx1d"))))
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder
        (let* ((out (assoc-ref %outputs "out"))
               (targetdir (string-append out "/share/web/jquery"))
               (source (assoc-ref %build-inputs "source"))
               (dist (string-append source "/dist")))
          (begin
            (use-modules (guix build utils))
            (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://jquery.com/")
    (synopsis "JQuery web framework")
    (description "jQuery is a fast, small, and feature-rich JavaScript library.
It makes things like HTML document traversal and manipulation, event handling,
animation, and Ajax much simpler with an easy-to-use API that works across a
multitude of browsers.  With a combination of versatility and extensibility,
jQuery has changed the way that millions of people write JavaScript.")
    (license license:expat)))

(define-public web-bootstrap
  (package
    (name "web-bootstrap")
    (version "4.3.1") ; Feb. 13, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/twbs/bootstrap.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "18g76r53sa2ahcriy7jk5wvxd3s8qc4as87xwqvfkxibdn5ifrxs"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
         (let* ((out (assoc-ref %outputs "out"))
                (name "bootstrap")
                (targetdir (string-append out "/share/web/" name))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (begin
             (use-modules (guix build utils))
             (copy-recursively dist targetdir)))))
    (native-inputs `(("source" ,source)))
    (home-page "https://getbootstrap.com/")
    (synopsis "Bootstrap web framework")
    (description "Bootstrap is an open source toolkit for developing
with HTML, CSS, and JS.  Quickly prototype your ideas or build your
entire app with our Sass variables and mixins, responsive grid system,
extensive prebuilt components, and powerful plugins built on jQuery.")
    (license license:expat)))

(define-public web-bootstrap-3
  (package
    (inherit web-bootstrap)
    (name "web-bootstrap")
    (version "3.4.1") ; Feb. 13, 2019
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/twbs/bootstrap.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wsv79rvzaacyf740mwmhxadpwf28pad711jhbxl26zgqjrpzcbp"))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (name "bootstrap")
                (targetdir (string-append out "/share/web/" name))
                (source (assoc-ref %build-inputs "source"))
                (dist (string-append source "/dist")))
           (copy-recursively dist targetdir)
           (substitute* (find-files (string-append targetdir "/css") ".")
             (("../fonts") (string-append targetdir "/fonts")))
           #t))))
    (native-inputs `(("source" ,source)))))

(define-public web-bootstrap-3.3
  (package
    (inherit web-bootstrap-3)
    (name "web-bootstrap")
    (version "3.3.7") ; July 25, 2016
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/twbs/bootstrap.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0li7vdr4avz34b9xvwk7skbvnvzbx002cw5nfm7iwvi1wk8v5bri"))))
    (native-inputs `(("source" ,source)))))

(define-public web-bootstrap-native
  (let ((commit "2e48d7ee29d4063e3bd2024ff83ddc50a550c4dd"))
    (package
      (name "web-bootstrap-native")
      (version (string-append "4.0.0" "-beta-" (string-take commit 7))) ; Sept, 8, 2017
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/thednp/bootstrap.native.git")
                       (commit commit)))
                (file-name (string-append name "-" commit))
                (sha256
                 (base32
                  "1hkyibyfby0mnkavr3xbmr20kb88wy6jw28b206pd236xnp2qkx0"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (let* ((out (assoc-ref %outputs "out"))
                (name "bootstrap-native")
                (targetdir (string-append out "/share/web/" name))
                (source (assoc-ref %build-inputs "source")))
           (begin
             (use-modules (guix build utils))
             (copy-recursively source targetdir)))))
      (native-inputs `(("source" ,source)))
      (home-page "https://github.com/thednp/bootstrap.native")
      (synopsis "Bootstrap minimal")
      (description "Bootstrap native does not use jquery.")
      (license license:expat))))

(define-public httpd-2.2
  (package
    (inherit httpd)
    (version "2.2.34")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/httpd/httpd-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0q4196krxbyaswl5yzmm0r5ry2dijfhvxjdld3bl0mxcvzaq6cg5"))))
    (inputs
     `(("openssl" ,openssl-1.0)
       ,@(alist-delete "openssl" (package-inputs httpd))))
    (arguments
     (substitute-keyword-arguments (package-arguments httpd)
       ((#:configure-flags flags)
        `(cons "--enable-mods-shared=most" ,flags))))))

(define-public mod-python
  (let ((commit "902bb8700e2c45ffd96b78e2f1146a3c101be7f5")
        (revision "1"))
    (package
      (name "mod-python")
      (version (git-version "3.5.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/grisha/mod_python.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0lqlpldw11dml24n05305xzpbjlkay497djjczvgsj5v8djkxcq0"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f ; tests require a running apache server
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out    (assoc-ref outputs "out")))
                 (substitute* "Makefile.in"
                   ;; Don't use httpd's prefix
                   (("LIBEXECDIR=.*") (string-append "LIBEXECDIR=" out "/modules\n")))
                 (substitute* "scripts/Makefile.in"
                   (("BINDIR=.*") (string-append "BINDIR=" out "/bin")))
                 (substitute* "dist/setup.py.in"
                   ;; we want out modules dir, not httpd's
                   ;; If we want SYSCONFDIR to be the same as httpd's :
                   ;; (("@SYSCONFDIR@") (string-append (assoc-ref %build-inputs "httpd") "/etc/httpd"))
                   (("@LIBEXECDIR@") (string-append out "/modules")))
                 (substitute* "dist/Makefile.in"
                   (("\\$\\(DESTDIR\\)") "/")
                   (("--root") (string-append "--prefix=" out " --root"))))
               #t)))))
      (inputs
       `(("httpd" ,httpd)
         ("readline" ,readline)
         ("python" ,python-2))) ; does not seem to build with python3.7+
      (native-inputs `(("flex" ,(@ (gnu packages flex) flex))))
      (home-page "http://modpython.org/")
      (synopsis "Apache/Python Integration")
      (description "Mod_python is an Apache module that embeds the Python
interpreter within the server.  With mod_python you can write web-based
applications in Python that will run many times faster than traditional CGI and
will have access to advanced features such as ability to retain database
connections and other data between hits and access to Apache internals.")
      (license license:asl2.0))))

(define-public mod-python-24-httpd22
  (package
    (inherit mod-python)
    (name "mod-python-24-httpd22")
    (version "3.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://archive.apache.org/dist/httpd/"
                            "modpython/mod_python-" version ".tgz"))
        (sha256
         (base32
          "0sss2xi6l1a2z8y6ji0cp8vgyvnhq8zrg0ilkvpj1mygbzyk28xd"))
        (patches
          (list
            (origin
              (method url-fetch)
              (uri "https://sources.debian.org/data/main/liba/libapache2-mod-python/3.3.1-11/debian/patches/04_autoconf_python_multiarch.patch")
              (file-name "mod-python-24-python-discovery.patch")
              (sha256
               (base32
                "0n3zp8j6q0mp0scry7d2hi0baqkim42bqq2c81p4l6mizsy8ry4h")))
            (origin
              (method url-fetch)
              (uri "https://sources.debian.org/data/main/liba/libapache2-mod-python/3.3.1-11/debian/patches/10_bts521965.patch")
              (file-name "mod-python-24-apr13-compat.patch")
              (sha256
               (base32
                "1k2cd2r13938fbm473sn0ivicaylkcqigyqn2wjir9ppch98kybg")))))))
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((python (assoc-ref inputs "python"))
                    (tcl (assoc-ref inputs "tcl"))
                    (py-version (python:python-version python))
                    (tcl-version ,(version-major+minor (package-version tcl))))
               (substitute* "configure.in"
                 (("PY_LIBS=.*")
                  (string-append "PY_LIBS=-L" python "/lib/python" py-version "\n"))
                 (("PY_LDFLAGS=.*")
                  (string-append "PY_LDFLAGS=\"-lpython" py-version
                                 " -lreadline -lssl -lcrypto"
                                 " -ltk" tcl-version " -ltcl" tcl-version
                                 " -lgdbm -ltirpc -lnsl -lz\"\n"))
                 (("PY_INCLUDES=.*")
                  (string-append "PY_INCLUDES=-I" python "/include/python" py-version "\n")))
               (invoke "autoreconf" "-vfi"))))
         (add-after 'unpack 'patch-sources
           (lambda _
             (substitute* "test/test.py"
               (("2\\.2") "2.4"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "src/mod_python.so" (string-append out "/modules"))
               (with-directory-excursion "dist"
                 (invoke "python" "setup.py" "install" "--root=/"
                         (string-append "--prefix=" out)))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("flex" ,(@ (gnu packages flex) flex))))
    (inputs
     `(("httpd" ,httpd-2.2)
       ("python" ,python-2.4)
       ,@(package-inputs python-2.4)))))

(define-public mod-python-24
  (package
    (inherit mod-python-24-httpd22)
    (name "mod-python-24")
    (source
      (origin
        (inherit (package-source mod-python-24-httpd22))
        (patches
          (append
            (origin-patches (package-source mod-python-24-httpd22))
            (list
              (origin
                (method url-fetch)
                (uri "https://sources.debian.org/data/main/liba/libapache2-mod-python/3.3.1-11/debian/patches/20_apache24.patch")
                (file-name "mod-python-24-apache24-compat.patch")
                (sha256
                 (base32
                  "1bmcx7ki7y486x6490yppssr7dh3a0qyki6gjf2lj83gyh68c0r0"))))))))
    (inputs
     `(("httpd" ,httpd)
       ("python" ,python-2.4)
       ,@(package-inputs python-2.4)))))

(define-public httpd-mod-python-24
  (package
    (inherit httpd)
    (name "httpd-mod-python-24")
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:modules (((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils)
                  ,@%gnu-build-system-modules
                  ((guix build python-build-system) #:prefix python:))
       ,@(substitute-keyword-arguments (package-arguments httpd)
          ((#:phases phases '%standard-phases)
           `(modify-phases ,phases
              (add-after 'install 'unpack-mod-python
                (lambda* args
                  ((assoc-ref gnu:%standard-phases 'unpack)
                   #:source (assoc-ref %build-inputs "mod-python"))))
              (add-after 'unpack-mod-python 'change-directory
                (lambda _
                  ;; Make sure we're in the correct folder
                  (chdir "../mod_python-3.3.1") #t))
              (add-after 'change-directory 'bootstrap-mod-python
                (lambda* (#:key inputs #:allow-other-keys)
                  (let* ((python (assoc-ref inputs "python"))
                         (tcl (assoc-ref inputs "tcl"))
                         (py-version (python:python-version python))
                         (tcl-version ,(version-major+minor (package-version tcl))))
                    (substitute* "configure.in"
                      (("PY_LIBS=.*")
                       (string-append "PY_LIBS=-L" python "/lib/python" py-version "\n"))
                      (("PY_LDFLAGS=.*")
                       (string-append "PY_LDFLAGS=\"-lpython" py-version
                                      " -lreadline -lssl -lcrypto"
                                      " -ltk" tcl-version " -ltcl" tcl-version
                                      " -lgdbm -ltirpc -lnsl -lz\"\n"))
                      (("PY_INCLUDES=.*")
                       (string-append "PY_INCLUDES=-I" python "/include/python" py-version "\n")))
                    (invoke "autoreconf" "-vfi"))))
              (add-after 'bootstrap-mod-python 'patch-bin-file-mod-python
                (assoc-ref gnu:%standard-phases 'patch-usr-bin-file))
              (add-after 'patch-bin-file-mod-python 'patch-source-shebangs-mod-python
                (assoc-ref gnu:%standard-phases 'patch-source-shebangs))
              (add-after 'patch-source-shebangs-mod-python 'configure-mod-python
                (lambda* args
                  ((assoc-ref gnu:%standard-phases 'configure)
                   #:outputs %outputs
                   #:inputs %build-inputs
                   #:configure-flags (list (string-append "--with-apxs="
                                                          (assoc-ref %outputs "out")
                                                          "/bin/apxs")))))
              (add-after 'configure-mod-python 'patch-more-shebangs-mod-python
                (assoc-ref gnu:%standard-phases 'patch-generated-file-shebangs))
              (add-after 'patch-more-shebangs-mod-python 'make-mod-python
                (assoc-ref gnu:%standard-phases 'build))
              (add-after 'make-mod-python 'install-mod-python
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (install-file "src/mod_python.so" (string-append out "/modules"))
                    (with-directory-excursion "dist"
                      (invoke "python" "setup.py" "install" "--root=/"
                              (string-append "--prefix=" out)))
                    #t))))))))
    (native-inputs
     `(,@(package-native-inputs httpd)
       ,@(package-native-inputs mod-python-24)
       ("mod-python" ,(package-source mod-python-24))))
    (inputs
     `(,@(alist-delete "openssl" (package-inputs httpd))
       ,@(package-inputs python-2.4)
       ("python" ,python-2.4)))))

(define-public httpd22-mod-python-24
  (package
    (inherit httpd-mod-python-24)
    (name "httpd22-mod-python-24")
    (version (package-version httpd-2.2))
    (source
      (origin
        (inherit (package-source httpd-2.2))))
    (arguments
     (substitute-keyword-arguments (package-arguments httpd-mod-python-24)
       ((#:configure-flags flags)
        `(cons "--enable-mods-shared=most" ,flags))))
    (native-inputs
     `(,@(package-native-inputs httpd-2.2)
        ,@(package-native-inputs mod-python-24-httpd22)
       ("mod-python" ,(package-source mod-python-24-httpd22))))
    (inputs
     `(,@(package-inputs httpd-2.2)
       ,@(package-inputs python-2.4)
       ("python" ,python-2.4)))))

(define-public web-font-awesome
  (package
    (inherit font-awesome)
    (name "web-font-awesome")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out  (assoc-ref outputs "out"))
                   (dest (string-append out "/share/web/font-awesome")))
              (for-each
                (lambda (dir)
                  (copy-recursively dir (string-append dest dir)))
                '("css" "fonts" "less" "scss")))
            #t)))))))
