(define-module (gn packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
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
    (version "3.2.1") ; March 20, 2017
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jquery/jquery.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1x2lxwsm7c6hbg7z7s1fwg9dywmrnlcrbrx3vjksjbvvs6infhqm"))))
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
  (let ((commit "betabeta"))
  (package
    (name "web-bootstrap")
    (version (string-append "4.0.0" "-" (string-take commit 7)))
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/twbs/bootstrap/releases/download/v4.0.0-beta/bootstrap-4.0.0-beta-dist.zip")
       (file-name (string-append name "-" version))
       (sha256
        (base32 "0jzi76gm3vyxld5lz1723al8a8skcn9r1ch51sdgzxx32f273bc9"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
         (let* ((out (assoc-ref %outputs "out"))
                (name "bootstrap")
                (targetdir (string-append out "/share/web/" name))
                )
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source"))
                   (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
                   )
               (and
                (mkdir-p targetdir)
                (zero? (system* unzip source "-d" targetdir))
                ))))))
    (home-page "http://getbootstrap.com/")
    (synopsis "Bootstrap web framework")
    (description "Bootstrap is an open source toolkit for developing
with HTML, CSS, and JS. Quickly prototype your ideas or build your
entire app with our Sass variables and mixins, responsive grid system,
extensive prebuilt components, and powerful plugins built on jQuery.")
    (license license:expat))))

(define-public web-bootstrap-native
  (let ((commit "2e48d7ee29d4063e3bd2024ff83ddc50a550c4dd"))
  (package
    (name "web-bootstrap-native")
    (version (string-append "4.0.0" "-beta-" (string-take commit 7)))
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
    (native-inputs `(("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((out (assoc-ref %outputs "out"))
              (name "bootstrap-native")
              (targetdir (string-append out "/share/web/" name))
              )
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source")))
               (and
                    ; (mkdir-p targetdir)
                    (copy-recursively source targetdir)
                    ))))))
    (home-page "https://github.com/thednp/bootstrap.native")
    (synopsis "Bootstrap minimal")
    (description "Bootstrap native does not use jquery.")
    (license license:expat))))

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
