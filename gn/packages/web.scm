(define-module (gn packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages fonts)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial))

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
