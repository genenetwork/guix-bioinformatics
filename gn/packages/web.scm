(define-module (gn packages web)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
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
   (version "3.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/jquery/jquery/archive/" version ".zip"))
     (file-name (string-append name "-" version))
     (sha256
      (base32 "0a8sq7a52bh3a4slf03yfclwsv4iadlzmha4v6kqfby5sd66si18"))))
   (build-system trivial-build-system)
   (native-inputs `(("source" ,source)
                    ("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (let* ((out (assoc-ref %outputs "out"))
             (name "jquery")
             (targetdir (string-append out "/share/web/" name))
             )
        (begin
          (use-modules (guix build utils))
          (let ((source (assoc-ref %build-inputs "source"))
                   (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
                )
            (and
             (mkdir-p "source")
             (chdir "source")
             (zero? (system* unzip source))
             (mkdir-p targetdir)
             (copy-recursively "jquery-3.2.1/dist" targetdir)

             ; (copy-recursively source targetdir)
             ; (file-copy (string-append out "/dist/jquery.slim.min.j") source)
             ))))))
   (home-page "http://jquery.com/")
   (synopsis "JQuery web framework")
   (description "jQuery.")
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
