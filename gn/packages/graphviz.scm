(define-module (gn packages graphviz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (srfi srfi-1))

(define-public graphviz-2.26
  (package
    (inherit graphviz)
    (name "graphviz")
    (version "2.26.3")
    (outputs '("out"))
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               ;; TODO: Replace with official release
               "mirror://debian/pool/main/g/graphviz/"
               "graphviz_" version ".orig.tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18bzyg17ni0lpcd2g5dhan8fjv3vzkjym38jq8vm42did5p9j47l"))))
    ;; TODO: unbundle libraries?
    (arguments
     `(#:configure-flags '("--enable-swig=no")
       ,@(substitute-keyword-arguments (package-arguments graphviz)
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'move-docs) ; one output
               (delete 'move-guile-bindings))))))
    (inputs
      ;; TODO(?): Add perl, guile@1.8, gtk@2, lua5.1, tcl8.[3-6], rsvg, python-2.4
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("freeglut" ,freeglut)
       ,@(fold alist-delete (package-inputs graphviz)
               '("libjpeg" "guile" "swig"))))
    (license license:cpl1.0)))
