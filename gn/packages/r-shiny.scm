(define-module (gn packages r-shiny)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages javascript)
  )

(define-public r-shiny-gn
  (package
    (inherit r-shiny)
    (name "r-shiny-gn")
    (arguments
     `(#:modules ((guix build r-build-system)
                  (guix build minify-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:imported-modules (,@%r-build-system-modules
                           (guix build minify-build-system))
       #:phases
       (modify-phases (@ (guix build r-build-system) %standard-phases)
         (add-after 'unpack 'replace-bundled-minified-JavaScript
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((replace-file (lambda (old new)
                                   (format #t "replacing ~a with ~a\n" old new)
                                   (delete-file old)
                                   (symlink new old))))
               ;; NOTE: Files in ./inst/www/shared/datepicker/js/locales/
               ;; contain just data.  They are not minified code, so we don't
               ;; replace them.
               (with-directory-excursion "inst/www/shared"
                 (replace-file "bootstrap/shim/respond.min.js"
                               (string-append (assoc-ref inputs "js-respond")
                                              "/share/javascript/respond.min.js"))
                 (replace-file "bootstrap/shim/html5shiv.min.js"
                               (string-append (assoc-ref inputs "js-html5shiv")
                                              "/share/javascript/html5shiv.min.js"))
                 (replace-file "json2-min.js"
                               (string-append (assoc-ref inputs "js-json2")
                                              "/share/javascript/json2.min.js"))
                 (replace-file "strftime/strftime-min.js"
                               (string-append (assoc-ref inputs "js-strftime")
                                              "/share/javascript/strftime.min.js"))
                 (replace-file "highlight/highlight.pack.js"
                               (string-append (assoc-ref inputs "js-highlight")
                                              "/share/javascript/highlight.min.js"))
                 (replace-file "datatables/js/jquery.dataTables.min.js"
                               (string-append (assoc-ref inputs "js-datatables")
                                              "/share/javascript/jquery.dataTables.min.js"))
                 (replace-file "selectize/js/selectize.min.js"
                               (string-append (assoc-ref inputs "js-selectize")
                                              "/share/javascript/selectize.min.js"))
                 (replace-file "selectize/js/es5-shim.min.js"
                               (string-append (assoc-ref inputs "js-es5-shim")
                                              "/share/javascript/es5-shim.min.js"))
                 (for-each (match-lambda
                             ((source . target)
                              (delete-file target)
                              (minify source #:target target)))
                           '(("jqueryui/jquery-ui.js" .
                              "jqueryui/jquery-ui.min.js")
                             ("datepicker/js/bootstrap-datepicker.js" .
                              "datepicker/js/bootstrap-datepicker.min.js")
                             ("ionrangeslider/js/ion.rangeSlider.js" .
                              "ionrangeslider/js/ion.rangeSlider.min.js")
                             ("bootstrap/js/bootstrap.js" .
                              "bootstrap/js/bootstrap.min.js")
                             ("shiny.js" .
                              "shiny.min.js")
                             ("jquery.js" .
                              "jquery.min.js")))))
             #t)))))
    (inputs
     `(("js-datatables" ,js-datatables)
       ("js-html5shiv" ,js-html5shiv)
       ("js-json2" ,js-json2)
       ("js-respond" ,js-respond)
       ("js-selectize" ,js-selectize)
       ("js-strftime" ,js-strftime-0.9)
       ("js-highlight" ,js-highlight)
       ("js-es5-shim" ,js-es5-shim-2)))))

(define-public js-strftime-0.9
  (package
    (inherit js-strftime)
    (name "js-strftime")
    (version "0.9.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url"https://github.com/samsonjs/strftime")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g72286afcd2sxdsbm5k868ms6yqhbz0p4yfg33lmzbk9q42v9bs"))))))

(define-public js-es5-shim-2
  (package
    (inherit js-es5-shim)
    (name "js-es5-shim")
    (version "2.0.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/es-shims/es5-shim")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j4f0rba6p9bnqvw0riami01nkj0svl3flm7c7d6xydr2642zz8d"))))))
