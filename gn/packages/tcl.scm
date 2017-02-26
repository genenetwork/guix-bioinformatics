
(define-module (gn packages tcl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix licenses))

(define-public environment-modules ; guix: fix DL and tests
  (package
    (name "environment-modules")
    (version "3.2.10")
    (source
     (origin
      (method url-fetch)
      ;; Would be nice to have a versioned download URL
      (uri "http://sourceforge.net/projects/modules/files/latest/download?source=directory")
      (sha256
       (base32
        "0m9rn1vr9acvc3qh9rdwnxlkzchdrbqajdvpv6hhay27hcmch1gv"))))
    (build-system gnu-build-system)
    (inputs
     `(("tcl" ,tcl)))
    (arguments
     '(#:configure-flags
       (let ((out (assoc-ref %outputs "out"))
             (tcl (assoc-ref %build-inputs "tcl")))
         (list (string-append "--with-tcl=" tcl "/lib")
               (string-append "--with-tclinclude=" tcl "/include")
               (string-append "--exec-prefix=" out)
               (string-append "--disable-versioning")
               (string-append "--mandir=" out "/share/man")
               "CPPFLAGS=-DUSE_INTERP_ERRORLINE"))
       #:tests? #f))

    (home-page "")
    (synopsis "Tool for managing the shell environment")
    (description
     "The Environment Modules system is a tool to help users manage
their Unix or Linux shell environment, by allowing groups of related
environment-variable settings to be made or removed dynamically.")
    (license gpl2)))

