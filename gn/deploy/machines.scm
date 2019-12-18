; Machine definitions

(define-module (gn deploy machines)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  ;; #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages web)
  #:use-module (gn packages web)
  #:use-module (srfi srfi-1))

; Configure a default version of the nginx web server
(define-public nginx-config
  (let ((commit "e2ac61bfa472f23eb8e0c6863395a79c94a3d68a")
        (revision "1"))
    (package
     (name "nginx-gn-config")
     (version (git-version "0.0.1" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "http://git.genenetwork.org/pjotrp/guix-bioinformatics.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pczs7farkcklvh96q1psjgv86mlwv93c3lzmc1mwp9m299g4qdr"))))
     (build-system trivial-build-system)
     (native-inputs `(("unzip" ,unzip)
                      ("source" ,source)))
     (propagated-inputs `(("nginx" ,nginx)))
     (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let ((target (string-append (assoc-ref %outputs "out")
                                       "/etc/nginx"))
                (nginx-etc (string-append (assoc-ref %build-inputs "nginx")
                                            "/share/nginx/conf")))
            ; (write target)
            (mkdir-p target)
            ; copy original nginx configuration /gnu/store/nginx-ver/share/nginx/conf/*
                                        ; (copy-recursively nginx-etc target)
            (copy-file (string-append nginx-etc "/nginx.conf")
                       (string-append target "/nginx.conf"))
            (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL\n")
            #t))))
     (home-page "http://git.genenetwork.org/pjotrp/guix-bioinformatics")
     (synopsis "Nginx configuration")
     (description "None.")
     (license license:expat))))
