(define-module (gn packages gitea)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages node)
  #:use-module (gnu packages version-control))

(define-public gitea
  (package
    (name "gitea")
    (version "1.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/go-gitea/gitea/releases"
                                  "/download/v" version
                                  "/gitea-src-" version ".tar.gz"))
              (sha256
               (base32
                "05z1pp2lnbr82pw97wy0j0qk2vv1qv9c46df13d03xdfsc3gsm50"))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-build
           (lambda _
             (chdir "src")
             (setenv "TAGS" "bindata sqlite sqlite_unlock_notify")
             #t))
         (replace 'build
           (lambda _
             (invoke "make" "build")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (begin
                 (invoke "make" "test")
                 ;; Gitea requires git with lfs support to run tests.
                 ;(invoke "make" "test-sqlite")
                 (invoke "make" "test-sqlite-migration"))
               #t)))
         (replace 'install
           (lambda _
             (invoke "make" "install")))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/gitea"))
                    (git (assoc-ref inputs "git")))
               (wrap-program bin
                 `("PATH" ":" prefix (,(string-append git "/bin")))))
             #t)))))
    (native-inputs
     `(("node" ,node)))
    (inputs
     `(("git" ,git)))
    (home-page "https://gitea.io/")
    (synopsis "Self-hosted git service")
    ;; TODO: Rewrite description
    (description "Gitea is a painless self-hosted Git service.  It is similar
to GitHub, Bitbucket, and GitLab.")
    (license license:expat)))
