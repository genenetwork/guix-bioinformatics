(define-module (gn packages gitea)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages version-control))

(define-public gitea
  (package
    (name "gitea")
    (version "1.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/go-gitea/gitea/releases"
                                  "/download/v" version
                                  "/gitea-" version "-linux-amd64"))
              (sha256
               (base32
                 "017bf09ym3244wqvqbpdqxmpnb7gs23fqn7h5k1vfcrwamfwc82n"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out     (assoc-ref %outputs "out"))
                (bin     (string-append out "/bin"))
                (target  (string-append bin "/gitea"))
                (git     (assoc-ref %build-inputs "git"))
                (bash    (assoc-ref %build-inputs "bash"))
                (path    (getenv "PATH"))
                (source  (assoc-ref %build-inputs "source")))
           (copy-file source "gitea")
           (chmod "gitea" #o555)
           (install-file "gitea" bin)
           (setenv "PATH" (string-append bash "/bin"))
           (wrap-program target
                         `("PATH" ":" prefix (,(string-append git "/bin")))))
         #t)))
    (native-inputs
     `(("source" ,source)))
    (inputs
     `(("bash" ,bash-minimal)
       ("git" ,git)))
    (home-page "https://gitea.io/")
    (synopsis "Self-hosted git service")
    ;; TODO: Rewrite description
    (description "Gitea is a painless self-hosted Git service.  It is similar
to GitHub, Bitbucket, and GitLab.")
    (supported-systems '("x86_64-linux"))
    (license license:expat)))
