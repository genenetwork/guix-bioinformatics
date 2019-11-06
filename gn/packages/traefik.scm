(define-module (gn packages traefik)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public traefik
  (package
    (name "traefik")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/containous/traefik/"
                                  "releases/download/v" version
                                  "/traefik_v" version "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "0i0c2d647ks7x3p8l46rslrmnasdk88a6caq1c6diy3ngdcwik23"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out     (assoc-ref %outputs "out"))
                (bin     (string-append out "/bin"))
                (target  (string-append bin "/traefik"))
                (gzip    (assoc-ref %build-inputs "gzip"))
                (tar     (assoc-ref %build-inputs "tar"))
                (source  (assoc-ref %build-inputs "source")))
           (setenv "PATH" (string-append gzip "/bin"))
           (invoke (string-append tar "/bin/tar") "xvf" source)
           (install-file "traefik" bin)
           (install-file "LICENSE.md" (string-append out "/share/doc/"
                                                     ,name "-" ,version)))
         #t)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar"  ,tar)))
    (home-page "https://traefik.io/")
    (synopsis "Cloud Native Edge Router")
    (description "Traefik (pronounced traffic) is a modern HTTP reverse proxy
and load balancer that makes deploying microservices easy.  Traefik integrates
with your existing infrastructure components (Docker, Swarm mode, Kubernetes,
Marathon, Consul, Etcd, Rancher, Amazon ECS, ...) and configures itself
automatically and dynamically.  Pointing Traefik at your orchestrator should be
the only configuration step you need.")
    (supported-systems '("x86_64-linux"))
    (license license:expat)))
