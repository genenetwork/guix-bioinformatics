(define-module (gn packages helm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public helm
  (package
    (name "helm")
    (version "2.15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.helm.sh/helm-v"
                                  version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "04957hdns5fkwpv1ln470iv2gbapzbrizglvf8jdicyl1f9dplm9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out     (assoc-ref %outputs "out"))
                (bin     (string-append out "/bin"))
                (target  (string-append bin "/helm"))
                (gzip    (assoc-ref %build-inputs "gzip"))
                (tar     (assoc-ref %build-inputs "tar"))
                (source  (assoc-ref %build-inputs "source")))
           (setenv "PATH" (string-append gzip "/bin"))
           (invoke (string-append tar "/bin/tar") "xvf" source)
           (chdir "linux-amd64")
           (chmod "helm" #o555)
           (install-file "helm" bin)
           (install-file "tiller" bin)
           (install-file "LICENSE" (string-append out "/share/doc/"
                                                  ,name "-" ,version)))
         #t)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar"  ,tar)))
    (home-page "https://helm.sh/")
    (synopsis "Kubernetes Package Manager")
    (description "Helm is a tool for managing Kubernetes charts.  Charts are
packages of pre-configured Kubernetes resources.
Use Helm to:
@enumerate
@item Find and use popular software packaged as Helm charts to run in Kubernetes
@item Share your own applications as Helm charts
@item Create reproducible builds of your Kubernetes applications
@item Intelligently manage your Kubernetes manifest files
@item Manage releases of Helm packages
@end enumerate")
    (supported-systems '("x86_64-linux"))
    (license license:asl2.0)))
