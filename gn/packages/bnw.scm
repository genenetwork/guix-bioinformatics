(define-module (gn packages bnw)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gn packages graphviz)
  #:use-module (gn packages maths)
  #:use-module (gnu packages python))

(define-public bnw
  (let ((commit "eb6b002b924694808384f1a8d7c6d1121806ae04")
        (revision "1"))
    (package
      (name "bnw")
      (version (git-version "1.22" revision commit))
      (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/ziejd2/BNW.git")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "10qwykp2zcyxih6a52icvy30ps69qk4v3jgirmdpw1l15zi4p2wq"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (let* ((out      (assoc-ref %outputs "out"))
                (source   (assoc-ref %build-inputs "source"))
                (bash     (assoc-ref %build-inputs "bash"))
                (graphviz (assoc-ref %build-inputs "graphviz"))
                (octave   (assoc-ref %build-inputs "octave"))
                (python   (assoc-ref %build-inputs "python")))
           (begin
             (use-modules (guix build utils))
             (copy-recursively source out)
             (for-each (lambda (file)
               (patch-shebang file
                              (list
                                (string-append bash "/bin")
                                (string-append octave "/bin")
                                (string-append python "/bin")
                                )))
               (find-files out ".*"))
             ;(with-directory-excursion out
             ;  (substitute* '("home.php"
             ;                 (find-files "sourcecodes" ".php")
             ;                 (find-files "sourcecodes/run_scripts" ".*"))
             ;    (("/usr/bin/dot") (string-append graphviz "/bin/dot"))))
             ))))
      (native-inputs `(("source" ,source)))
      (inputs
       `(("bash" ,bash-minimal)
         ("graphviz" ,graphviz-2.26)
         ("octave" ,octave-3.4.3)
         ("python" ,python-2)))
      (home-page "http://compbio.uthsc.edu/BNW/")
      (synopsis "Bayesian Network Webserver")
      (description "This contains the code for the @dfn{Bayesian Network Webserver} (BNW).")
      (license (list license:gpl2
                     license:gpl2+
                     license:lgpl2.1+)))))
