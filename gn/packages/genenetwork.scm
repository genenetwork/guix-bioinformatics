;; Bioinformatics module

(define-module (gn packages genenetwork)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  ;; #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ldc)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages bootstrap)
  #:use-module (gn packages bioinformatics)
  #:use-module (gn packages python)
  #:use-module (gn packages statistics)
  #:use-module (srfi srfi-1))

(define-public my-deploy
  (package
    (name "my-deploy")
    (version "0.0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (let* ((out  (assoc-ref %outputs "out"))
              (bash (assoc-ref %build-inputs "bash"))
              (foo  (string-append out "/foo")))
         (begin
           (use-modules (guix build utils))
           (mkdir out)
           (call-with-output-file foo
             (lambda (p)
               (format p
                       "#!~a~%echo \"${GUIX_FOO} ${GUIX_BAR}\"~%"
                       bash)))
           (chmod foo #o777)
           ;; wrap-program uses `which' to find bash for the wrapper
           ;; shebang, but it can't know about the bootstrap bash in
           ;; the store, since it's not named "bash".  Help it out a
           ;; bit by providing a symlink it this package's output.
           (symlink bash (string-append out "/bash"))
           (setenv "PATH" out)
           (wrap-program foo `("GUIX_FOO" prefix ("hello")))
           (wrap-program foo `("GUIX_BAR" prefix ("world")))
           #t))))
    (inputs `(("bash" ,(search-bootstrap-binary "bash"
                                                (%current-system)))))

    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))


(define-public genenetwork1
  (let ((commit "d622c803b"))
  (package
    (name "genenetwork1")
    (version (string-append "1.0-" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/genenetwork.git")
                   ;; (url "https://github.com/pjotrp/genenetwork.git")
                   (commit commit)))
             (file-name (string-append name "-" commit)) 
             (sha256
              (base32
               "14fzfcm4vl20mlhxjslfa01i1nmxpk8lbxmfvpq6dyfc22ir62py"))))
    (propagated-inputs `(
              ("python" ,python-2) ;; probably superfluous
              ("r" ,r) 
    ))
    (inputs `(
              ;; http://spring211.uthsc.edu/gn/thirdparty.tbz
              ;; graphviz-2.22.2  htmlgen  json  numarray-1.5.2  piddle  PIL  pp-1.5.7  pyx  pyXLWriter  svg
              ("mysql" ,mysql)
              ("nginx" ,nginx)
              ("graphviz" ,graphviz)
              ; ("python2-jinja2" ,python2-jinja2)
              ; ("python2-sqlalchemy" ,python2-sqlalchemy)
              ; ("python2-setuptools" ,python2-setuptools)
              ; ("python2-scipy" ,python2-scipy)
              ;; looks like python-numarray is not needed
              ; ("python2-numpy" ,python2-numpy)
              ; ("python2-pandas" ,python2-pandas)
              ; ("python2-passlib" ,python2-passlib)
              ; ("python2-redis" ,python2-redis)
              ; ("python2-requests" ,python2-requests)
              ; ("python2-simplejson" ,python2-simplejson)
              ; ("python2-pyyaml" ,python2-pyyaml)
              ;; python-yolk is not needed
              ("python2-pil" ,python2-pil)
              ("python2-numarray" ,python2-numarray)
              ("plink" ,plink) ;; gn1
              ; ("r-qtl" ,r-qtl)
              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://genenetwork.org/")
    (synopsis "Full genenetwork services")
    (description "Genenetwork installation sumo.")
    (license license:agpl3+))))

(define-public genenetwork2
  (let ((commit "9e9475053"))
  (package
    (name "genenetwork2")
    (version (string-append "2.0-" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://github.com/pjotrp/genenetwork2.git")
                   (commit commit)))
             (file-name (string-append name "-" commit)) 
             (sha256
              (base32
               "09hvy9mf4dnmkb8qg49viffzrxk53m2kr4r955m84dxaa5pdrjhd"))))
    (propagated-inputs `(  ;; propagated for development purposes
              ("python" ,python-2) ;; probably superfluous
              ("r" ,r)
              ("redis" ,redis)
              ("mysql" ,mysql)
              ("gemma" ,gemma-git)
              ("pylmm-gn2" ,pylmm-gn2)
              ("plink2" ,plink-ng)
              ("nginx" ,nginx)
              ("python2-flask" ,python2-flask)
              ("python2-htmlgen-gn" ,python2-htmlgen-gn)
              ("python2-jinja2" ,python2-jinja2)
              ("python2-sqlalchemy" ,python2-sqlalchemy)
              ("python2-flask-sqlalchemy" ,python2-flask-sqlalchemy)
              ("python2-setuptools" ,python2-setuptools)
              ("python2-scipy" ,python2-scipy)
              ;; looks like python-numarray is not needed
              ("python2-mysqlclient" ,python2-mysqlclient)
              ("python2-numarray" ,python2-numarray)
              ("python2-numpy" ,python2-numpy)
              ("python2-pandas" ,python2-pandas)
              ("python2-parallel" ,python2-parallel)
              ("python2-passlib" ,python2-passlib)
              ("python2-piddle" ,python2-piddle)
              ("python2-redis" ,python2-redis)
              ("python2-requests" ,python2-requests)
              ("python2-rpy2" ,python2-rpy2)
              ("python2-scipy" ,python2-scipy)
              ("python2-simplejson" ,python2-simplejson)
              ("python2-pyyaml" ,python2-pyyaml)
              ("python2-xlsxwriter" ,python2-xlsxwriter)
              ;; python-yolk is not needed
              ("plink" ,plink) 
              ("qtlreaper" ,qtlreaper) 
              ("r-qtl" ,r-qtl)
              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://genenetwork.org/")
    (synopsis "Full genenetwork services")
    (description "Genenetwork installation sumo.")
    (license license:agpl3+))))

;; ./pre-inst-env guix download http://files.genenetwork.org/raw_database/db_webqtl_s.zip
;; 0sscjh0wml2lx0mb43vf4chg9gpbfi7abpjxb34n3kyny9ll557x

(define-public genenetwork2-database-small
  (let ((md5 "93e745e9c"))
    (package
    (name "genenetwork2-database-small")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri "http://files.genenetwork.org/raw_database/db_webqtl_s.zip")
       (file-name (string-append name "-" md5)) 
       (sha256
        (base32 "0sscjh0wml2lx0mb43vf4chg9gpbfi7abpjxb34n3kyny9ll557x"))))
    (build-system trivial-build-system)
    (native-inputs `(("unzip" ,unzip)
                     ("source" ,source)))

    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip"))
                         )
                   (and (mkdir "db")
                        (zero? (system* unzip source "-d" "db"))
                        (chdir "db"))))))
    (home-page "http://genenetwork.org/")
    (synopsis "Small database to run on genenetwork")
    (description "Genenetwork installation + database.")
    (license license:agpl3+))))

