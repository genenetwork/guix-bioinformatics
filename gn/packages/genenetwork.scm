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
              ("python2-pillow" ,python2-pillow)
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
  (let ((commit "a8fcff44d3bd768d02e7ed0c80b84d2516bdad2a"))
  (package
    (name "genenetwork2")
    (version (string-append "2.0-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://github.com/genenetwork/genenetwork2_diet.git")
                   (commit commit)))
             (file-name (string-append name "-" (string-take commit 7)))
             (sha256
              (base32
               "1zs6jgrpwzxmfjz03whnaw8q6h8f53mycl440p058gfn8x7pd618"))))
    (propagated-inputs `(  ;; propagated for development purposes
              ("python" ,python-2) ;; probably superfluous
              ("r" ,r)
              ("r-wgcna" ,r-wgcna)
              ("r-qtl" ,r-qtl)
              ("redis" ,redis)
              ("mysql" ,mysql)
              ("gemma" ,gemma-git)
              ("genenetwork2-files-small" ,genenetwork2-files-small)
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
              ("python2-piddle-gn" ,python2-piddle-gn)
              ("python2-redis" ,python2-redis)
              ("python2-pillow" ,python2-pillow)
              ("python2-requests" ,python2-requests)
              ("python2-rpy2" ,python2-rpy2)
              ("python2-scipy" ,python2-scipy)
              ("python2-simplejson" ,python2-simplejson)
              ("python2-pyyaml" ,python2-pyyaml)
              ("python2-xlsxwriter" ,python2-xlsxwriter)
              ;; python-yolk is not needed
              ("plink" ,plink) 
              ("qtlreaper" ,qtlreaper) 
              ))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
         (modify-phases %standard-phases
           (add-before 'install 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
                      (let* (
                             (datafiles (string-append (assoc-ref inputs "genenetwork2-files-small") "/share/genenetwork2" ))
                             (pylmmcmd (string-append (assoc-ref inputs "pylmm-gn2") "/bin/pylmm_redis"))
                             (plink2cmd (string-append (assoc-ref inputs "plink2") "/bin/plink2"))
                             (gemmacmd (string-append (assoc-ref inputs "gemma") "/bin/gemma"))
                             )
                             
               (substitute* '("etc/default_settings.py")
                            (("^GENENETWORK_FILES =.*") (string-append "GENENETWORK_FILES = \"" datafiles "\"\n" ))
                            (("^PYLMM_COMMAND =.*") (string-append "PYLMM_COMMAND = \"" pylmmcmd "\"\n" ))
                            (("^PLINK_COMMAND =.*") (string-append "PLINK_COMMAND = \"" plink2cmd "\"\n" ))
                            (("^GEMMA_COMMAND =.*") (string-append "GEMMA_COMMAND = \"" gemmacmd "\"\n" ))
                            )
               ))))
       #:tests? #f))   ; no 'setup.py test'
    (home-page "http://genenetwork.org/")
    (synopsis "Full genenetwork services")
    (description "Genenetwork installation sumo.")
    (license license:agpl3+))))

;; ./pre-inst-env guix download http://files.genenetwork.org/raw_database/db_webqtl_s.zip
;; 0sscjh0wml2lx0mb43vf4chg9gpbfi7abpjxb34n3kyny9ll557x

(define-public genenetwork2-files-small
  (let ((pfff "xx"))
    (package
    (name "genenetwork2-files-small")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri "http://files.genenetwork.org/data_files/gn2_data_s-20160303-C9E672ECED1F51B915DE419B5B2C524E.tar.lz4")
       (file-name (string-append name "-" pfff)) 
       (sha256
        (base32 "058ymx3af6abdhdxyxj0i9qfvb6v7j091frjpp6jh4ahks7r23lj"))))
    (build-system trivial-build-system)
    (native-inputs `(("lz4" ,lz4)
                     ("tar" ,tar)
                     ("source" ,source)))

    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((out (assoc-ref %outputs "out"))
              (name "gn2_data_s")
              (tarfn (string-append name ".tar"))
              (targetdir (string-append out "/share/genenetwork2/"))
              )
           (begin
             (use-modules (guix build utils))
             (let ((source (assoc-ref %build-inputs "source"))
                   (lz4unpack (string-append (assoc-ref %build-inputs "lz4") "/bin/lz4"))
                   (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                   )
               (and 
                    (zero? (system* lz4unpack source "-d" tarfn))
                    (zero? (system* tar "xf" tarfn))
                    (mkdir-p targetdir)
                    (copy-recursively name targetdir)
                    ))))))
    (home-page "http://genenetwork.org/")
    (synopsis "Small file archive to run on genenetwork")
    (description "Genenetwork genotype and mapping files.")
    (license license:agpl3+))))

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

