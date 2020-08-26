(define-module (gn packages databases)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls))

(define-public mysql-5.0
  (package
    (inherit mysql)
    (version "5.0.96")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://downloads.mysql.com/archives/mysql-"
                            (version-major+minor version)
                            "/mysql-" version ".tar.gz"))
        (sha256
         (base32
          "117w87bqj2vqxkiljcwyaxbkj5fygl5570zla0baln2ifwa3i1a3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list
         "--with-unix-socket-path=/var/run/mysqld/mysqld.sock"
         "--with-extra-charsets=all"
         "--enable-thread-safe-client"
         "--without-openssl"
         "--without-docs"
         ;; Use system readline.
         "--without-readline"
         "--without-libedit")
       ;; TODO: Enable tests.
       ;#:test-target "test-force"
       ;#:test-target "test-bt"
       #:make-flags '("CXXFLAGS=-Wno-narrowing -fpermissive")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (setenv "PERL5LIB" (string-append
                                  (getcwd) "/mqsql-test"
                                  ":" (getenv "PERL5LIB")))
             #t))
         (add-after 'install 'clean-install-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively (string-append out "/mysql-test"))
               (delete-file-recursively (string-append out "/sql-bench"))
               (for-each delete-file
                         (find-files (string-append out "/bin") "^mysqltest"))
               #t))))))
    (inputs
     `(("ncurses" ,ncurses)
       ("perl" ,perl)
       ("procps" ,procps)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (native-inputs `())))

(define-public mysql-5.5
  (package
    (inherit mysql)
    (version "5.5.62")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://downloads.mysql.com/archives/mysql-"
                            (version-major+minor version)
                            "/mysql-" version ".tar.gz"))
        (sha256
         (base32
          "1mwrzwk9ap09s430fpdkyhvx5j2syd3xj2hyfzvanjphq4xqbrxi"))))
    (arguments
     `(#:configure-flags
       '("-DBUILD_CONFIG=mysql_release"
         "-DWITH_SSL=system"
         "-DWITH_ZLIB=system"
         "-DWITH_READLINE=OFF"
         "-DWITH_LIBEDIT=OFF"
         "-DDEFAULT_CHARSET=utf8"
         "-DDEFAULT_COLLATION=utf8_general_ci"
         "-DMYSQL_DATADIR=/var/lib/mysql"
         "-DMYSQL_UNIX_ADDR=/run/mysqld/mysqld.sock"
         "-DINSTALL_INFODIR=share/mysql/docs"
         "-DINSTALL_MANDIR=share/man"
         "-DINSTALL_PLUGINDIR=lib/mysql/plugin"
         "-DINSTALL_SCRIPTDIR=bin"
         "-DINSTALL_INCLUDEDIR=include/mysql"
         "-DINSTALL_DOCREADMEDIR=share/mysql/docs"
         "-DINSTALL_SUPPORTFILESDIR=share/mysql"
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_DOCDIR=share/mysql/docs"
         "-DINSTALL_SHAREDIR=share/mysql"
         ;; Get rid of test data.
         "-DINSTALL_MYSQLTESTDIR="
         "-DINSTALL_SQLBENCHDIR=")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'clean-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively (string-append out "/data"))
               (for-each delete-file
                         (find-files (string-append out "/bin") "_embedded$"))
               #t))))))
    (inputs
     `(("libaio" ,libaio)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl-1.0)
       ("readline" ,readline)
       ("zlib" ,zlib)))))
