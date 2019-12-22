(define-module (gn packages php)
  #:use-module (gnu packages)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages php)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

;; This package is EOL as of January 2019.
(define-public php-5.6
  (package
    (inherit php)
    (name "php")
    (version "5.6.40")
    (home-page "https://secure.php.net/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "distributions/"
                                  "php-" version ".tar.xz"))
              (sha256
               (base32
                "073dcpiaq89b0nv9a5andlkhwxhpr5f2wd65s7xxg59rxqgaas8k"))))
    (arguments
     `(#:configure-flags
       (let-syntax ((with (syntax-rules ()
                            ((_ option input)
                             (string-append option "="
                                            (assoc-ref %build-inputs input))))))
         (list (with "--with-bz2" "bzip2")
               (with "--with-curl" "curl")
               (with "--with-gdbm" "gdbm")
               (with "--with-gettext" "glibc") ; libintl.h
               (with "--with-gmp" "gmp")
               (with "--with-ldap" "openldap")
               (with "--with-ldap-sasl" "cyrus-sasl")
               (with "--with-libxml-dir" "libxml2")
               (with "--with-openssl-dir" "openssl")
               (with "--with-pdo-pgsql" "postgresql")
               (with "--with-pdo-sqlite" "sqlite")
               (with "--with-pgsql" "postgresql")
               ;; PHP’s Pspell extension, while retaining its current name,
               ;; now uses the Aspell library.
               (with "--with-pspell" "aspell")
               (with "--with-readline" "readline")
               (with "--with-sqlite3" "sqlite")
               (with "--with-tidy" "tidy")
               (with "--with-xsl" "libxslt")
               (with "--with-zlib-dir" "zlib")
               ;; We could add "--with-snmp", but it requires netsnmp that
               ;; we don't have a package for. It is used to build the snmp
               ;; extension of php.
               "--with-external-pcre"
               "--with-external-gd"
               "--with-iconv"
               "--with-openssl"
               "--with-mysqli"          ; Required for, e.g. wordpress
               "--with-pdo-mysql"
               "--with-zip"
               "--with-zlib"
               "--enable-bcmath"        ; Required for, e.g. Zabbix frontend
               ;"--enable-calendar"
               "--enable-dba=shared"
               "--enable-exif"
               "--enable-flatfile"
               "--enable-fpm"
               "--enable-ftp"
               "--enable-inifile"
               ;"--enable-intl"
               "--enable-mbstring"
               "--enable-pcntl"
               "--enable-sockets"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-record-build-flags
           (lambda _
             ;; Prevent configure flags from being stored and causing
             ;; unnecessary runtime dependencies.
             (substitute* "scripts/php-config.in"
               (("@CONFIGURE_OPTIONS@") "")
               (("@PHP_LDFLAGS@") ""))
             ;; This file has ISO-8859-1 encoding.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "main/build-defs.h.in"
                 (("@CONFIGURE_COMMAND@") "(omitted)")))
             #t))
         (add-before 'build 'patch-/bin/sh
           (lambda _
             (substitute* '("run-tests.php" "ext/standard/proc_open.c")
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'prepare-tests
           (lambda _
             ;; Some of these files have ISO-8859-1 encoding, whereas others
             ;; use ASCII, so we can't use a "catch-all" find-files here.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* '("ext/mbstring/tests/mb_send_mail02.phpt"
                              "ext/mbstring/tests/mb_send_mail04.phpt"
                              "ext/mbstring/tests/mb_send_mail05.phpt"
                              "ext/mbstring/tests/mb_send_mail06.phpt")
                 (("/bin/cat") (which "cat"))))
             (substitute* '("ext/mbstring/tests/mb_send_mail01.phpt"
                            "ext/mbstring/tests/mb_send_mail03.phpt"
                            "ext/standard/tests/general_functions/bug34794.phpt"
                            "ext/standard/tests/general_functions/bug44667.phpt"
                            "ext/standard/tests/general_functions/proc_open.phpt")
               (("/bin/cat") (which "cat")))

             ;; The encoding of this file is not recognized, so we simply drop it.
             (delete-file "ext/mbstring/tests/mb_send_mail07.phpt")

             (substitute* "ext/standard/tests/streams/bug60602.phpt"
               (("'ls'") (string-append "'" (which "ls") "'")))

             ,@(if (string-prefix? "arm" (or (%current-system)
                                             (%current-target-system)))
                   ;; Drop tests known to fail on armhf.
                   '((for-each delete-file
                              (list
                                "ext/calendar/tests/unixtojd_error1.phpt"
                                ;; arm can be a lot slower, so a time-related test fails
                                "ext/fileinfo/tests/cve-2014-3538-nojit.phpt"
                                "ext/pcre/tests/bug76514.phpt"
                                "ext/pcre/tests/preg_match_error3.phpt"
                                "ext/standard/tests/general_functions/var_export-locale.phpt"
                                "ext/standard/tests/general_functions/var_export_basic1.phpt"
                                "ext/intl/tests/timezone_getErrorCodeMessage_basic.phpt"
                                "ext/intl/tests/timezone_getOffset_error.phpt")))
                   '())

             ;; Drop tests that are known to fail.
             (for-each delete-file
                       '("ext/posix/tests/posix_getgrgid.phpt"    ; Requires /etc/group.
                         "ext/sockets/tests/bug63000.phpt"        ; Fails to detect OS.
                         "ext/sockets/tests/socket_shutdown.phpt" ; Requires DNS.
                         "ext/sockets/tests/socket_send.phpt"     ; Likewise.
                         "ext/sockets/tests/mcast_ipv4_recv.phpt" ; Requires multicast.
                         ;; These needs /etc/services.
                         "ext/standard/tests/general_functions/getservbyname_basic.phpt"
                         "ext/standard/tests/general_functions/getservbyport_basic.phpt"
                         "ext/standard/tests/general_functions/getservbyport_variation1.phpt"
                         ;; And /etc/protocols.
                         "ext/standard/tests/network/getprotobyname_basic.phpt"
                         "ext/standard/tests/network/getprotobynumber_basic.phpt"
                         ;; And exotic locales.
                         "ext/standard/tests/strings/setlocale_basic1.phpt"
                         "ext/standard/tests/strings/setlocale_basic2.phpt"
                         "ext/standard/tests/strings/setlocale_basic3.phpt"
                         "ext/standard/tests/strings/setlocale_variation1.phpt"

                         ;; XXX: These iconv tests have the expected outcome,
                         ;; but with different error messages.
                         ;; Expects "illegal character", instead gets "unknown error (84)".
                         "ext/iconv/tests/bug52211.phpt"
                         ;; Expects "wrong charset", gets unknown error (22).
                         "ext/iconv/tests/iconv_strlen_error2.phpt"
                         "ext/iconv/tests/iconv_substr_error2.phpt"
                         ;; Expects conversion error, gets "error condition Termsig=11".
                         "ext/iconv/tests/iconv_strpos_error2.phpt"
                         "ext/iconv/tests/iconv_strrpos_error2.phpt"
                         ;; Expects "invalid multibyte sequence" but got
                         ;; "unknown error".
                         "ext/iconv/tests/bug76249.phpt"

                         ;; Expects a false boolean, gets empty array from glob().
                         "ext/standard/tests/file/bug41655_1.phpt"
                         "ext/standard/tests/file/glob_variation5.phpt"
                         ;; Expects iconv to detect illegal characters, instead gets
                         ;; "unknown error (84)" and heap corruption(!).
                         "ext/iconv/tests/bug48147.phpt"
                         ;; Expects illegal character ".", gets "=?utf-8?Q?."
                         "ext/iconv/tests/bug51250.phpt"
                         ;; iconv throws "buffer length exceeded" on some string checks.
                         "ext/iconv/tests/iconv_mime_encode.phpt"
                         ;; file_get_contents(): iconv stream filter
                         ;; ("ISO-8859-1"=>"UTF-8") unknown error.
                         "ext/standard/tests/file/bug43008.phpt"
                         ;; Table data not created in sqlite(?).
                         "ext/pdo_sqlite/tests/bug_42589.phpt"

                         ;; These tests are specifically added for php-5.6.40
                         ;;DBA INIFILE handler test
                         "ext/dba/tests/dba_inifile.phpt"
                         ;;Testing ftp_nb_fget resume parameter
                         "ext/ftp/tests/ftp_nb_fget_basic3.phpt"
                         ;;Test iconv_mime_decode() function : usage variations - Pass different data types to charset arg
                         "ext/iconv/tests/iconv_mime_decode_variation3.phpt"
                         ;;Test iconv_strlen() function : usage variations - Pass different data types as $encoding arg
                         "ext/iconv/tests/iconv_strlen_variation2.phpt"
                         ;;Test iconv_strpos() function : usage variations - pass different data types as $charset arg
                         "ext/iconv/tests/iconv_strpos_variation4.phpt"
                         ;;Test iconv_strrpos() function : usage variations - pass different data types as $encoding arg
                         "ext/iconv/tests/iconv_strrpos_variation3.phpt"
                         ;;Bug #52681 (mb_send_mail() appends an extra MIME-Version header)
                         "ext/mbstring/tests/bug52861.phpt"
                         ;;SimpleXML: XPath
                         "ext/simplexml/tests/008.phpt"
                         ;;substr_compare()
                         "ext/standard/tests/strings/substr_compare.phpt"
                         ;;Bug #64230 (XMLReader does not suppress errors)
                         "ext/xmlreader/tests/bug64230.phpt"
                         ;; unable to connect to qa.php.net:80
                         "ext/dom/tests/dom005.phpt"
                         ;;openssl_error_string() tests
                         "ext/openssl/tests/openssl_error_string_basic.phpt"
                         ;;Bug #61948 (CURLOPT_COOKIEFILE '' raises open_basedir restriction)
                         "ext/curl/tests/bug61948.phpt"
                         ;;Bug #64267 (CURLOPT_INFILE doesn't allow reset)
                         "ext/curl/tests/bug64267.phpt"
                         ;;Test curl_error() & curl_errno() function with problematic proxy
                         "ext/curl/tests/curl_basic_010.phpt"
                         ;;Test 10: EXSLT Support
                         "ext/xsl/tests/xslt010.phpt"))

             ;; Skip tests requiring network access.
             (setenv "SKIP_ONLINE_TESTS" "1")
             ;; Without this variable, 'make test' passes regardless of failures.
             (setenv "REPORT_EXIT_STATUS" "1")
             ;; Skip tests requiring I/O facilities that are unavailable in the
             ;; build environment
             (setenv "SKIP_IO_CAPTURE_TESTS" "1")
             #t)))
       #:test-target "test"))
    (inputs
     `(,@(fold alist-delete (package-inputs php)
               '("openssl" "pcre"))
       ("libxml2" ,libxml2)
       ("openssl" ,openssl-1.0)
       ("pcre" ,pcre)))))
