(define-module (gn packages python24)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages python)
  #:use-module (srfi srfi-1))

;; TODO: Check against 'guix lint -c cve python2.4' list:
;; CVE-2019-9740, CVE-2019-9947, CVE-2019-9948, CVE-2018-1060, CVE-2018-1061,
;; CVE-2014-9365, CVE-2012-0845, CVE-2012-1150, CVE-2011-1521, CVE-2011-4940,
;; CVE-2010-3492, CVE-2008-5031, CVE-2008-5983
(define-public python-2.4
  (package
    (inherit python-2)
    (name "python2.4")
    (version "2.4.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.bz2"))
      (sha256
       (base32
        "021y88a4ki07dgq19yhg6zfvmncfiz7h5b2255438i9zmlwl246s"))))
    (outputs '("out"))
    (arguments
      (substitute-keyword-arguments (package-arguments python-2)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'check 'delete-failing-test
              (lambda _
                (delete-file "Lib/test/test_socket.py")
                #t))
            (add-after 'check 'find-netinet-in-h
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((glibc (assoc-ref inputs "libc")))
                  (substitute* (find-files "Lib/plat-generic" ".*")
                    (("/usr/include/netinet/in.h")
                     (string-append glibc "/include/netinet/in.h")))
                  #t)))
            (delete 'move-tk-inter)))))
    ;; Remove the inputs which are not found during building/testing:
    (inputs
     `(,@(fold alist-delete (package-inputs python-2)
               '("bzip2" "gdbm" "tk" "openssl" "zlib"))))
    (native-search-paths
      (list (search-path-specification
              (variable "PYTHONPATH")
              (files '("lib/python2.4/site-packages")))))
    (properties '((cpe-name . "python")))))
