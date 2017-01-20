;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gn packages ldc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages ldc) #:prefix ldcmain:)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages zip))

(define-public ldc-0.17.2
  (package
    (inherit ldcmain:ldc)
    (name "ldc")
    (version "0.17.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ldc-developers/ldc/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0iksl6cvhsiwnlh15b7s9v8f3grxk27jn0vja9n4sad7fvfwmmlc"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((unpack (lambda (source target)
                             (with-directory-excursion target
                               (zero? (system* "tar" "xvf"
                                               (assoc-ref inputs source)
                                               "--strip-components=1"))))))
               (and (unpack "phobos-src" "runtime/phobos")
                    (unpack "druntime-src" "runtime/druntime")
                    (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite")))))
         (add-after 'unpack-submodule-sources 'patch-dmd2
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "dmd2/root/port.c"
               ((" ::isnan") " isnan")
               ((" ::isinf") " isinf")
               (("#undef isnan") "")
               (("#undef isinf") ""))
             #t))
         (add-after 'unpack-submodule-sources 'patch-phobos
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "runtime/phobos/std/process.d"
               (("/bin/sh") (which "sh"))
               (("echo") (which "echo")))
             (substitute* "runtime/phobos/std/datetime.d"
               (("/usr/share/zoneinfo/")
                (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
             (substitute* "tests/d2/dmd-testsuite/Makefile"
               (("/bin/bash") (which "bash")))
             #t)))
       #:tests? #f))
    (native-inputs
     `(("llvm" ,llvm-3.7)
       ("clang" ,clang-3.7)
       ("zlib" ,zlib)
       ("unzip" ,unzip)
       ("phobos-src"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/ldc-developers/phobos/archive/ldc-v"
                 version ".tar.gz"))
           (file-name (string-append "ldc-phobos-" version ".tar.gz"))
           (sha256
            (base32
             "07hh3ic3r755mq9hn9gfr0wlc5y8cr91xz2ydb6gqy4zy8jgp5s9"))))
       ("druntime-src"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/ldc-developers/druntime/archive/ldc-v"
                 version ".tar.gz"))
           (file-name (string-append "ldc-druntime-" version ".tar.gz"))
           (sha256
            (base32
             "1m1dhday9dl3s04njmd29z7ism2xn2ksb9qlrwzykdgz27b3dk6x"))))
       ("dmd-testsuite-src"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v"
                 version ".tar.gz"))
           (file-name (string-append "testsuite-" version ".tar.gz"))
           (sha256
            (base32
             "0n7gvalxwfmia4gag53r9qhcnk2cqrw3n4icj1yri0zkgc27pm60"))))))))

(define-public ldc-1.1.0-beta6
  (let ((version2 "1.1.0-beta4") ; libraries have not been updated
        (commit "a26bfc1223dddf87af882c53c3e305a905252ab2"))
    (package
      (inherit ldcmain:ldc)
      (name "ldc")
      (version "1.1.0-beta6")
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/ldc-developers/ldc.git")
              (commit commit)))
        (file-name (string-append name "-" version "-" (string-take commit 9)))
        (sha256
         (base32
          "1m3fx0v7dsww253z79kkxjk1yjcmixhhlz7vm0aa2670x4hnw0l5"))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                                 (assoc-ref inputs source)
                                                 "--strip-components=1"))))))
                 (and (unpack "phobos-src" "runtime/phobos")
                      (unpack "druntime-src" "runtime/druntime")
                      (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite")))))
           (add-after 'unpack-submodule-sources 'patch-phobos
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "runtime/phobos/std/process.d"
                 (("/bin/sh") (which "sh"))
                 (("echo") (which "echo")))
               (substitute* "runtime/phobos/std/datetime.d"
                 (("/usr/share/zoneinfo/")
                  (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                 (("/bin/bash") (which "bash")))
               #t)))
         #:tests? #f))
      (native-inputs
       `(("llvm" ,llvm-3.7)
         ("ldc" ,ldc-0.17.2)
         ("zlib" ,zlib)
         ("ncurses" ,ncurses)
         ("phobos-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/phobos/archive/ldc-v"
                   version2 ".tar.gz")) ; older version
             (file-name (string-append "ldc-phobos-" version2 ".tar.gz"))
             (sha256
              (base32
               "1iwy5rs0rqkicj1zfsa5yqvk8ard99bfr8g69qmhlbzb98q0kpks"))))
         ("druntime-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/druntime/archive/ldc-v"
                   version2 ".tar.gz"))
             (file-name (string-append "ldc-druntime-" version2 ".tar.gz"))
             (sha256
              (base32
               "1qsiw5lz1pr8ms9myjf8b94nqi7f1781k226jvxwnhkjg11d0s63"))))
         ("dmd-testsuite-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v"
                   version2 ".tar.gz")) ; version is behind
             (file-name (string-append "dmd-testsuite-" version2 ".tar.gz"))
             (sha256
              (base32
               "0jp54hyi75i9g41rvgmm3zg21yzv57q8dghrhb432rb0n9j15mbp")))))))))

(define-public ldc ldc-1.1.0-beta6)

(define-public rdmd
    (package
      (name "rdmd")
      (version "v2.072.1") ;; remove v when putting in mainline
      (source (origin
                (method url-fetch)
                (uri (string-append
                    "https://github.com/dlang/tools/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ryn4fv1mj8c8d7y4lrr43baahl3z7sk5bryj9kd829paz573va6"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check) ; There is no Makefile, so there's no 'make check'.
           (replace
            'build
            (lambda _
              (zero? (system* "ldc2" "rdmd.d"))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "rdmd" bin)))))))
      (native-inputs
       `(("ldc" ,ldc)))
      (home-page "https://github.com/D-Programming-Language/tools/")
      (synopsis "Specialized equivalent to 'make' for the D language")
      (description
       "rdmd is a companion to the dmd compiler that simplifies the typical
edit-compile-link-run or edit-make-run cycle to a rapid edit-run cycle.  Like
make and other tools, rdmd uses the relative dates of the files involved to
minimize the amount of work necessary.  Unlike make, rdmd tracks dependencies
and freshness without requiring additional information from the user.")
      (license license:boost1.0)))
