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
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages zip))

(define-public ldc-0.17.2 ; guix in main line
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

(define-public ldc-1.1.0-patched ; guix in main line w.o. patch
  (let ((version2 "1.1.0")) ; version for libraries
    (package
      (inherit ldcmain:ldc)
      (name "ldc")
      (version "1.1.0-patched") ; version for ldc
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/ldc-developers/ldc/archive/v"
                      version2 ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "10zkrmx9bcmhfxvgykm3fkjamzc8js96wm032bv0fyil5c9ja2y1"))))
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
         ("phobos-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/phobos/archive/ldc-v"
                   version2 ".tar.gz")) ; older version
             (file-name (string-append "ldc-phobos-" version2 ".tar.gz"))
             (sha256
              (base32
               "0z5v55b9s1ppf0c2ivjq7sbmq688c37c92ihc3qwrbxnqvkkvrlk"))))
         ("druntime-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/druntime/archive/ldc-v"
                   version2 ".tar.gz"))
             (file-name (string-append "ldc-druntime-" version2 ".tar.gz"))
             (patches (search-patches "ldc-druntime-finiTLSRanges.patch"))
             (sha256
              (base32
               "07qvrqj6vgakd6qr4x5f70w6zwkzd1li5x8i1b5ywnds1z5lnfp6"))))
         ("dmd-testsuite-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v"
                   version2 ".tar.gz")) ; version is behind
             (file-name (string-append "dmd-testsuite-" version2 ".tar.gz"))
             (sha256
              (base32
               "12cak7yqmsgjlflx0dp6fwmwb9dac25amgi86n0bb95ard3547wy")))))))))

(define-public ldc ldc-1.1.0-patched)
