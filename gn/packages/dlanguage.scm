;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Roel Janssen <roel@gnu.org>
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

(define-module (gn packages dlanguage)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages zip)
  #:use-module (guix git-download))

(define-public ldc
  (package
    (name "ldc")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ldc-developers/ldc/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1jvilxx0rpqmkbja4m69fhd5g09697xq7vyqp2hz4hvxmmmv4j40"))))
    (build-system cmake-build-system)
    (supported-systems '("x86_64-linux" "i686-linux"))
    (arguments `(
      ;;  When #:tests? set to #t only the following tests FAILED:
      ;;  223 - std.path (Failed)       ;; phobos/std/path.d(3083): ~root
      ;;  243 - std.datetime (Failed)   ;; Directory /usr/share/zoneinfo/ does not exist.
      ;;  253 - std.socket (Failed)     ;; No service for epmap
      ;;  See ./build/Testing/Temporary/LastTest.log     
    #:tests? #t
    #:phases
    (modify-phases %standard-phases         
    (add-after 'unpack 'unpack-phobos-source
               (lambda* (#:key source inputs #:allow-other-keys)
                        (begin
                          (with-directory-excursion "runtime/phobos"
                            (copy-file (assoc-ref inputs "phobos-src")
                                       "phobos-src.tar")
                            (zero? (system* "tar" "xvf" "phobos-src.tar" "--strip-components=1")))
    ))) ;; add-after
    (add-after 'unpack 'unpack-druntime-source
               (lambda* (#:key source inputs #:allow-other-keys)
                        (begin
                          (with-directory-excursion "runtime/druntime"
                            (copy-file (assoc-ref inputs "druntime-src")
                                       "druntime-src.tar.gz")
                            (zero? (system* "tar" "xvzf" "druntime-src.tar.gz" "--strip-components=1")))
    ))) ;; add-after
    (add-after 'unpack 'unpack-dmd-testsuite-source
               (lambda* (#:key source inputs #:allow-other-keys)
                        (begin
                          (with-directory-excursion "tests/d2/dmd-testsuite"
                            (copy-file (assoc-ref inputs "dmd-testsuite-src")
                                       "dmd-testsuite-src.tar.gz")
                            (zero? (system* "tar" "xvzf" "dmd-testsuite-src.tar.gz" "--strip-components=1")))
    ))) ;; add-after

    (add-after
     'unpack-phobos-source 'patch-phobos
     (lambda* (#:key source inputs #:allow-other-keys)
       (substitute* "runtime/phobos/std/process.d"
                    (("/bin/sh") (which "sh"))
                    (("echo") (which "echo")))
       (substitute* "runtime/phobos/std/datetime.d"
                    (("/usr/share/zoneinfo/") (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
       #t)) ;; add-after

    (add-after
     'unpack-dmd-testsuite-source 'patch-dmd-testsuite
     (lambda _
       (substitute* "tests/d2/dmd-testsuite/Makefile"
                    (("/bin/bash") (which "bash")))
       #t)) ;; add-after
    
    ) ;; modify-phases
    )) ; arguments

    (inputs
     `( ("libconfig" ,libconfig)
        ("libedit" ,libedit)
        ("tzdata" ,tzdata)))  ;; for tests
    (native-inputs
     `(("llvm" ,llvm)
       ("clang" ,clang)
       ("unzip" ,unzip) ;; for tests
       ("phobos-src"  ;; runtime/phobos
        ,(origin
          (method url-fetch)
          (uri (string-append "https://github.com/ldc-developers/phobos/archive/ldc-v" version ".tar.gz"))
          (sha256
           (base32
            "0sgdj0536c4nb118yiw1f8lqy5d3g3lpg9l99l165lk9xy45l9z4"))
          (patches (list (search-patch "ldc-disable-tests.patch")))))

       ("druntime-src"  ;; runtime/druntime
        ,(origin
          (method url-fetch)
          (uri (string-append "https://github.com/ldc-developers/druntime/archive/ldc-v" version ".tar.gz"))
          (sha256
           (base32
            "0z4mkyddx6c4sy1vqgqvavz55083dsxws681qkh93jh1rpby9yg6"))))
       ("dmd-testsuite-src"  ;; runtime/druntime
        ,(origin
          (method url-fetch)
          (uri (string-append "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v" version ".tar.gz"))
          (sha256
           (base32
            "0yc6miidzgl9k33ygk7xcppmfd6kivqj02cvv4fmkbs3qz4yy3z1"))))
      ))
    (home-page "https://github.com/ldc-developers/ldc")
    (synopsis "LLVM compiler for the D programming language.")
    (description
     "LDC is a compiler for the D programming Language.  It is based on the
latest DMD frontend and uses LLVM as backend.  LLVM provides a fast and modern
backend for high quality code generation.  LDC is released under a BSD license
with exceptions for the DMD frontend and code from GDC.  The development takes
place mostly on x86-32 and x86-64 Linux and that is where LDC works best.")
    (license license:bsd-3)))
