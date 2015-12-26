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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
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
    ;; #:tests? #f
    #:phases
    (modify-phases %standard-phases         
    (add-after 'unpack 'unpack-phobos-source
               (lambda* (#:key source inputs #:allow-other-keys)
                        (begin
                          (with-directory-excursion "runtime/phobos"
                            (copy-file (assoc-ref inputs "phobos-src")
                                       "phobos-src.tar.gz")
                            (zero? (system* "tar" "xvzf" "phobos-src.tar.gz" "--strip-components=1")))
    ))) ;; add-after
    (add-after 'unpack 'unpack-druntime-source
               (lambda* (#:key source inputs #:allow-other-keys)
                        (begin
                          (with-directory-excursion "runtime/druntime"
                            (copy-file (assoc-ref inputs "druntime-src")
                                       "druntime-src.tar.gz")
                            (zero? (system* "tar" "xvzf" "druntime-src.tar.gz" "--strip-components=1")))
    ))) ;; add-after

    ) ;; modify-phases
    )) ; arguments

    (inputs
     `( ("libconfig" ,libconfig)
        ("libedit" ,libedit)))
    (native-inputs
     `(("llvm" ,llvm)
       ("clang" ,clang)
       ("phobos-src"  ;; runtime/phobos
        ,(origin
          (method url-fetch)
          (uri (string-append "https://github.com/ldc-developers/phobos/archive/ldc-v" version ".tar.gz"))
          (sha256
           (base32
            "0sgdj0536c4nb118yiw1f8lqy5d3g3lpg9l99l165lk9xy45l9z4"))))
       ("druntime-src"  ;; runtime/druntime
        ,(origin
          (method url-fetch)
          (uri (string-append "https://github.com/ldc-developers/druntime/archive/ldc-v" version ".tar.gz"))
          (sha256
           (base32
            "0z4mkyddx6c4sy1vqgqvavz55083dsxws681qkh93jh1rpby9yg6"))))
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
