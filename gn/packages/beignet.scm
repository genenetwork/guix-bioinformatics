;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
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

(define-module (gn packages beignet)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages web)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages opencl)
  #:use-module (gn packages ocl-headers)
  #:use-module (gn packages ocl-icd))

(define-public beignet
  (package
   (name "beignet")
   (version "1.1.1")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://01.org/sites/default/files/beignet-"
                                 version "-source.tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "068i5srqpncfw0kklxdyzxcm5w56pi91jp7pkv6cglzvnjgcdx4v"))))
    (inputs `(("autoconf" ,autoconf)
             ("llvm" ,llvm-3.5)
             ("libpthread-stubs", libpthread-stubs)
             ("clang" ,clang-3.5)
             ("libdrm" ,libdrm)
             ("libtool" ,libtool)
             ("libsm" ,libsm)
             ("libxfixes" ,libxfixes)
             ("libxext" ,libxext)
             ("libedit" ,libedit)
             ("xextproto" ,xextproto)
             ("python" ,python-2)
             ("opencl-headers" ,opencl-headers)
             ("glu" ,glu)
             ("zlib" ,zlib)
             ("pkg-config" ,pkg-config)
             ("freeglut" ,freeglut)
             ("mesa-utils" ,mesa-utils)
             ("ncurses" ,ncurses)
             ("ocl-icd" ,ocl-icd)))
    (build-system cmake-build-system)
    (arguments `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release" "-DCOMPILER=CLANG") #:tests? #f))
    (home-page "https://forge.imag.fr/projects/ocl-icd/")
    (synopsis "Intel's OpenCL framework")
    (description "Intel's OpenCL framework that works with Intel IvyBridge GPUs and above")
    (license license:gpl2)))
