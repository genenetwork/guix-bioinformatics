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

(define-module (gn packages pocl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages web)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)  
  #:use-module (gnu packages linux))

(define-public pocl
(let ((commit "a6f377a"))
  (package
    (name "pocl")
    (version (string-append "v09rc-" commit ))
    (source (origin
              (method git-fetch)
              (uri (git-reference
              (url "https://github.com/pocl/pocl.git")
              (commit commit)))
              (file-name (string-append name "-" commit))
              (sha256
               (base32
                "0b1y8c2y0xx5sqfpkkvgmp02czgmq5immypgm4hhpmp512hcj38j"))))
(native-inputs `(("autoconf" ,autoconf)
                ("automake" ,automake)
                ("libtool" ,libtool)
                ("cmake" ,cmake)
                ("pkg-config" ,pkg-config)))
(inputs        `(("python" ,python-2)
               ("boost" ,boost)
               ("dbus" ,dbus)
               ("clang" ,clang)
               ("enca" ,enca)
               ("eudev" ,eudev)
               ("fftw-openmpi" ,fftw-openmpi)
               ("glew" ,glew)
               ("hwloc" ,hwloc)
               ("libcap" ,libcap)
               ("libjpeg" ,libjpeg)
               ("libltdl" ,libltdl)
               ("libtiff" ,libtiff)
               ("llvm" ,llvm)
               ("mesa-utils" ,mesa-utils)
               ("openmpi" ,openmpi)
               ("randrproto" ,randrproto)
               ("libxrandr" ,libxrandr)
               ("xineramaproto" ,xineramaproto)
               ("libxinerama" ,libxinerama)
               ("libxcursor" ,libxcursor)
               ("fftw-openmpi" ,fftw-openmpi)))
    (build-system cmake-build-system)
    (arguments 
     `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release" "-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))                
    (synopsis "pocl: Portable Computing Language (pocl) aims to become a MIT-licensed
     open source implementation of the OpenCL standard which can be easily adapted for
     new targets and devices, both for homogeneous CPU and heterogenous GPUs/accelerators.")
    (description "Portable Computing Language (pocl) aims to become a MIT-licensed
     open source implementation of the OpenCL standard which can be easily adapted for
     new targets and devices, both for homogeneous CPU and heterogenous GPUs/accelerators.")
    (home-page "http://portablecl.org/")
    (license license:gpl2))))
