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

(define-module (gn packages opencl-icd-loader)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
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
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)  
  #:use-module (gnu packages linux))

(define-public opencl-icd-loader
(let ((commit "bf894eb"))
  (package
    (name "opencl-icd-loader")
    (version (string-append "2.1-" commit ))
    (source (origin
              (method git-fetch)
              (uri (git-reference
              (url "https://github.com/KhronosGroup/OpenCL-ICD-Loader.git")
              (commit commit)))
              (file-name (string-append name "-" commit))
              (sha256
               (base32
                "1sbxdd9vgl3m8j39kwvvk5cflyj7480pq0s307zg1ssidvj98v3g"))))
(native-inputs `(("autoconf" ,autoconf)
                ("automake" ,automake)
                ("libtool" ,libtool)
                ("cmake" ,cmake)
                ("pkg-config" ,pkg-config)))
(inputs        `(("python" ,python-2)
               ("boost" ,boost)
               ("dbus" ,dbus)
               ("clang" ,clang)
               ("clang-runtime" ,clang-runtime)
               ("enca" ,enca)
               ("eudev" ,eudev)
               ("fftw-openmpi" ,fftw-openmpi)
               ("glew" ,glew)
               ("hwloc" ,hwloc)
               ("libcap" ,libcap)
               ("libjpeg" ,libjpeg-turbo)
               ("libltdl" ,libltdl)
               ("libtiff" ,libtiff)
               ("llvm" ,llvm)
               ("mesa-utils" ,mesa-utils)
               ("openmpi" ,openmpi)
               ("perl" ,perl)
               ("randrproto" ,randrproto)
               ("libxrandr" ,libxrandr)
               ("xineramaproto" ,xineramaproto)
               ("libxinerama" ,libxinerama)
               ("libxcursor" ,libxcursor)
               ("fftw-openmpi" ,fftw-openmpi)))
    (build-system cmake-build-system)                
    (synopsis "The Khronos OpenCL ICDs (Installable Client Driver)")
    (description "This package provides the Khronos OpenCL ICDs")
    (home-page "https://www.khronos.org/registry/cl/")
    (license license:gpl2))))
