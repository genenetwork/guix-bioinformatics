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

(define-module (gn packages arrayfire)
  #:use-module ((guix licenses))
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages web)
  #:use-module (gnu packages ldc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)  
  #:use-module (gnu packages linux))

(define-public arrayfire
(let ((commit "9ca1ea0"))
  (package
    (name "arrayfire")
    (version "3.3.1-hotfix")
    (source (origin
              (method git-fetch)
              (uri (git-reference
              (url "https://github.com/arrayfire/arrayfire.git")
              (commit commit)))
              (file-name (string-append name "-" commit))
              (sha256
               (base32
                "067y98a9s78vpz7xsg889xnwfaza7c5gdv97yg9cgrdl0iljvsgp"))))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("gawk" ,gawk)
        ("glew" ,glew)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs `(("boost" ,boost)
       ("glfw" ,glfw)
       ("curl" ,curl)
       ("atlas" ,atlas)
       ("dbus" ,dbus)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("glew" ,glew)
       ("glib" ,glib)
       ("lapack" ,lapack)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libtiff" ,libtiff)
       ("libyajl" ,libyajl)
       ("mesa-utils" ,mesa-utils)
       ("python" ,python-2)
       ("freeimage" ,freeimage)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("fftw-openmpi" ,fftw-openmpi)
       ("glew" ,glew)
       ("glu" ,glu)
       ("openblas" ,openblas)
       ("git" ,git)
       ("cmake" ,cmake)))
    (build-system cmake-build-system)
    (arguments 
     `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release" "-DBUILD_OPENCL=OFF" "-DBUILD_GRAPHICS=OFF") #:tests? #f))     
    (synopsis "ArrayFire: a general purpose GPU library. https://arrayfire.com")
    (description "ArrayFire is a high performance software library for parallel computing with an easy-to-use API. Its array based function set makes parallel programming simple.")
    (home-page "http://arrayfire.com/")
    (license (list gpl2 gpl2+ gpl3 gpl3+)))))

(define-public glfw
  (package
    (name "glfw")
    (version "3.1.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/glfw/glfw/archive/"
                                 version ".tar.gz"))
             (sha256
              (base32
               "08pixv8hd5xsccf7l8cqcijjqaq4k4da8qbp77wggal2fq445ika"))))
    (build-system cmake-build-system)
    (arguments `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON") #:tests? #f))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("libtool" ,libtool)
        ("libpthread-stubs" ,libpthread-stubs)
        ("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)
       ("dbus" ,dbus)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("glew" ,glew)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libtiff" ,libtiff)
       ("mesa-utils" ,mesa-utils)
       ("randrproto" ,randrproto)
       ("libxrandr" ,libxrandr)
       ("xineramaproto" ,xineramaproto)
       ("libxinerama" ,libxinerama)
       ("libxcursor" ,libxcursor)
       ("python" ,python-2)))       
    (home-page "http://www.glfw.org/")
    (synopsis "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events.")
    (description "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events.")
    (license (list gpl2))))

