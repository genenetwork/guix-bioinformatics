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
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  ;; #:use-module (gnu packages fftw)
  ;; #:use-module (gnu packages fftw-openmpi) - in algebra
  ;; #:use-module (gnu packages fftwf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages opencl)
  #:use-module (gn packages ocl-headers)
  #:use-module (gn packages ocl-icd))

(define-public arrayfire
(package
    (name "arrayfire")
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://arrayfire.com/arrayfire_source/arrayfire-full-" version
                                  ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "045adww6dqmyz6kkfmq7xawi5v9a894yp5j9pzn6j568gi48pyqc"))))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("gawk" ,gawk)
        ("git" ,git)
        ("glew" ,glew)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs `(("boost" ,boost)
       ("glfw" ,glfw)
       ("compute" ,compute)
       ("curl" ,curl)
       ("clBLAS" ,clBLAS)
       ("clFFT" ,clFFT)
       ("atlas" ,atlas)
       ("dbus" ,dbus)
       ("opencl-headers" ,opencl-headers)
       ("ocl-icd" ,ocl-icd)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("glew" ,glew)
       ("glib" ,glib)
       ("lapack" ,lapack)
       ("scalapack" ,scalapack)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libtiff" ,libtiff)
       ("libyajl" ,libyajl)
       ("mesa-utils" ,mesa-utils)
       ("python" ,python-2)
       ("freeimage" ,freeimage)
       ("freeglut" ,freeglut)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("fftw-openmpi" ,fftw-openmpi)
       ("glew" ,glew)
       ("glu" ,glu)
       ("openblas" ,openblas)
       ("wget" ,wget)
       ("cmake" ,cmake)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_OPENCL=ON" "-DBUILD_CUDA=OFF" "-DBUILD_GRAPHICS=OFF" "-DUSE_SYSTEM_BOOST_COMPUTE=ON" "-DUSE_SYSTEM_CLBLAS=ON" "-DUSE_SYSTEM_CLFFT=ON")
       #:tests? #t))
    (synopsis "ArrayFire: a general purpose GPU library. https://arrayfire.com")
    (description "ArrayFire is a high performance software library for parallel computing with an easy-to-use API. Its array based function set makes parallel programming simple.Now on Guix")
    (home-page "http://arrayfire.com/")
    (license (list license:gpl2
                   license:gpl2+
                   license:gpl3
                   license:gpl3+))))

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
    (arguments `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
                 #:tests? #f))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("git" ,git)
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
    (license (list license:gpl2))))

(define-public clBLAS
  (package
    (name "clBLAS")
    (version "v2.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/clMathLibraries/clBLAS/archive/"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0adlb02lqzrklfybhnv4n0p37mvkvdi3vqiwa05x2mv05ywnr93j"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f
                 #:configure-flags '("../clBLAS-2.10/src" "-DBUILD_SHARED_LIBS=ON" "-DCMAKE_BUILD_TYPE=Release" "-DBUILD_TEST=OFF")))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("gfortran" ,gfortran)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)
       ("dbus" ,dbus)
       ("boost" ,boost)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("fftw-openmpi" ,fftw-openmpi)
       ("glew" ,glew)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libtiff" ,libtiff)
       ("mesa-utils" ,mesa-utils)
       ("openmpi" ,openmpi)
       ("ocl-icd" ,ocl-icd)
       ("opencl-headers" ,opencl-headers)
       ("randrproto" ,randrproto)
       ("libxrandr" ,libxrandr)
       ("xineramaproto" ,xineramaproto)
       ("libxinerama" ,libxinerama)
       ("libxcursor" ,libxcursor)
       ("python" ,python-2)))
    (home-page "http://www.glfw.org/")
    (synopsis "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events.")
    (description "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events.")
    (license (list license:gpl2))))

(define-public clFFT
  (package
    (name "clFFT")
    (version "v2.10.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/clMathLibraries/clFFT/archive/"
                                 version ".tar.gz"))
             (sha256
              (base32
               "19hrk1lf06kch8x9dpbdj0waycn2mldrmj2y4vzi7zn2gdfw6g73"))))
    (build-system cmake-build-system)
    (arguments `(#:configure-flags '("../clFFT-2.10.1/src" "-DBUILD_SHARED_LIBS=ON" "-DCMAKE_BUILD_TYPE=Release") #:tests? #f))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)
       ("dbus" ,dbus)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("fftw-openmpi" ,fftw-openmpi)
       ("glew" ,glew)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libtiff" ,libtiff)
       ("mesa-utils" ,mesa-utils)
       ("openmpi" ,openmpi)
       ("ocl-icd" ,ocl-icd)
       ("opencl-headers" ,opencl-headers)
       ("randrproto" ,randrproto)
       ("libxrandr" ,libxrandr)
       ("xineramaproto" ,xineramaproto)
       ("libxinerama" ,libxinerama)
       ("libxcursor" ,libxcursor)
       ("python" ,python-2)))
    (home-page "http://www.glfw.org/")
    (synopsis "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events.")
    (description "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events.")
    (license (list license:gpl2))))

(define-public compute
  (package
    (name "compute")
    (version "v0.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/boostorg/compute/archive/"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1r16zd1wdnn9gx278mkvr13k3i79hr35v6vj0fn7v3n92ngwxnhd"))))
    (build-system cmake-build-system)
    (arguments `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON" "-DCMAKE_BUILD_TYPE=Release") #:tests? #f))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)
       ("dbus" ,dbus)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("fftw-openmpi" ,fftw-openmpi)
       ("glew" ,glew)
       ("boost" ,boost)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libtiff" ,libtiff)
       ("mesa-utils" ,mesa-utils)
       ("openmpi" ,openmpi)
       ("opencl-headers" ,opencl-headers)
       ("ocl-icd" ,ocl-icd)
       ("randrproto" ,randrproto)
       ("libxrandr" ,libxrandr)
       ("xineramaproto" ,xineramaproto)
       ("libxinerama" ,libxinerama)
       ("libxcursor" ,libxcursor)
       ("python" ,python-2)))
    (home-page "http://boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries,BoostCompute")
    (description "Peer-reviewed portable C++ source libraries,BoostCompute")
    (license (list license:x11-style))))
