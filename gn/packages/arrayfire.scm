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

(define-module (gnu packages arrayfire)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix cmake-build-system)
  #:use-module (guix licenses)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux))

(define-public arrayfire
  (package
    (name "arrayfire")
    (version "v3.3.alpha")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/arrayfire/arrayfire/archive/" version 
                                  ".tar.gz"))
              (sha256
               (base32
                "0rla0mi5wby8bkpzrj063y6js3d4dlfl3qwfvm8m8skfc21dz52p"))))
    (build-system cmake-build-system)
    (arguments `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release -DBUILD_OPENCL=ON")))
    (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("doxygen" ,doxygen)
        ("gawk" ,gawk)
        ("gettext" ,gnu-gettext)
        ("icedtea7" ,icedtea7) 
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)
        ("swig" ,swig)
        ("which" ,which)
        ("yasm" ,yasm)))
    (inputs
     `(("boost" ,boost)
       ("bzip2" ,bzip2)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glew" ,glew)
       ("gnutls" ,gnutls)
       ("gperf" ,gperf)
       ("libcap" ,libcap)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libssh" ,libssh)
       ("libtiff" ,libtiff)
       ("libxml2" ,libxml2)
       ("libxmu" ,libxmu)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxt" ,libxt)
       ("libyajl" ,libyajl)
       ("lzo" ,lzo)
       ("mesa-utils" ,mesa-utils)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("python" ,python-2)
       ("tinyxml" ,tinyxml)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (synopsis "ArrayFire: a general purpose GPU library. https://arrayfire.com")
    (description "ArrayFire is a high performance software library for parallel computing with an easy-to-use API. Its array based function set makes parallel programming simple.")
    (home-page "http://arrayfire.com/")
    (license gpl3+)))

(define-public glfw
  (package
    (name "glfw")
    (version "3.0.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/glfw/glfw/archive/"
                                 version ".zip"))
             (file-name (string-append name "-" version ".zip"))
             (sha256
              (base32
               "1g0jm80cakk60477zz9z1mpsznxaadsfm318yiigf6kackrkqfqg"))))
    (arguments `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("cmake" ,cmake)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs
     `(("bzip2" ,bzip2)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ("enca" ,enca)
       ("eudev" ,eudev)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glew" ,glew)
       ("gnutls" ,gnutls)
       ("gperf" ,gperf)
       ("libcap" ,libcap)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libssh" ,libssh)
       ("libtiff" ,libtiff)
       ("libxml2" ,libxml2)
       ("libxmu" ,libxmu)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxt" ,libxt)
       ("lzo" ,lzo)
       ("mesa-utils" ,mesa-utils)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("python" ,python-2)
       ("tinyxml" ,tinyxml)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (home-page "http://www.glfw.org/")
    (synopsis "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events.")
    (description "glfw is an Open Source, multi-platform library for creating windows with OpenGL contexts and receiving input and events. It is easy to integrate into existing applications and does not lay claim to the main loop.
GLFW is written in C and has native support for Windows, OS X and many Unix-like systems using the X Window System, such as Linux and FreeBSD.
GLFW is licensed under the zlib/libpng license.")
    (license license:gpl2+)))
