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

(define-module (gn packages cl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages zip)  
  #:use-module (gnu packages linux))

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
        ("glew" ,glew)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs `(("boost" ,boost)
       ("glfw" ,glfw)
       ("compute" ,compute)
       ("clblas" ,clblas)
       ("clfft" ,clfft)
       ("atlas" ,atlas)
       ("dbus" ,dbus)
       ("opencl-headers" ,opencl-headers)
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
       ("openblas" ,openblas)
       ("glu" ,glu)))
    (build-system cmake-build-system)
    (arguments 
     `(#:configure-flags '("-DBUILD_OPENCL=ON" "-DBUILD_CUDA=OFF" "-DBUILD_GRAPHICS=OFF" "-DUSE_SYSTEM_BOOST_COMPUTE=ON" "-DUSE_SYSTEM_CLBLAS=ON" "-DUSE_SYSTEM_CLFFT=ON" "-DBUILD_TEST=ON") 
       #:tests? #t))     
    (synopsis "ArrayFire: a general purpose GPU library. https://arrayfire.com")
    (description "ArrayFire is a high performance software library for parallel computing with an easy-to-use API. Its array based function set makes parallel programming simple.")
    (home-page "http://arrayfire.com/")
    (license (list license:gpl2 
                   license:gpl2+ 
                   license:gpl3
                   license:bsd-3 
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
    (license (list license:gpl2
                   license:zlib))))
                       
(define-public clblas
  (package
    (name "clblas")
    (version "2.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/clMathLibraries/clBLAS/archive/v"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0adlb02lqzrklfybhnv4n0p37mvkvdi3vqiwa05x2mv05ywnr93j"))))
    (build-system cmake-build-system)    
    (arguments `(#:tests? #f 
                 #:configure-flags '("../clBLAS-2.10/src" "-DBUILD_SHARED_LIBS=ON" "-DCMAKE_BUILD_TYPE=Release" "-DBUILD_TEST=OFF"))) ;;tlpgen requires network access. Disable it.
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
    (license (list license:gpl2
                   license:asl2.0))))
                       
(define-public clfft
  (package
    (name "clfft")
    (version "2.10.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/clMathLibraries/clFFT/archive/v"
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
    (license (list license:gpl2
                   license:asl2.0))))
                       
(define-public compute
  (package
    (name "compute")
    (version "0.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/boostorg/compute/archive/v"
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
       ("randrproto" ,randrproto)
       ("libxrandr" ,libxrandr)
       ("xineramaproto" ,xineramaproto)
       ("libxinerama" ,libxinerama)
       ("libxcursor" ,libxcursor)
       ("python" ,python-2)))       
    (home-page "http://boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries,BoostCompute")
    (description "Peer-reviewed portable C++ source libraries,BoostCompute")
    (license (list license:x11-style
                   license:boost1.0))))
                                              
(define-public opencl-headers
(let ((commit "c1770dc"))
  (package
    (name "opencl-headers")
    (version (string-append "2.1-" commit ))
    (source (origin
              (method git-fetch)
              (uri (git-reference
              (url "https://github.com/KhronosGroup/OpenCL-Headers.git")
              (commit commit)))
              (file-name (string-append name "-" commit))
              (sha256
               (base32
                "0m9fkblqja0686i2jjqiszvq3df95gp01a2674xknlmkd6525rck"))))
    (propagated-inputs '())
    (inputs '())
    (native-inputs '())
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (copy-recursively "." (string-append
                                                 (assoc-ref outputs "out")
                                                 "/include/CL")))))))
    (synopsis "The Khronos OpenCL headers")
    (description "This package provides the Khronos OpenCL headers")
    (home-page "https://www.khronos.org/registry/cl/")
    (license (list license:gpl2)))))
    
(define-public pocl
(let ((commit "2fa2834"))
(package
    (name "pocl")
    (version (string-append "0.13-" commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
              (url "https://github.com/pocl/pocl.git")
              (commit commit)))
              (file-name (string-append name "-" commit))
              (sha256
               (base32
                "00b2f17rgddkk0ssn2xvbrgr80i2ycfh4g81ph8cm5lyaby7wl7a"))))
    (native-inputs `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("gawk" ,gawk)
        ("glew" ,glew)
        ("libtool" ,libtool)
        ("pkg-config" ,pkg-config)))
    (inputs `(("boost" ,boost)
       ("glfw" ,glfw);;compute package deprecated
       ("atlas" ,atlas)
       ("boost" ,boost)
       ("opencl-headers" ,opencl-headers)
       ("ocl-icd" ,ocl-icd)
       ("enca" ,enca)
       ("clang" ,clang-3.8)
       ("eudev" ,eudev)
       ("glib" ,glib)
       ("hwloc" ,hwloc)
       ("lapack" ,lapack)
       ("scalapack" ,scalapack)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg)
       ("libltdl" ,libltdl)
       ("libtiff" ,libtiff)
       ("libyajl" ,libyajl)
       ("llvm" ,llvm-3.8)
       ("mesa-utils" ,mesa-utils)
       ("python" ,python-2)
       ("perl" ,perl)
       ("freeimage" ,freeimage)
       ("freeglut" ,freeglut)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("fftw-openmpi" ,fftw-openmpi)
       ("glew" ,glew)
       ("openblas" ,openblas)
       ("glu" ,glu)))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                    (add-after 'unpack `bootstrap
                      (lambda _
                        (zero? (system* "autoreconf" "-vfi")))))))   
    (synopsis "pocl: A portable OpenCL implementation")
    (description "pocl is a MIT-licensed open source implementation of the OpenCL standard")
    (home-page "http://portablecl.org/")
    (license (list license:gpl2 
                   license:gpl2+ 
                   license:gpl3
                   license:bsd-3 
                   license:gpl3+
                   license:x11)))))
                   
(define-public ocl-icd
  (package
   (name "ocl-icd")
   (version "2.2.9")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://forge.imag.fr/frs/download.php/716/ocl-icd-"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "1rgaixwnxmrq2aq4kcdvs0yx7i6krakarya9vqs7qwsv5hzc32hc"))))
    (inputs `(("zip" ,zip)
             ("autoconf" ,autoconf)
             ("automake" ,automake)
             ("ruby" ,ruby)
             ("libtool" ,libtool)
             ("opencl-headers" ,opencl-headers)
             ("libgcrypt" ,libgcrypt)))                                              
    (build-system gnu-build-system)
     (arguments
     '(#:phases (modify-phases %standard-phases
                    (add-after 'unpack `bootstrap
                      (lambda _
                        (zero? (system* "autoreconf" "-vfi")))))))    
    (home-page "https://forge.imag.fr/projects/ocl-icd/")
    (synopsis "OpenCL implementations are provided as ICD (Installable Client Driver).")
    (description "OpenCL implementations are provided as ICD (Installable Client Driver).
    An OpenCL program can use several ICD thanks to the use of an ICD Loader as provided by this project.
    This free ICD Loader can load any (free or non free) ICD")
    (license (list license:gpl2 
                   license:ruby))))
                   
(define-public llvm
  (package
    (name "llvm")
    (version "3.6.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://llvm.org/releases/"
                          version "/llvm-" version ".src.tar.xz"))
      (sha256
       (base32
        "153vcvj8gvgwakzr4j0kndc0b7wn91c2g1vy2vg24s6spxcc23gn"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)))
    (arguments
     `(#:configure-flags '("-DCMAKE_SKIP_BUILD_RPATH=FALSE"
                           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE")))
    (home-page "http://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
    (license (list license:ncsa))))

(define (clang-runtime-from-llvm llvm hash)
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/compiler-rt-" version ".src.tar.xz"))
       (sha256 (base32 hash))))
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("llvm" ,llvm)))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:tests? #f))

    (home-page "http://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license (list license:ncsa))

    ;; <http://compiler-rt.llvm.org/> doesn't list MIPS as supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define (clang-from-llvm llvm clang-runtime hash)
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/cfe-" version ".src.tar.xz"))
       (sha256 (base32 hash))))
    ;; Using cmake allows us to treat llvm as an external library.  There
    ;; doesn't seem to be any way to do this with clang's autotools-based
    ;; build system.
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ("gcc-lib" ,gcc "lib")
       ,@(package-inputs llvm)))
    (propagated-inputs
     `(("llvm" ,llvm)
       ("clang-runtime" ,clang-runtime)))
    (arguments
     `(#:configure-flags
       (list "-DCLANG_INCLUDE_TESTS=True"

             ;; Find libgcc_s, crtbegin.o, and crtend.o.
             (string-append "-DGCC_INSTALL_PREFIX="
                            (assoc-ref %build-inputs "gcc-lib"))

             ;; Use a sane default include directory.
             (string-append "-DC_INCLUDE_DIRS="
                            (assoc-ref %build-inputs "libc")
                            "/include"))

       ;; Don't use '-g' during the build to save space.
       #:build-type "Release"

       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'set-glibc-file-names
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((libc (assoc-ref inputs "libc"))
                           (compiler-rt (assoc-ref inputs "clang-runtime")))
                       (substitute* "lib/Driver/Tools.cpp"
                         ;; Patch the 'getLinuxDynamicLinker' function to that
                         ;; it uses the right dynamic linker file name.
                         (("/lib64/ld-linux-x86-64.so.2")
                          (string-append libc
                                         ,(glibc-dynamic-linker)))

                         ;; Link to libclang_rt files from clang-runtime.
                         (("TC\\.getDriver\\(\\)\\.ResourceDir")
                          (string-append "\"" compiler-rt "\"")))))))))

    ;; Clang supports the same environment variables as GCC.
    (native-search-paths
     (list (search-path-specification
            (variable "CPATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))

    (home-page "http://clang.llvm.org")
    (synopsis "C language family frontend for LLVM")
    (description
     "Clang is a compiler front end for the C, C++, Objective-C and
Objective-C++ programming languages.  It uses LLVM as its back end.  The Clang
project includes the Clang front end, the Clang static analyzer, and several
code analysis tools.")
    (license (list license:ncsa))))

(define-public clang-runtime
  (clang-runtime-from-llvm
   llvm
   "11qx8d3pbfqjaj2x207pvlvzihbs1z2xbw4crpz7aid6h1yz6bqg"))

(define-public clang
  (clang-from-llvm llvm clang-runtime
                   "1wwr8s6lzr324hv4s1k6na4j5zv6n9kdhi14s4kb9b13d93814df"))
    
(define-public llvm-3.8
  (package (inherit llvm)
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/llvm-" version ".src.tar.xz"))
       (sha256
        (base32
         "0ikfq0gxac8xpvxj23l4hk8f12ydx48fljgrz1gl9xp0ks704nsm"))))
    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)
       ("libffi" ,libffi)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags '("-DCMAKE_SKIP_BUILD_RPATH=FALSE"
                           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
                           "-DLLVM_ENABLE_PIC=ON"
                           ;"-DLLVM_ENABLE_RTTI=ON"
                           "-DLLVM_ENABLE_WERROR=OFF"
                           ;;"-DLLVM_REQUIRES_RTTI=ON"
                           )))))

(define-public clang-runtime-3.8
  (clang-runtime-from-llvm
   llvm-3.8
   "1c2nkp9563873ffz22qmhc0wakgj428pch8rmhym8agjamz3ily8"))

(define-public clang-3.8
  (clang-from-llvm llvm-3.8 clang-runtime-3.8
                   "1ybcac8hlr9vl3wg8s4v6cp0c0qgqnwprsv85lihbkq3vqv94504"))
