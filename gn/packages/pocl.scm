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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
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
  #:use-module (gnu packages xml)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages gnupg)
  #:use-module ((gnu packages zip) #:prefix gnuzip:)
  #:use-module (gnu packages linux))

(define-public pocl
(let ((commit "7aefc52"))
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
                "03wbcxkx1w5kx0crangsnah0xq218bhb99vglmn00wcjhhw80qim"))))
(native-inputs `(("autoconf" ,autoconf)
                ("automake" ,automake)
                ("libtool" ,libtool)
                ("cmake" ,cmake)
                ("pkg-config" ,pkg-config)))
(inputs        `(("python" ,python-2)
               ("boost" ,boost)
               ("dbus" ,dbus)
               ("clang" ,clang-3.7.1)
               ("clang-runtime" ,clang-runtime-3.7.1)
               ("enca" ,enca)
               ("eudev" ,eudev)
               ("fftw-openmpi" ,fftw-openmpi)
               ("glew" ,glew)
               ("hwloc" ,hwloc)
               ("libcap" ,libcap)
               ("libjpeg" ,libjpeg)
               ("libltdl" ,libltdl)
               ("libtiff" ,libtiff)
               ("llvm" ,llvm-3.7.1)
               ("ocl-icd" ,ocl-icd)
               ("opencl-headers" ,opencl-headers)
               ("mesa-utils" ,mesa-utils)
               ("openmpi" ,openmpi)
               ("perl" ,perl)
               ("randrproto" ,randrproto)
               ("libxrandr" ,libxrandr)
               ("xineramaproto" ,xineramaproto)
               ("libxinerama" ,libxinerama)
               ("libxcursor" ,libxcursor)
               ("fftw-openmpi" ,fftw-openmpi)))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-icd")
     #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'rewrite-usr-bin
          (lambda _
                   (zero? (system* "./autogen.sh")))))))                
    (synopsis "pocl: Portable Computing Language (pocl) aims to become a MIT-licensed
     open source implementation of the OpenCL standard which can be easily adapted for
     new targets and devices, both for homogeneous CPU and heterogenous GPUs/accelerators.")
    (description "Portable Computing Language (pocl) aims to become a MIT-licensed
     open source implementation of the OpenCL standard which can be easily adapted for
     new targets and devices, both for homogeneous CPU and heterogenous GPUs/accelerators.")
    (home-page "http://portablecl.org/")
    (license license:gpl2))))
        
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
    (inputs `(("zip" ,gnuzip:zip)
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
    (license license:gpl2)))
    
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
    (license license:gpl2))))
    
(define (clang-runtime-from-llvm llvm hash)
  (package
    (name "clang-runtime")
    (version (package-version llvm-3.7.1))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/compiler-rt-" version ".src.tar.xz"))
       (sha256 
        (base32 
        "10c1mz2q4bdq9bqfgr3dirc6hz1h3sq8573srd5q5lr7m7j6jiwx"))))
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("llvm" ,llvm)))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"))

    (home-page "http://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license license:ncsa)

    ;; <http://compiler-rt.llvm.org/> doesn't list MIPS as supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define (clang-from-llvm llvm clang-runtime hash)
  (package
    (name "clang")
    (version (package-version llvm-3.7.1))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/cfe-" version ".src.tar.xz"))
       (sha256 
        (base32 
         "0x065d0w9b51xvdjxwfzjxng0gzpbx45fgiaxpap45ragi61dqjn"))
       (patches (list (search-patch "clang-libc-search-path.patch")))))
    ;; Using cmake allows us to treat llvm as an external library.  There
    ;; doesn't seem to be any way to do this with clang's autotools-based
    ;; build system.
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ("libstdc++-4.9", libstdc++-4.9)
       ("python" ,python-2)
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
                          (string-append "\"" compiler-rt "\"")))

                       ;; Same for libc's libdir, to allow crt1.o & co. to be
                       ;; found.
                       (substitute* "lib/Driver/ToolChains.cpp"
                         (("@GLIBC_LIBDIR@")
                          (string-append libc "/lib")))))))))

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
    (license license:ncsa)))
         
(define-public llvm-3.7.1
  (package (inherit llvm)
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/llvm-" version ".src.tar.xz"))
       (sha256
        (base32
         "1masakdp9g2dan1yrazg7md5am2vacbkb3nahb3dchpc1knr8xxy"))))))

(define-public clang-runtime-3.7.1
  (clang-runtime-from-llvm
   llvm-3.7.1
   "0dl1kbrhz96djsxqr61iw5h788s7ncfpfb7aayixky1bhdaydcx4"))

(define-public clang-3.7.1
  (clang-from-llvm llvm clang-runtime
                   "12yv3jwdjcbkrx7zjm8wh4jrvb59v8fdw4mnmz3zc1jb00p9k07w"))



    
    
