;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public arrayfire
(package
    (name "arrayfire")
    (version "3.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/arrayfire/arrayfire.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "113ldnqsil4p84sayv7jh8vnn0nalxibhdyvvwp94vqk20kqg4lw"))
              (patches (search-patches "arrayfire-lapack-detection.patch"
                                       "arrayfire-newer-boost-compute.patch"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("googletest" ,googletest)
       ("assets" ,(origin
                    (method git-fetch)
                     (uri (git-reference
                            (url "https://github.com/arrayfire/assets.git")
                            (commit "729c7b64039e6433ae5ee521658ba20147efcb02"))) ; March 4, 2019
                     (file-name (git-file-name "arrayfire-assets" "submodule"))
                     (sha256
                      (base32
                       "05zg7m6zlwi3llbv7l5wd9qi9ppb9p3ad2i5xmqwvcbgx5ry4l2s"))))
       ("threads" ,(origin
                     (method git-fetch)
                     (uri (git-reference
                            (url "https://github.com/alltheflops/threads.git")
                            (commit "5e778ce0a7f0f80af9d32ea3569df3dbec834f59"))) ; Dec 16, 2015
                     (file-name (git-file-name "arrayfire-threads" "submodule"))
                     (sha256
                      (base32
                       "1rj2357r124b4ry0s467fz9hs4jxcyacliwprggvai85a39pqabx"))))))
    (inputs
     `(("boost" ,boost)
       ("clBLAS" ,clBLAS)
       ("clFFT" ,clFFT)
       ("fftw-openmpi" ,fftw-openmpi)
       ("fftwf" ,fftwf)
       ("ocl-icd" ,ocl-icd)
       ("openblas" ,openblas)
       ("opencl-headers" ,opencl-headers)))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DBUILD_OPENCL=ON"
         "-DBUILD_CUDA=OFF"
         "-DBUILD_GRAPHICS=OFF"
         "-DBUILD_TEST=OFF" ; building tests segfaults
         "-DUSE_SYSTEM_CLBLAS=ON"
         "-DUSE_SYSTEM_CLFFT=ON"
         "-DUSE_SYSTEM_GTEST=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-more-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((assets  (assoc-ref inputs "assets"))
                   (threads (assoc-ref inputs "threads")))
               (copy-recursively assets "assets")
               (copy-recursively threads "src/backend/cpu/threads"))
             #t))
         (add-after 'unpack 'fix-sources
           (lambda _
             (substitute* "src/backend/opencl/blas.cpp"
               ;; https://github.com/arrayfire/arrayfire/commit/90a9ffbce5c38352a365e03a634ffaf0d2fb9933
               (("#undef BLAS_FUNC_DEF")
                "#undef BLAS_FUNC_DEF\n#undef BLAS_FUNC"))
             (substitute* '("src/backend/cpu/blas.cpp"
                            "src/backend/opencl/cpu/cpu_blas.cpp")
               ;; https://github.com/arrayfire/arrayfire/pull/2538/files
               (("&cblas_##PREFIX##FUNC;")
                "(FUNC##_func_def<TYPE>)&cblas_##PREFIX##FUNC;"))
             #t)))
       #:tests? #f)) ; Building the tests fail linking, so we build the examples as a test.
    (home-page "https://arrayfire.com/")
    (synopsis "High performance library for parallel computing")
    (description
     "ArrayFire is a high performance software library for parallel computing
with an easy-to-use API.  Its array based function set makes parallel
programming simple.")
    (license (list license:bsd-3 ; everything except CMakeModules folder
                   license:cc0)))) ; assets

(define-public clBLAS
  (package
    (name "clBLAS")
    (version "2.12")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/clMathLibraries/clBLAS.git")
                    (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "154mz52r5hm0jrp5fqrirzzbki14c1jkacj75flplnykbl36ibjs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "../source/src"
             "-DUSE_SYSTEM_GTEST=ON"
             ;"-DBoost_USE_STATIC_LIBS=OFF" ; this does not seem to apply
             "-DBUILD_TEST=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("Boost_USE_STATIC_LIBS   ON")
                "Boost_USE_STATIC_LIBS   OFF"))
             #t)))))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("openblas" ,openblas)
       ("opencl-headers" ,opencl-headers)
       ("python" ,python-2)))
    (inputs
     `(("boost" ,boost)
       ("ocl-icd" ,ocl-icd)))
    (home-page "https://github.com/clMathLibraries/clBLAS")
    (synopsis "Library containing BLAS functions written in OpenCL")
    (description
     "The primary goal of @code{clBLAS} is to make it easier for developers to
utilize the inherent performance and power efficiency benefits of heterogeneous
computing.  @code{clBLAS} interfaces do not hide nor wrap @code{OpenCL}
interfaces, but rather leaves @code{OpenCL} state management to the control of
the user to allow for maximum performance and flexibility.  The @code{clBLAS}
library does generate and enqueue optimized @code{OpenCL} kernels, relieving the
user from the task of writing, optimizing and maintaining kernel code themselves.")
    (license license:asl2.0)))

(define-public clFFT
  (package
    (name "clFFT")
    (version "2.10.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/clMathLibraries/clFFT.git")
                    (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1ksmcsqf25bmaq9rr2z1m936mxgh9cx6can2l7nna4mzlj0aghrs"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("../source/src"
                           "-DBUILD_TEST=ON"
                           "-DUSE_SYSTEM_GTEST=ON"
                           "-DBoost_USE_STATIC_LIBS=OFF")
       #:test-target "Test"))
    (native-inputs
     `(("boost" ,boost)
       ("fftw-openmpi" ,fftw-openmpi)
       ("fftwf" ,fftwf)
       ("googletest" ,googletest)))
    (inputs
     `(("ocl-icd" ,ocl-icd)
       ("opencl-headers" ,opencl-headers)))
    (home-page "https://github.com/clMathLibraries/clFFT/")
    (synopsis "Library containing FFT functions written in OpenCL")
    (description "@code{clFFT} is a software library containing FFT functions
written in @code{OpenCL}.  In addition to GPU devices, the library also supports
running on CPU devices to facilitate debugging and heterogeneous programming.")
    (license license:asl2.0)))

(define-public compute ; superceeded by boost-1.61+
  (package
    (name "compute")
    (version "0.5")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/boostorg/compute.git")
                    (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0jc04k43br49hqgzrxfn3wfz2m94q1z45zfg5nahqb7p0lbfgwyx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DCMAKE_BUILD_TYPE=Release")
       #:tests? #f)) ; tests require OpenCL device
    (native-inputs
     `(("boost" ,boost-for-mysql) ; 1.59.0
       ("opencl-headers" ,opencl-headers)
       ("ocl-icd" ,ocl-icd)))
    (home-page "http://boostorg.github.io/compute/")
    (synopsis "C++ GPU Computing Library for OpenCL")
    (description
     "@code{Boost.Compute} is a GPU/parallel-computing library for C++ based on
@code{OpenCL}.  The core library is a thin C++ wrapper over the @code{OpenCL
API} and provides access to compute devices, contexts, command queues and memory
buffers.")
    (license license:boost1.0)))
