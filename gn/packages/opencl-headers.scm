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

(define-module (gn packages opencl-headers)
  #:use-module ((guix licenses))
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)  
  #:use-module (gnu packages linux))

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
    (license gpl2))))
