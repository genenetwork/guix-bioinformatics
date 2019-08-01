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

(define-module (gn packages afnumpy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public afnumpy
  (let ((commit "c5594c1"))
    (package
      (name "afnumpy")
      (version (string-append "rel1-" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/FilipeMaia/afnumpy.git")
                       (commit commit)))
                (file-name (string-append name "-" commit))
                (sha256
                 (base32
                  "0n30xn8cz0ww7czb3m6dz5sh87khan7ag3vb192narmxj37l8qy7"))))
      (build-system python-build-system)
      ;; (native-inputs
      ;; `(("python-setuptools" ,python-setuptools)))
      (arguments
       `(#:python ,python-2
         #:tests? #f))
      (home-page "https://github.com/FilipeMaia/afnumpy")
      (synopsis "Numerical library array processing of numbers, strings, records and objects")
      (description
       "A GPGPU-accelerated drop-in of python's numpy")
      (license license:gpl2))))
