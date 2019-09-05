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

(define-module (gn packages openfyba)
  #:use-module ((guix licenses))
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg))

(define-public openfyba
  (package
   (name "openfyba")
   (version "4.1.1")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/kartverket/fyba.git")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0zxw4pf3s7rb9g8209i3rj0v5jjw1vb79knd5mzvw9drpl9bbgpl"))))
    (inputs `(("zip" ,zip)
             ("autoconf" ,autoconf)
             ("automake" ,automake)
             ("libtool" ,libtool)
             ("libgcrypt" ,libgcrypt)))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             (invoke "autoreconf" "-vfi"))))))
    (home-page "http://labs.kartverket.no/sos/")
    (synopsis "source code release of the FYBA library")
    (description "OpenFYBA is the source code release of the FYBA library.")
    (license gpl2)))
