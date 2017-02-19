;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Pjotr Prins <pjotr.guix@thebird.nl>
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

(define-module (gn packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix build-system perl))

(define-public perl-time-parsedate
(package
  (name "perl-time-parsedate")
  (version "2015.103")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/M/MU/MUIR/modules/Time-ParseDate-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1lgfr87j4qwqnln0hyyzgik5ixqslzdaksn9m8y824gqbcihc6ic"))))
  (build-system perl-build-system)
  (arguments `(#:tests? #f))          ;Oh well
  (home-page
    "http://search.cpan.org/dist/Time-ParseDate")
  (synopsis "Parse and format time values")
  (description "ParseDate")
  (license #f)))
