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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system perl))

(define-public perl-time-parsedate ; guix: old lib and tests failing - works though
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

(define-public logtodo
  (let ((commit "acda8f2a7aa67f590ae03487306794b039eaba68"))
  (package
    (name "logtodo")
    (version (string-append "1.0.5-" (string-take commit 7) ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   ;; (url "https://github.com/genenetwork/genenetwork2.git")
                   (url "https://gitlab.com/pjotrp/logtodo.git")
                   (commit commit)))
             (file-name (string-append name "-" (string-take commit 7)))
             (sha256
              (base32
               "0bcjnb09sxgv7c45nk4sipxw38a6ldhmijgn614h7bibwqyvybmc"))))
    (build-system perl-build-system)
    (propagated-inputs `(("perl-time-parsedate" ,perl-time-parsedate)))
    (arguments
     `(
       #:phases
        (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                           (let ((out (assoc-ref outputs "out")))
                             (install-file "logtodo" (string-append out "/bin"))))))
       #:tests? #f))   ; no 'setup.py test' really!
    (home-page "https://gitlab.com/pjotrp/logtodo")
    (synopsis "Pjotr's time logger")
    (description "None")
    (license license:gpl2+))))
