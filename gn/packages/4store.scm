;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
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

(define-module (gn packages 4store)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages file)
  #:use-module (gnu packages xml))

(define-public 4store ; guix ready
  (let ((commit "c5a56d7c7504551a1f2fff6c16c4d9a440e4a317"))
    (package
      (name "4store")
      (version (string-append "1.1.6-0." (string-take commit 7)))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/garlik/4store/archive/"
                                    commit ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32 "12pn5masjrfp1d8v1dz47qga0ns8d03f2f5icnb9d6m1jfp8l38a"))
                (patches (list (search-patch "4store-fix-buildsystem.patch")))))
      (build-system gnu-build-system)
      (native-inputs
       `(("perl" ,perl)
         ("python" ,python-2)
         ("file" ,file)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gettext" ,gnu-gettext)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("glib" ,glib)
         ("rasqal" ,rasqal)
         ("libxml2" ,libxml2)
         ("raptor2" ,raptor2)
         ("readline" ,readline)
         ("avahi" ,avahi)
         ("pcre" ,pcre)
         ("cyrus-sasl" ,cyrus-sasl)
         ("openssl" ,openssl)
         ("util-linux" ,util-linux)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'configure 'generate-configure
             (lambda _
               (zero? (system* "./autogen.sh")))))))
      (home-page "http://www.4store.org")
      (synopsis "Clustered RDF storage and query engine")
      (description "4store is a RDF/SPARQL store written in C, either single
machines or networked clusters.")
      (license license:gpl3+))))
