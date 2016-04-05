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

(define-module (gn packages sparql-query)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public sparql-query
  (package
    (name "sparql-query")
    (version "1.1")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/tialaramex/sparql-query/archive/"
                          version ".tar.gz"))
      (sha256
       (base32 "0yq3k20472rv8npcc420q9ab6idy584g5y0q501d360k5q0ggr8w"))
      (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("readline" ,readline)
       ("ncurses" ,ncurses)
       ("glib" ,glib)
       ("libxml2" ,libxml2)
       ("curl" ,curl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #t
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'remove-git-dependency
           (lambda _
             (substitute* "Makefile"
               (("^gitrev :=.*$") "gitrev = \"v1.0-3-gb015131\""))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "sparql-query" bin)
               (system* "ln" "--symbolic"
                        (string-append bin "/sparql-query")
                        (string-append bin "/sparql-update")))))
         (replace 'check
           (lambda _
             (and
              ;; The make-flags seem to get ignored here.
              (zero? (system* "make" "CC=gcc" "scan-test"))
              (zero? (system* "./scan-test"))))))))
    (home-page "https://github.com/tialaramex/sparql-query/")
    (synopsis "Command-line tool for accessing SPARQL endpoints over HTTP")
    (description "sparql-query is a command-line tool for accessing SPARQL
endpoints over HTTP.  It has been intentionally designed to 'feel' similar to
tools for interrogating SQL databases. For example, you can enter a query over
several lines, using a semi-colon at the end of a line to indicate the end of
your query.  It also supports readline so that you can more easily recall and
edit previous queries, even across sessions. It can be used non-interactively,
for example from a shell script.")
    (license license:gpl2+)))
