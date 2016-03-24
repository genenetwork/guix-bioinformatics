;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
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

(define-module (gn packages erlang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls))

(define-public erlang
  (package
    (name "erlang")
    ;; When updating, remember to update the hash of erlang-manpages!
    (version "18.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://erlang.org/download/otp_src_"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "1hy9slq9gjvwdb504dmvp6rax90isnky6chqkyq5v4ybl4lq3azx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)

       ;; Erlang's documentation is distributed in a separate tarball.
       ("erlang-manpages"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://erlang.org/download/otp_doc_man_"
                               version ".tar.gz"))
           (sha256
            (base32
             "1hpcr7a3dx2y9gnb53bvb4g6lyvbwigadl9s3f978s01x40f32wp"))))))
    (inputs
     `(("ncurses" ,ncurses)
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-ssl=" (assoc-ref %build-inputs "openssl")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'path-to-rm
           (lambda _
             (substitute* "erts/configure"
               (("/bin/rm") (which "rm")))
             (substitute* "lib/odbc/configure"
               (("/bin/rm") (which "rm")))))
         (add-after 'patch-source-shebangs 'patch-source-env
           (lambda _
             (let ((escripts
                    (append
                     (find-files "." "\\.escript")
                     (find-files "lib/stdlib/test/escript_SUITE_data/")
                     '("erts/lib_src/utils/make_atomics_api"
                       "erts/preloaded/src/add_abstract_code"
                       "lib/diameter/bin/diameterc"
                       "lib/reltool/examples/display_args"
                       "lib/reltool/examples/mnesia_core_dump_viewer"
                       "lib/snmp/src/compile/snmpc.src"
                       "make/verify_runtime_dependencies"
                       "make/emd2exml.in"))))
               (substitute* escripts
                 (("/usr/bin/env") (which "env"))))))
         (add-before 'configure 'set-erl-top
           (lambda _
             (setenv "ERL_TOP" (getcwd))))
         (add-after 'install 'patch-erl
           ;; This only works after install.
           (lambda _
             (substitute* (string-append (assoc-ref %outputs "out") "/bin/erl")
               (("sed") (which "sed")))))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (manpages (assoc-ref inputs "erlang-manpages"))
                    (share (string-append out "/share/")))
             (mkdir-p share)
             (with-directory-excursion share
               (and
                 (zero? (system* "tar" "xvf" manpages))
                 ;; Delete superfluous files.
                 (for-each delete-file '("COPYRIGHT"
                                         "PR.template"
                                         "README"))))))))
       #:test-target "release_tests"))
    (home-page "http://erlang.org/")
    (synopsis "The Erlang programming language")
    (description
     "Erlang is a programming language used to build massively
scalable soft real-time systems with requirements on high
availability.  Some of its uses are in telecoms, banking, e-commerce,
computer telephony and instant messaging.  Erlang's runtime system has
built-in support for concurrency, distribution and fault tolerance.")
    (license license:asl2.0)))
