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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages tls))

(define-public erlang
  (package
   (name "erlang")
    ;; This is the from git install
    ;; When updating, remember to update the hash of erlang-manpages!
    (version "19.0-rc2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/erlang/otp/archive/OTP-"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ny2c3n9v8ifjpmnhlj5gpmzcsi882qv2vfhsssrk2f9rk4z4hpb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
      ))

       ;; Erlang's documentation is distributed in a separate tarball.
    (inputs
     `(("ncurses" ,ncurses)
       ("mesa" ,mesa)
       ("wxwidgets" ,wxwidgets)
       ("openssl" ,openssl)))
    (propagated-inputs
     ;; Headers from Mesa and GLU are needed.
     `(("glu" ,glu)
       ("mesa" ,mesa)))

    (arguments
     `(#:configure-flags
       (list "--disable-saved-compile-time" "--enable-wx" "--enable-native-libs"
             "--enable-threads" "--enable-dynamic-ssl-lib" "--enable-shared-zlib"
             "--enable-smp-support"
             (string-append "--with-ssl=" (assoc-ref %build-inputs "openssl")))
       #:phases
       (modify-phases %standard-phases
        (add-before 'configure 'autoconf
          (lambda _
            ;; (zero? (system* "autoreconf" "-vfi"))))
            (zero? (system* "./otp_build" "autoconf"))))
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
         )
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
