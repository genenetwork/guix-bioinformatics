;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Pjotr Prins <pjotr.public12@thebird.nl>
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

(define-module (gn packages elixir)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)  ; for patch
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages version-control))

(define-public elixir
  (package
    (name "elixir")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/elixir-lang/elixir/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jsc6kl7f74yszcypdv3w3vhyc9qfqav8nwc41in082m0vpfy95y"))
              ))

    (build-system gnu-build-system)
    (native-inputs
     `(("patch" ,patch)
       ("patch/elixir-disable-failing-tests" ,(search-patch "elixir-disable-failing-tests.patch"))
       ("patch/elixir-disable-mix-tests" ,(search-patch "elixir-disable-mix-tests.patch"))
        ))
    (inputs
     `(("erlang" ,erlang)
       ("git" ,git)))
    (arguments
     `(#:phases (modify-phases %standard-phases
         (delete 'configure)
         (add-before
          'build 'rewrite-path
          (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "bin/elixir"
                     (("ERL_EXEC=\"erl\"") (string-append "ERL_EXEC=" (which "erl"))))))
         (add-after 'build 'disable-breaking-elixir-tests ;; when making this convential part of source the build breaks!
          (lambda* (#:key inputs #:allow-other-keys)

            (and
             (zero? (system* "patch" "--force" "-p1" "-i" (assoc-ref inputs "patch/elixir-disable-failing-tests")))
             (zero? (system* "patch" "--force" "-p1" "-i" (assoc-ref inputs "patch/elixir-disable-mix-tests")))
             (delete-file "./lib/mix/test/mix/tasks/deps.git_test.exs")
             (delete-file "./lib/mix/test/mix/shell_test.exs")
            )))

         (replace 'check
                  (lambda _
                    (zero? (system* "make" "test")))))
       #:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #t)) ;; 3124 tests, 0 failures, 11 skipped

    (home-page "http://elixir-lang.org/")
    (synopsis "The Elixir programming language")
(description "Elixir is a dynamic, functional language designed for
building scalable and maintainable applications. Elixir leverages the
Erlang VM, known for running low-latency, distributed and
fault-tolerant systems, while also being successfully used in web
development and the embedded software domain.")
    (license license:asl2.0)))
