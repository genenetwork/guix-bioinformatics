;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages version-control))

(define-public elixir
  (package
    (name "elixir")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elixir-lang/elixir"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0shznrblh51s87b8z5a3hbk36d4yhkja312b9y7wxih0333kv44s"))
              ;; FIXME: 27 tests (out of 4K) had to be disabled as
              ;; they fail in the build environment.  Common failures
              ;; are:
              ;; - Mix.Shell.cmd() fails with error 130
              ;; - The git_repo fixture cannot be found
              ;; - Communication with spawned processes fails with EPIPE
              ;; - Failure to copy files
                                        ; (patches (search-patches "elixir-disable-failing-tests.patch"))))
              ;;
              ))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("lib/elixir/lib/system.ex"
                            "lib/mix/lib/mix/scm/git.ex"
                            "lib/mix/test/test_helper.exs")
               (("(cmd\\(['\"])git" _ prefix)
                (string-append prefix (which "git"))))
             (substitute* "bin/elixir"
               (("ERL_EXEC=\"erl\"")
                (string-append "ERL_EXEC=" (which "erl"))))
             #t))
         (add-after 'unpack 'fix-or-disable-tests
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Some tests require access to a home directory.
             (setenv "HOME" "/tmp")

             ;; FIXME: These tests fail because the "git_repo" fixture does
             ;; not exist or cannot be found.
             (for-each delete-file '("lib/mix/test/mix/dep_test.exs"
                                     "lib/mix/test/mix/tasks/deps.git_test.exs"
                                     "lib/mix/test/mix/tasks/deps_test.exs"
                                     "lib/mix/test/mix/tasks/cmd_test.exs"
                                     "lib/mix/test/mix/tasks/escript_test.exs"
                                     "lib/mix/test/mix/tasks/deps.tree_test.exs"
                                     "lib/mix/test/mix/tasks/archive_test.exs"
                                     "lib/mix/test/mix/umbrella_test.exs"
                                     "lib/mix/test/mix/rebar_test.exs"
                                     "lib/mix/test/mix/local/installer_test.exs"
                                     "lib/mix/test/mix/shell/quiet_test.exs"
                                     "lib/mix/test/mix/shell/io_test.exs"
                                     "lib/iex/test/iex/helpers_test.exs"))

             ;; FIXME: Mix.Shell.cmd() always fails with error code 130.
             (delete-file "lib/mix/test/mix/shell_test.exs")
             #t))
         ; (add-before 'build 'break (lambda () (#f)))
         (add-before 'build 'make-current
           ;; The Elixir compiler checks whether or not to compile files by
           ;; inspecting their timestamps.  When the timestamp is equal to the
           ;; epoch no compilation will be performed.  Some tests fail when
           ;; files are older than Jan 1, 2000.
           (lambda _
             (for-each (lambda (file)
                         (let ((recent 1400000000))
                           (utime file recent recent 0 0)))
                       (find-files "." ".*"))
             #t))
         (delete 'configure))))
    (propagated-inputs
     `(("erlang" ,erlang)
       ("git" ,git)
       ("glibc-utf8-locales" ,glibc-utf8-locales)
       ))
    (home-page "http://elixir-lang.org/")
    (synopsis "Elixir programming language")
    (description "Elixir is a dynamic, functional language used to build
scalable and maintainable applications.  Elixir leverages the Erlang VM, known
for running low-latency, distributed and fault-tolerant systems, while also
being successfully used in web development and the embedded software domain.")
    (license license:asl2.0)))
