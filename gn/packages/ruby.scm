;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
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

(define-module (gn packages ruby)
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby))


(define-public apache-maven
  (package
    (name "apache-maven")
    (version "3.3.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.tudelft.nl/apache/maven/maven-3/3.3.9/source/apache-maven-" version "-src.tar.gz"))
              (sha256
               (base32
                "1g0iavyb34kvs3jfrx2hfnr8lr11m39sj852cy7528wva1glfl4i"))))
    (build-system gnu-build-system)
    (home-page "http://ant.apache.org")
    (synopsis "Build tool for Java")
    (description
     "Ant is a platform-independent build tool for Java.  It is similar to
make but is implemented using the Java language, requires the Java platform,
and is best suited to building Java projects.  Ant uses XML to describe the
build process and its dependencies, whereas Make uses Makefile format.")
    (license license:asl2.0)))


(define-public jruby
  (package
    (name "jruby")
    (version "9.0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://s3.amazonaws.com/jruby.org/downloads/9.0.5.0/jruby-bin-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1wysymqzc7591743f2ycgwpm232y6i050izn72lck44nhnyr5wwy"))
        ))
    (build-system gnu-build-system)
    (native-inputs
     `(("ant" ,ant)
       ;; ("maven" ,maven)
       ("jdk" ,icedtea "jdk")))
    (inputs
     `(("readline" ,readline)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list (string-append "lib/ruby/gems/"
                                        (version-major+minor version)
                                        ".0"))))))
    (synopsis "Programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://ruby-lang.org")
    (license license:ruby)))
