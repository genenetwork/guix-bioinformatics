;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Roel Janssen <roel@gnu.org>
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

(define-module (gn packages slurm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages zip)
  #:use-module (guix git-download))

(define-public slurm-llnl
  (package
    (name "slurm-lnll")
    (version "15-08-6-1")
    (source (origin
             (method url-fetch)
             ;; https://github.com/SchedMD/slurm/archive/slurm-15-08-6-1.tar.gz
              (uri (string-append "https://github.com/SchedMD/slurm/archive/slurm-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00m553aa277iarxj6dalmklyb64r7ias49bfwzbacsfg8h3kar8m"))))
    (inputs `(
              ("openssl" ,openssl)
              ("munge" ,munge)))
    (build-system gnu-build-system)
    (home-page "http://www.schedmd.com/")
    (synopsis "Simple Linux Utility for Resource Management")
    (description
     "Fault-tolerant, and highly scalable cluster management and job
scheduling system for large and small Linux clusters.")
    (license license:gpl2+)))

(define-public munge
  (package
    (name "munge")
    (version "0.5.11")
    (source (origin
             (method url-fetch)
             ;; https://github.com/dun/munge/archive/munge-0.5.11.tar.gz
              (uri (string-append "https://github.com/dun/munge/archive/munge-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0njplyalwwqh7xr7xc7klc6x06mq0ak8w2pxh85w8n4hxkmqqnf5"))))
    (inputs `(("openssl" ,openssl)
              ("libgcrypt" ,libgcrypt)
            ))
    (build-system gnu-build-system)
    (home-page "?")
    (synopsis #f)
    (description #f)
    (license license:gpl2+)))
