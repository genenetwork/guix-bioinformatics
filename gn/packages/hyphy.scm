;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
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

(define-module (gn packages hyphy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages video)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages web)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)  
  #:use-module (gnu packages linux))

(define-public hyphy ; guix: check
  (package
    (name "hyphy")
    (version "2.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/veg/hyphy/archive/" version 
                                  ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00i3609nywb1xfq50p3kvfbvahql241ciq23jrf67z0yp4y5l5a9"))))
    (inputs
     `(("python" ,python-2)
       ("fftw-openmpi" ,fftw-openmpi)))
    (build-system cmake-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                 (add-after 'unpack `bootstrap 
                  (lambda _
                   (zero? (system* "make" "MPI"))))))) 
    (arguments 
     `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release")
       #:tests? #f))     
    (synopsis "hyphy: an open-source software package for the analysis
of genetic sequences using techniques in phylogenetics, molecular
evolution, and machine learning.")
    (description "HyPhy is an open-source software package for the
analysis of genetic sequences using techniques in phylogenetics,
molecular evolution, and machine learning.  It features a complete
graphical user interface (GUI) and a rich scripting language for
limitless customization of analyses. Additionally, HyPhy features
support for parallel computing environments (via message passing
interface (MPI)) and it can be compiled as a shared library and called
from other programming environments such as Python and R. ")
    (home-page "http://hyphy.org")
    (license license:expat)))



