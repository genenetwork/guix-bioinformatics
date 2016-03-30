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

(define-module (gn packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages zip))

;; ----------------------------------------------------------------------------
;; WORKING PACKAGES
;; ----------------------------------------------------------------------------

(define-public xz-java
  (package
   (name "xz-java")
   (version "1.5")
   (source (origin
     (method url-fetch)
     (uri (string-append "http://tukaani.org/xz/"
                         name "-" version ".zip"))
     (sha256
      (base32 "0x6vn9dp9kxk83x2fp3394n95dk8fx9yg8jns9371iqsn0vy8ih1"))))
   (build-system ant-build-system)
   (arguments
    `(#:tests? #f ; There's no test target.
      #:jdk ,icedtea-7
      #:phases
      (modify-phases %standard-phases
        (replace 'unpack
          (lambda _
            (mkdir-p "source")
            (chdir "source")
            (zero? (system* "unzip" (assoc-ref %build-inputs "source")))))
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/xz/")))
              (mkdir-p out)
              (copy-file "build/jar/xz.jar" (string-append out "/xz-1.5.jar"))))))))
   (native-inputs
    `(("unzip" ,unzip)))
   (home-page "http://tukaani.org/xz/java.html")
   (synopsis "Implementation of XZ data compression in pure Java")
   (description "This aims to be a complete implementation of XZ data
compression in pure Java.  Single-threaded streamed compression and
decompression and random access decompression have been fully implemented.")
   (license license:public-domain)))

(define-public jakarta-oro
  (package
    (name "jakarta-oro")
    (version "2.0.8")
    (source (origin
      (method url-fetch)
      (uri (string-append "http://archive.apache.org/dist/jakarta/oro/"
                          name "-" version ".tar.gz"))
      (sha256
       (base32 "0rpmnsskiwmsy8r0sckz5n5dbvh3vkxx8hpm177c754r8xy3qksc"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; There is no 'check' target
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/share/java/oro/")))
               (mkdir-p out)
               (copy-file "jakarta-oro-2.0.8.jar" (string-append out "/oro-2.0.8.jar"))))))
                ))
    (home-page "http://jakarta.apache.org/oro/")
    (synopsis "Set of text-processing Java classes")
    (description "The Jakarta-ORO Java classes are a set of text-processing
Java classes that provide Perl5 compatible regular expressions, AWK-like regular
expressions, glob expressions, and utility classes for performing substitutions,
splits, filtering filenames, etc.")
    (license license:asl1.1)))

;; ----------------------------------------------------------------------------
;; IN PROGRESS
;; ----------------------------------------------------------------------------

(define-public jsch
  (package
    (name "jsch")
    (version "0.1.53")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge.net/jsch/" name "/"
                                  version "/" name "-" version ".zip"))
              (sha256
               (base32 "1729j7khwj6yvkr26fjaf273i3krhz2n1m3mbv5ms4x00qrhrxdn"))))
    (build-system ant-build-system)
    (home-page "http://www.jcraft.com/jsch/")
    (synopsis "JSch is a pure Java implementation of SSH2")
    (description "JSch is a pure Java implementation of SSH2.  JSch allows you
to connect to an sshd server and use port forwarding, X11 forwarding, file
transfer, etc., and you can integrate its functionality into your own Java
programs.")
    (license license:bsd-3)))

(define-public commons-vfs
  (package
    (name "commons-vfs")
    (version "2.0")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "http://mirrors.supportex.net/apache/commons/vfs/source/"
            name "-" version "-src.tar.gz"))
      (sha256
       (base32 "1gkfg9g14kjkh2kf041ssdz9xnw7hpfmdsyrs1bbhyzikwg4s3d9"))))
    (build-system ant-build-system)
    (home-page "https://commons.apache.org/proper/commons-vfs/")
    (synopsis "Commons Virtual File System API for Java")
    (description "Commons VFS provides a single API for accessing various
different file systems.  It presents a uniform view of the files from various
different sources, such as the files on local disk, on an HTTP server, or
inside a Zip archive. ")
    (license license:asl2.0)))

(define-public ivy
;; Ivy tries to download the following packages:
;; - https://repo1.maven.org/maven2/org/apache/ant/ant/1.7.1/ant-1.7.1.jar               => We have ant-1.9.6
;; - https://repo1.maven.org/maven2/org/apache/ant/ant-nodeps/1.7.1/ant-nodeps-1.7.1.jar => Probably not needed
;; - https://repo1.maven.org/maven2/org/apache/ant/ant-trax/1.7.1/ant-trax-1.7.1.jar     => Classes moved to ant.jar
;; - https://repo1.maven.org/maven2/commons-httpclient/commons-httpclient/3.0/commons-httpclient-3.0.jar
;; - https://repo1.maven.org/maven2/oro/oro/2.0.8/oro-2.0.8.jar                          => Packaged
;; - https://repo1.maven.org/maven2/commons-vfs/commons-vfs/1.0/commons-vfs-1.0.jar
;; - https://repo1.maven.org/maven2/com/jcraft/jsch/0.1.50/jsch-0.1.50.jar
;; - https://repo1.maven.org/maven2/com/jcraft/jsch.agentproxy/0.0.6/jsch.agentproxy-0.0.6.jar
;; - https://repo1.maven.org/maven2/com/jcraft/jsch.agentproxy.connector-factory/0.0.6/jsch.agentproxy.connector-factory-0.0.6.jar
;; - https://repo1.maven.org/maven2/com/jcraft/jsch.agentproxy.jsch/0.0.6/jsch.agentproxy.jsch-0.0.6.jar
;; - https://repo1.maven.org/maven2/org/bouncycastle/bcpg-jdk14/1.45/bcpg-jdk14-1.45.jar
;; - https://repo1.maven.org/maven2/org/bouncycastle/bcprov-jdk14/1.45/bcprov-jdk14-1.45.jar
;; - https://repo1.maven.org/maven2/junit/junit/3.8.2/junit-3.8.2.jar
;; - https://repo1.maven.org/maven2/commons-lang/commons-lang/2.6/commons-lang-2.6.jar
;; - https://repo1.maven.org/maven2/org/apache/ant/ant-testutil/1.7.0/ant-testutil-1.7.0.jar
;; - https://repo1.maven.org/maven2/ant/ant-launcher/1.6.2/ant-launcher-1.6.2.jar
;; - https://repo1.maven.org/maven2/ant-contrib/ant-contrib/1.0b3/ant-contrib-1.0b3.jar
;; - https://repo1.maven.org/maven2/xerces/xercesImpl/2.6.2/xercesImpl-2.6.2.jar
;; - https://repo1.maven.org/maven2/xerces/xmlParserAPIs/2.6.2/xmlParserAPIs-2.6.2.jar
  (package
    (name "ivy")
    (version "2.4.0")
    (source (origin
      (method url-fetch)
      (uri (string-append "http://ftp.nluug.nl/internet/apache/ant/" name "/"
                          version "/apache-" name "-" version "-src.tar.gz"))
      (sha256
       (base32 "1xkfn57g2m7l6y0xdq75x5rnrgk52m9jx2xah70g3ggl8750hbr0"))))
    (build-system ant-build-system)
    (inputs
     `(("nss-certs" ,nss-certs)))
    (home-page "http://ant.apache.org/ivy/")
    (synopsis "Dependency manager for Ant")
    (description "Apache Ivy is a popular dependency manager focusing on
flexibility and simplicity.")
    (license license:asl2.0)))


;; ----------------------------------------------------------------------------
;; ON HOLD: WAITING FOR MAVEN PACKAGES
;; ----------------------------------------------------------------------------

;; TODO: Needs commons-compress
;; (define-public ant-compress
;;   (package
;;     (name "ant-compress")
;;     (version "1.4")
;;     (source (origin
;;               (method url-fetch)
;;               (uri "https://www.apache.org/dist/ant/antlibs/compress/source/apache-ant-compress-1.4-src.tar.gz"))
;;             (sha256
;;              (base32 "17v5i11srmi12ckgrvhlwn0gvapgizs5672x252h143r0ya4c04d")))
;;       ))

;; TODO: Needs Maven.
;; (define-public commons-compress
;;   (package
;;     (name "apache-commons-compress")
;;     (version "1.10")
;;     (source (origin
;;               (method url-fetch)
;;               (uri ("http://ftp.tudelft.nl/apache//commons/compress/source/commons-compress-1.10-src.tar.gz")))
;;             (sha256
;;              (base32 "06b40k9dmgqkga3qmfpgzq87jf3fkcxnwaiyczclh58yibg19604")))
;;     ))

;; TODO: Needs Maven.
;; (define-public jcommander
;;   (package
;;     (name "jcommander")
;;     (version "1.48")
;;     (source (origin
;;       (method url-fetch)
;;       (uri (string-append "https://github.com/cbeust/jcommander/archive/"
;;                           name "-" version ".tar.gz"))
;;       (sha256
;;        (base32 "1qn56hd6sxkfdv9j6pwf8c7ia00n39zry236a6achc87wq2kmfnw"))))
;;     (build-system ant-build-system)
;;     (home-page "http://jcommander.org/")
;;     (synopsis "Java framework for parsing command line parameters")
;;     (description "JCommander is a very small Java framework that makes it
;; trivial to parse command line parameters. ")
;;     (license license:asl2.0)))

;; TODO: Needs Maven.
;; (define-public maven-remote-resources-plugin
;;   (package
;;     (name "maven-remote-resources-plugin")
;;     (version "1.5")
;;     (source (origin
;;       (method svn-fetch)
;;       (uri (svn-reference
;;             (url (string-append "http://svn.apache.org/viewvc/maven/plugins/tags/"
;;                                 name "-" version))
;;             (revision 1513840)))
;;       (file-name (string-append name "-" version "-checkout"))
;;       (sha256
;;        (base32 "1g0iavyb34kvs3jfrx2hfnr8lr11m39sj852cy7528wva1glfl4i"))))
;;     (build-system ant-build-system)
;;     (home-page "https://maven.apache.org/plugins/maven-remote-resources-plugin/")
;;     (synopsis "")
;;     (description "")
;;     (license license:asl2.0)))

;; TODO: Needs Maven.
;; (define-public junit
;;   (package
;;     (name "junit")
;;     (version "4.12")
;;     (source (origin
;;       (method url-fetch)
;;       (uri (string-append "https://github.com/junit-team/"
;;                           name "/archive/r" version ".tar.gz"))
;;       (sha256
;;        (base32 "1r6ww2y3jpbpqh03r0966xxiz2vs8n72g6n6l0sjs3aspy56v8b1"))))
;;     (build-system ant-build-system)
;;     (home-page "http://junit.org/")
;;     (synopsis "Framework to write repeatable tests")
;;     (description "JUnit is a simple framework to write repeatable tests.  It is
;; an instance of the xUnit architecture for unit testing frameworks.")
;;     (license license:asl2.0)))

(define-public maven
  (package
    (name "maven")
    (version "3.3.9")
    (source (origin
      (method url-fetch)
      (uri (string-append "http://apache.proserve.nl/maven/maven-3/"
                          version "/source/apache-maven-" version "-src.tar.gz"))
      (sha256
       (base32 "1g0iavyb34kvs3jfrx2hfnr8lr11m39sj852cy7528wva1glfl4i"))))
    (build-system ant-build-system)
    (arguments
     `(#:make-flags "-Dmaven.home=build/"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-m2-variable
           (lambda _
             (setenv "M2_HOME" (string-append (assoc-ref %outputs "out") "/maven-3.3.9")))))))
    (home-page "https://maven.apache.org/")
    (synopsis "")
    (description "Apache Maven is a software project management and
comprehension tool. Based on the concept of a project object model (POM),
Maven can manage a project's build, reporting and documentation from a central
piece of information.")
    (license license:asl2.0)))
