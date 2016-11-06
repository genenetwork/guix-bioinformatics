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
  #:use-module (gnu packages ruby)
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

(define-public bio-vcf
  (package
   (name "bio-vcf")
   (version "0.9.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "bio-vcf" version))
     (sha256
      (base32
       "1007bn0w8l11q867lxsyqnk0vgvv12skvk9gyglv7g44knr5vh4j"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
   ))
   (synopsis
    "Smart lazy multi-threaded parser for VCF format with useful
filtering and output rewriting (JSON, RDF etc.)")
   (description
    "Smart lazy multi-threaded parser for VCF format with useful
filtering and output rewriting (JSON, RDF etc.)")
   (home-page
    "http://github.com/pjotrp/bioruby-vcf")
   (license expat)))

(define-public bio-table
  (package
   (name "bio-table")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "bio-table" version))
     (sha256
      (base32
       "1jlpls734kd41rffn2y2747nr14k5rwgaj2g3k48i9xgsfcmrn6r"))))
   (build-system ruby-build-system)
   (arguments
    `(#:tests? #f ; There are no tests.
   ))
   (propagated-inputs
    `(("ruby-bio-logger" ,ruby-bio-logger)))
   (synopsis
    "Functions and tools for tranforming and changing tab delimited
and comma separated table files - useful for Excel sheets and SQL/RDF
output")
   (description
    "Functions and tools for tranforming and changing tab delimited
and comma separated table files - useful for Excel sheets and SQL/RDF
output")
   (home-page
    "http://github.com/pjotrp/bioruby-table")
   (license expat)))

(define-public ruby-net-http-digest-auth
  (package
    (name "ruby-net-http-digest-auth")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "net-http-digest_auth" version))
       (sha256
        (base32
         "14801gr34g0rmqz9pv4rkfa3crfdbyfk6r48vpg5a5407v0sixqi"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis
     "An implementation of RFC 2617 - Digest Access Authentication.  At this time
the gem does not drop in to Net::HTTP and can be used for with other HTTP
clients.

In order to use net-http-digest_auth you'll need to perform some request
wrangling on your own.  See the class documentation at Net::HTTP::DigestAuth
for an example.")
    (description
     "An implementation of RFC 2617 - Digest Access Authentication.  At this time
the gem does not drop in to Net::HTTP and can be used for with other HTTP
clients.

In order to use net-http-digest_auth you'll need to perform some request
wrangling on your own.  See the class documentation at Net::HTTP::DigestAuth
for an example.")
    (home-page
     "http://github.com/drbrain/net-http-digest_auth")
    (license #f)))

(define-public ruby-ntlm-http
  (package
    (name "ruby-ntlm-http")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ntlm-http" version))
       (sha256
        (base32
         "0yx01ffrw87wya1syivqzf8hz02axk7jdpw6aw221xwvib767d36"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis
     "Ruby/NTLM HTTP provides NTLM authentication over http.")
    (description
     "Ruby/NTLM HTTP provides NTLM authentication over http.")
    (home-page "http://www.mindflowsolutions.net")
    (license #f)))

(define-public ruby-webrobots
  (package
    (name "ruby-webrobots")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "webrobots" version))
       (sha256
        (base32
         "19ndcbba8s8m62hhxxfwn83nax34rg2k5x066awa23wknhnamg7b"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis
     "This library helps write robots.txt compliant web robots in Ruby.
")
    (description
     "This library helps write robots.txt compliant web robots in Ruby.
")
    (home-page "https://github.com/knu/webrobots")
    (license #f)))

(define-public ruby-mechanize
  (package
    (name "ruby-mechanize")
    (version "2.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mechanize" version))
       (sha256
        (base32
         "1f861x62kmggy60krv229s5jl7afq9nblwcfih3kp9bm5c5jn16y"))))
    (build-system ruby-build-system)
    (arguments '(#:gem-flags '("--trace")))
    (inputs
     `(("ruby" ,ruby)))
    (propagated-inputs
     `(("ruby-domain-name" ,ruby-domain-name)
       ("ruby-http-cookie" ,ruby-http-cookie)
       ("ruby-mime-types" ,ruby-mime-types)
       ("ruby-net-http-digest-auth"
        ,ruby-net-http-digest-auth)
       ("ruby-net-http-persistent"
        ,ruby-net-http-persistent)
       ("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-ntlm-http" ,ruby-ntlm-http)
       ("ruby-webrobots" ,ruby-webrobots)))
    (synopsis
     "The Mechanize library is used for automating interaction with websites.
Mechanize automatically stores and sends cookies, follows redirects,
and can follow links and submit forms.  Form fields can be populated and
submitted.  Mechanize also keeps track of the sites that you have visited as
a history.")
    (description
     "The Mechanize library is used for automating interaction with websites.
Mechanize automatically stores and sends cookies, follows redirects,
and can follow links and submit forms.  Form fields can be populated and
submitted.  Mechanize also keeps track of the sites that you have visited as
a history.")
  (home-page
   "http://docs.seattlerb.org/mechanize/")
  (license expat)))
