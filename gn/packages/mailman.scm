(define-module (gn packages mailman)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls))

;;;
;;;^L
;;;

(define zpl2.1
  ((@@ (guix licenses) license) "Zope Public License 2.1"
                                "http://old.zope.org/Resources/ZPL/"
                                "https://directory.fsf.org/wiki/License:ZPL-2.1"))

;;;
;;;^L
;;;

(define-public mailman
  (package
    (name "mailman")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mailman" version))
        (sha256
         (base32
          "1qph9i93ndahfxi3bb2sd0kjm2c0pkh844ai6zacfmvihl1k3pvy"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda _
             ;; This module is built into Python 3.7+.
             (substitute* "setup.py"
               ((".*importlib_resources.*") ""))
             #t)))))
    (propagated-inputs
     `(("python-aiosmtpd" ,python-aiosmtpd)
       ("python-alembic" ,python-alembic)
       ("python-atpublic" ,python-atpublic)
       ("python-authheaders" ,python-authheaders)
       ("python-authres" ,python-authres)
       ("python-click" ,python-click)
       ("python-dateutil" ,python-dateutil)
       ("python-dnspython" ,python-dnspython)
       ("python-falcon" ,python-falcon)
       ("python-flufl.bounce" ,python-flufl.bounce)
       ("python-flufl.i18n" ,python-flufl.i18n)
       ("python-flufl.lock" ,python-flufl.lock)
       ("python-gunicorn" ,python-gunicorn)
       ;("python-importlib-resources" ,python-importlib-resources) ; built into python-3.7
       ("python-lazr.config" ,python-lazr.config)
       ("python-passlib" ,python-passlib)
       ("python-requests" ,python-requests)
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-zope.component" ,python-zope.component)
       ("python-zope.configuration" ,python-zope.configuration)
       ("python-zope.event" ,python-zope.event)
       ("python-zope.interface" ,python-zope.interface)))
    (home-page "https://www.list.org")
    (synopsis "Mailing list manager")
    (description
     "GNU Mailman is software for managing email discussion and mailing
lists.  Both users and administrators generally perform their actions in a
web interface, although email and command-line interfaces are also provided.
The system features built-in archiving, automatic bounce processing, content
filtering, digest delivery, and more.")
    (license license:gpl3+)))

(define-public python-aiosmtpd
  (package
    (name "python-aiosmtpd")
    (version "1.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "aiosmtpd" version))
        (sha256
         (base32
          "1xdfk741pjmz1cm8dsi4n5vq4517i175rm94696m3f7kcgk7xsmp"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-failing-test
           (lambda _
             (delete-file "aiosmtpd/tests/test_smtps.py")
             #t))
         (replace 'check
           (lambda _
             (invoke "python" "-m" "nose2" "-v"))))))
    (native-inputs
     `(("python-flufl.testing" ,python-flufl.testing)
       ("python-nose2" ,python-nose2)))
    (propagated-inputs
     `(("python-atpublic" ,python-atpublic)))
    (home-page "https://aiosmtpd.readthedocs.io/")
    (synopsis "Asyncio based SMTP server")
    (description
     "This project is a reimplementation of the Python stdlib @code{smtpd.py}
based on asyncio.")
    (license license:asl2.0)))

(define-public python-flufl.testing
  (package
    (name "python-flufl.testing")
    (version "0.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.testing" version))
        (sha256
         (base32
          "1nkm95mhcfhl4x5jgs6y97ikszaxsfh07nyawsih6cxxm6l62641"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose2" ,python-nose2)))
    (home-page "https://gitlab.com/warsaw/flufl.testing")
    (synopsis "Collection of test tool plugins")
    (description
     "A small collection of test tool plugins for nose2 and flake8.")
    (license license:asl2.0)))

(define-public python-atpublic
  (package
    (name "python-atpublic")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "atpublic" version))
        (sha256
         (base32
          "0i3sbxkdlbb4560rrlmwwd5y4ps7k73lp4d8wnmd7ag9k426gjkx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "-m" "nose2" "-v"))))))
    (native-inputs
     `(("python-nose2" ,python-nose2)))
    (home-page "https://public.readthedocs.io/")
    (synopsis "Python library for populating __all__")
    (description
     "This is a very simple decorator and function which populates a modules
@code{__all__} and optionally the module globals.  This provides both a
pure-Python implementation and an optional C implementation.")
    (license license:asl2.0)))

(define-public python-authheaders
  (package
    (name "python-authheaders")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "authheaders" version))
        (sha256
         (base32
          "1ljcp8vk2n4xwk8p758b6q5sgdicyj4gxxpkmh33mx21jscn6q4i"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-authres" ,python-authres)
       ("python-dkimpy" ,python-dkimpy)
       ("python-dnspython" ,python-dnspython)
       ("python-publicsuffix" ,python-publicsuffix)))
    (home-page "https://github.com/ValiMail/authentication-headers")
    (synopsis "Library wrapping email authentication header verification and generation")
    (description
     "A library wrapping email authentication header verification and generation.")
    (license license:expat)))

(define-public python-authres
  (package
    (name "python-authres")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "authres" version))
        (sha256
         (base32
          "1dr5zpqnb54h4f5ax8334l1dcp8j9083d7v4vdi1xqkwmnavklck"))))
    (build-system python-build-system)
    (home-page "https://launchpad.net/authentication-results-python")
    (synopsis "Authentication Results Header Module")
    (description
     "This package provides RFC 5451/7001/7601 Authentication-Results Headers
generation and parsing for Python.")
    (license license:asl2.0)))

(define-public python-flufl.bounce
  (package
    (name "python-flufl.bounce")
    (version "3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.bounce" version))
        (sha256
         (base32
          "0k5kjqa3x6gvwwxyzb2vwi1g1i6asm1zw5fivylxz3d583y4kid2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-atpublic" ,python-atpublic)
       ("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-nose2" ,python-nose2)))
    (home-page "https://fluflbounce.readthedocs.io/en/latest/")
    (synopsis "Email bounce detectors")
    (description "The @code{flufl.bounce} library provides a set of heuristics
and an API for detecting the original bouncing email addresses from a bounce
message.  Many formats found in the wild are supported, as are VERP and
RFC 3464.")
    (license license:asl2.0)))

(define-public python-flufl.i18n
  (package
    (name "python-flufl.i18n")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.i18n" version))
        (sha256
         (base32
          "1csgds59nx0ann9v2alqr69lakp1cnc1ikmbgn96l6n23js7c2ah"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-atpublic" ,python-atpublic)))
    (home-page "https://flufli18n.readthedocs.io")
    (synopsis "API for Python internationalization")
    (description
     "This package provides a high level, convenient API for managing
internationalization translation contexts in Python application.  There is a
simple API for single-context applications, such as command line scripts which
only need to translate into one language during the entire course of thei
execution.  There is a more flexible, but still convenient API for multi-context
applications, such as servers, which may need to switch language contexts for
different tasks.")
    (license license:asl2.0)))

(define-public python-flufl.lock
  (package
    (name "python-flufl.lock")
    (version "3.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flufl.lock" version))
        (sha256
         (base32
          "0nzzd6l30ff6cwsrlrb94xzfja4wkyrqv3ydc6cz0hdbr766mmm8"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-atpublic" ,python-atpublic)))
    (home-page "https://flufllock.readthedocs.io")
    (synopsis "NFS-safe file locking with timeouts for POSIX systems")
    (description
     "This package provides NFS-safe file locking with timeouts for POSIX systems.")
    (license license:asl2.0)))

(define-public python-gunicorn
  (package
    (name "python-gunicorn")
    (version "19.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "gunicorn" version))
        (sha256
         (base32
          "1wzlf4xmn6qjirh5w81l6i6kqjnab1n1qqkh7zsj1yb6gh4n49ps"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-verion-restrictions
           (lambda _
             (substitute* "requirements_test.txt"
               (("coverage.*") "coverage\n")
               (("pytest.*") "pytest\n")
               (("pytest-cov.*") "pytest-cov\n"))
             #t)))))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)))
    (home-page "https://gunicorn.org")
    (synopsis "WSGI HTTP Server for UNIX")
    (description "Gunicorn 'Green Unicorn' is a Python WSGI HTTP Server for
UNIX.  It's a pre-fork worker model ported from Ruby's Unicorn project.  The
Gunicorn server is broadly compatible with various web frameworks, simply
implemented, light on server resource usage, and fairly speedy.")
    (license license:expat)))

(define-public python-importlib-resources
  (package
    (name "python-importlib-resources")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "importlib_resources" version))
        (sha256
         (base32
          "0y3hg12iby1qyaspnbisz4s4vxax7syikk3skznwqizqyv89y9yk"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pathlib2" ,python-pathlib2)
       ("python-typing" ,python-typing)))
    (home-page "https://importlib-resources.readthedocs.io/")
    (synopsis "Read resources from Python packages")
    (description
     "Read resources from Python packages")
    (license license:asl2.0)))

(define-public python-lazr.config
  (package
    (name "python-lazr.config")
    (version "2.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lazr.config" version))
        (sha256
         (base32
          "1s7pyvlq06qjrkaw9r6nc290lb095n25ybzgavvy51ygpxkgqxwn"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-lazr.delegates" ,python-lazr.delegates)
       ("python-zope.interface" ,python-zope.interface)))
    (home-page "https://launchpad.net/lazr.config")
    (synopsis "Create configuration schemas and process and validate configurations")
    (description
     "The LAZR config system is typically used to manage process configuration.
Process configuration is for saying how things change when we run systems on
different machines, or under different circumstances.  This system uses ini-like
file format of section, keys, and values.  The config file supports inheritance
to minimize duplication of information across files.  The format supports schema
validation.")
    (license license:lgpl3)))

(define-public python-zope.component
  (package
    (name "python-zope.component")
    (version "4.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.component" version))
        (sha256
         (base32
          "0mafp41aqcffbfl9dsac34clc7zlpxwwzkx8jllbg4xmqckddpvf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "setup.py" "test"))))))
    (propagated-inputs
     `(("python-zope.deferredimport" ,python-zope.deferredimport)
       ("python-zope.deprecation" ,python-zope.deprecation)
       ("python-zope.event" ,python-zope.event)
       ("python-zope.hookable" ,python-zope.hookable)
       ("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-persistent" ,python-persistent)
       ("python-zope.configuration" ,python-zope.configuration-bootstrap)
       ("python-zope.i18nmessageid" ,python-zope.i18nmessageid)
       ("python-zope.location" ,python-zope.location-bootstrap)
       ("python-zope.proxy" ,python-zope.proxy-bootstrap)
       ("python-zope.security" ,python-zope.security-bootstrap)
       ("python-zope.testing" ,python-zope.testing)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.component")
    (synopsis "Zope Component Architecture")
    (description "Zope Component Architecture")
    (license zpl2.1)))

(define-public python-zope.component-bootstrap
  (package
    (inherit python-zope.component)
    (arguments `(#:tests? #f))
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-zope.configuration
  (package
    (name "python-zope.configuration")
    (version "4.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.configuration" version))
        (sha256
         (base32
          "1qb88764fd7nkkmqv7fl9bxd1jirynkg5vbqkpqdiffnkxzp85kf"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.i18nmessageid" ,python-zope.i18nmessageid)
       ("python-zope.interface" ,python-zope.interface)
       ("python-zope.schema" ,python-zope.schema)))
    (native-inputs
     `(("python-manuel" ,python-manuel)
       ("python-zope.schema" ,python-zope.schema)
       ("python-zope.testing" ,python-zope.testing)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.configuration")
    (synopsis
      "Zope Configuration Markup Language (ZCML)")
    (description
      "Zope Configuration Markup Language (ZCML)")
    (license zpl2.1)))

(define-public python-zope.configuration-bootstrap
  (package
    (inherit python-zope.configuration)
    (arguments `(#:tests? #f))
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-zope.event
  (package
    (name "python-zope.event")
    (version "4.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.event" version))
        (sha256
         (base32
          "1ksbc726av9xacml6jhcfyn828hlhb9xlddpx6fcvnlvmpmpvhk9"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-zope.testrunner" ,python-zope.testrunner-bootstrap)))
    (home-page "https://github.com/zopefoundation/zope.event")
    (synopsis "Basic event publishing system")
    (description
     "This package provides a simple event system on which application-specific
event systems can be built.  For example, a type-based event dispatching system
that builds on @code{zope.interface} can be found in @code{zope.component}.  A
simpler system is distributed with this package and is described in Class-based
event handlers.")
    (license zpl2.1)))

(define-public python-zope.interface
  (package
    (name "python-zope.interface")
    (version "4.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.interface" version))
        (sha256
         (base32
          "1rgh2x3rcl9r0v0499kf78xy86rnmanajf4ywmqb943wpk50sg8v"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nose" ,python-nose)
       ("python-zope.event" ,python-zope.event)))
    (home-page "https://github.com/zopefoundation/zope.interface")
    (synopsis "Interfaces for Python")
    (description "Interfaces for Python")
    (license zpl2.1)))

(define-public python-dkimpy
  (package
    (name "python-dkimpy")
    (version "0.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dkimpy" version))
        (sha256
         (base32
          "1wlzahsy4dz3w7dzbr6ayd2vqps1zcbj6101lbbgarn43fkpmx3b"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-source
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (substitute* "dkim/dknewkey.py"
                 (("/usr/bin/openssl") (string-append openssl "/bin/openssl"))))
             #t))
         (replace 'check
           (lambda _
             (invoke "python" "test.py"))))))
    (propagated-inputs
     `(("python-dnspython" ,python-dnspython)))
    (native-inputs
     `(("python-authres" ,python-authres)
       ("python-pynacl" ,python-pynacl)))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://launchpad.net/dkimpy")
    (synopsis "DKIM (DomainKeys Identified Mail)")
    (description "Python module that implements @dfn{DKIM} (DomainKeys
Identified Mail) email signing and verification (RFC6376).  It also provides
helper scripts for command line signing and verification.  It supports DKIM
signing/verifying of ed25519-sha256 signatures (RFC 8463).  It also supports
the RFC 8617 Authenticated Received Chain (ARC) protocol.")
    (license license:bsd-3)))

(define-public python-lazr.delegates
  (package
    (name "python-lazr.delegates")
    (version "2.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lazr.delegates" version))
        (sha256
         (base32
          "1rdnl85j9ayp8n85l0ciip621j9dcziz5qnmv2m7krgwgcn31vfx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "nosetests"))))))
    (propagated-inputs
     `(("python-nose" ,python-nose)
       ("python-zope.interface" ,python-zope.interface)))
    (home-page "https://launchpad.net/lazr.delegates")
    (synopsis "Easily write objects that delegate behavior")
    (description
     "The @code{lazr.delegates} package makes it easy to write objects that
delegate behavior to another object.  The new object adds some property or
behavior on to the other object, while still providing the underlying interface,
and delegating behavior.")
    (license license:lgpl3)))

(define-public python-zope.deferredimport
  (package
    (name "python-zope.deferredimport")
    (version "4.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.deferredimport" version))
        (sha256
         (base32
          "1q89v54dwniiqypjbwywwdfjdr4kdkqlyqsgrpplgvsygdg39cjp"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.proxy" ,python-zope.proxy)))
    (native-inputs
     `(("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.deferredimport")
    (synopsis "Defer imports until used by code")
    (description
     "Often, especially for package modules, you want to import names for
convenience, but not actually perform the imports until necessary.  The
@code{zope.deferredimport} package provided facilities for defining names in
modules that will be imported from somewhere else when used.  You can also cause
deprecation warnings to be issued when a variable is used.")
    (license zpl2.1)))

(define-public python-zope.deprecation
  (package
    (name "python-zope.deprecation")
    (version "4.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.deprecation" version))
        (sha256
         (base32
          "1pz2cv7gv9y1r3m0bdv7ks1alagmrn5msm5spwdzkb2by0w36i8d"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.deprecation")
    (synopsis "Zope Deprecation Infrastructure")
    (description "Zope Deprecation Infrastructure")
    (license zpl2.1)))

(define-public python-zope.hookable
  (package
    (name "python-zope.hookable")
    (version "4.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.hookable" version))
        (sha256
         (base32
          "05fy9lynyglzyiy1nbzdyv3rgvznwv0s0q0dr2hcavv6lclkkpy1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-zope.testing" ,python-zope.testing)))
    (home-page "https://github.com/zopefoundation/zope.hookable")
    (synopsis "Zope hookable")
    (description "This package supports the efficient creation of hookable
objects, which are callable objects that are meant to be optionally replaced.
The idea is that you create a function that does some default thing and make i
hookable.  Later, someone can modify what it does by calling its sethook method
and changing its implementation.  All users of the function, including those
that imported it, will see the change.")
    (license zpl2.1)))

(define-public python-zope.i18nmessageid
  (package
    (name "python-zope.i18nmessageid")
    (version "4.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.i18nmessageid" version))
        (sha256
         (base32
          "1qw1f2p4ycqrm5ja4blwv2lllnn8d3jf2ml29pwadlvmivzys4g5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/zopefoundation/zope.i18nmessageid")
    (synopsis "Message Identifiers for internationalization")
    (description
     "This package provides facilities for declaring messages within program
source text; translation of the messages is the responsiblitiy of the
@code{zope.i18n} package.")
    (license zpl2.1)))

(define-public python-zope.schema
  (package
    (name "python-zope.schema")
    (version "4.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.schema" version))
        (sha256
         (base32
          "178631dks473rfsfd46pmqipz7fdkn9bjd35j6qlgavwf2l1v5rd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.event" ,python-zope.event)
       ("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-zope.i18nmessageid" ,python-zope.i18nmessageid)
       ("python-zope.testing" ,python-zope.testing)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.schema")
    (synopsis "zope.interface extension for defining data schemas")
    (description
     "Schemas extend the notion of interfaces to detailed descriptions of
Attributes (but not methods).  Every schema is an interface and specifies the
public fields of an object.  A field roughly corresponds to an attribute of a
Python object.  But a Field provides space for at least a title and a
description.  It can also constrain its value and provide a validation method.
Besides you can optionally specify characteristics such as its value being
read-only or not required.")
    (license zpl2.1)))

(define-public python-py3dns
  (package
    (name "python-py3dns")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "py3dns" version))
        (sha256
         (base32
          "1r25f0ys5p37bhld7m7n4gb0lrysaym3w318w2f8bncq7r3d81qz"))))
    (build-system python-build-system)
    ;; This package wants to read /etc/resolv.conf. We can't patch it without
    ;; removing functionality so we copy from Nix and "just don't build it".
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "setup.py"
               (("import DNS") "")
               (("DNS.__version__") (string-append "\"" ,version "\"")))
             #t)))
       #:tests? #f)) ; Also skip the tests.
    (home-page "https://launchpad.net/py3dns")
    (synopsis "Python 3 DNS library")
    (description "Python 3 DNS library")
    (license license:psfl)))

(define-public python-zope.proxy
  (package
    (name "python-zope.proxy")
    (version "4.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.proxy" version))
        (sha256
         (base32
          "05svkbri0jsavjy5jk36n1iba7z2ilb07zr8r3516765v5snjvdb"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-zope.security" ,python-zope.security-bootstrap)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.proxy")
    (synopsis "Generic Transparent Proxies")
    (description "zope.proxy is implemented via a C extension module, which
lets it do things like lie about its own __class__ that are difficult in pure
Python (and were completely impossible before metaclasses).")
    (license zpl2.1)))

(define-public python-zope.proxy-bootstrap
  (package
    (inherit python-zope.proxy)
    (arguments `(#:tests? #f))
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-zope.testrunner
  (package
    (name "python-zope.testrunner")
    (version "5.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.testrunner" version))
        (sha256
         (base32
          "0w3q66cy4crpj7c0hw0vvvvwf3g931rnvw7wwa20av7yqvv6ajim"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests can't find zope.exceptions.
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-zope.exceptions" ,python-zope.exceptions)
       ("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-zope.testing" ,python-zope.testing)))
    (home-page "https://github.com/zopefoundation/zope.testrunner")
    (synopsis "Zope testrunner script")
    (description "This package provides a flexible test runner with layer
support for the Zope framework.")
    (license zpl2.1)))

(define-public python-zope.testrunner-bootstrap
  (package
    (inherit python-zope.testrunner)
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-zope.exceptions" ,python-zope.exceptions-bootstrap)))
    (properties `((hidden? . #t)))))

(define-public python-zope.exceptions
  (package
    (name "python-zope.exceptions")
    (version "4.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.exceptions" version))
        (sha256
         (base32
          "04bjskwas17yscl8bs3l44maxspw1gdji0zcmr499fs420y9r9az"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "zope-testrunner" "--test-path=src" "\\[]"))))))
    (propagated-inputs
     `(("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-zope.testrunner" ,python-zope.testrunner-bootstrap)))
    (home-page "https://github.com/zopefoundation/zope.exceptions")
    (synopsis "Zope Exceptions")
    (description "This package contains exception exceptions and
 implementations which are so general purpose that they don't belong in Zope
 application-specific packages.")
    (license zpl2.1)))

(define-public python-zope.exceptions-bootstrap
  (package
    (inherit python-zope.exceptions)
    (arguments `(#:tests? #f))
    (propagated-inputs `())
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-zope.testing
  (package
    (name "python-zope.testing")
    (version "4.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.testing" version))
        (sha256
         (base32
          "1sh3c3i0m8n8fnhqiry0bk3rr356i56ry7calmn57s1pvv8yhsyn"))))
    (build-system python-build-system)
    (home-page "https://github.com/zopefoundation/zope.testing")
    (synopsis "Zope testing helpers")
    (description "This package provides a number of Zope testing helpers")
    (license zpl2.1)))

(define-public python-persistent
  (package
    (name "python-persistent")
    (version "4.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "persistent" version))
        (sha256
         (base32
          "0slbvq1m3rilgyhj6i522rsyv592xv9pmvm61mrmgkgf40kfnz69"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cffi" ,python-cffi)
       ("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-manuel" ,python-manuel)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/persistent/")
    (synopsis "Translucent persistent objects")
    (description "This package contains a generic persistence implementation for
Python.  It forms the core protocol for making objects interact
\"transparently\" with a database such as the ZODB.")
    (license zpl2.1)))

(define-public python-zope.location
  (package
    (name "python-zope.location")
    (version "4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.location" version))
        (sha256
         (base32
          "1b40pzl8v00d583d3gsxv1qjdw2dhghlgkbgxl3m07d5r3izj857"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.interface" ,python-zope.interface)
       ("python-zope.proxy" ,python-zope.proxy)
       ("python-zope.schema" ,python-zope.schema)))
    (native-inputs
     `(("python-zope.component" ,python-zope.component-bootstrap)
       ("python-zope.configuration" ,python-zope.configuration-bootstrap)
       ("python-zope.copy" ,python-zope.copy-bootstrap)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.location/")
    (synopsis "Zope Location")
    (description "Zope Location")
    (license zpl2.1)))

(define-public python-zope.location-bootstrap
  (package
    (inherit python-zope.location)
    (arguments `(#:tests? #f))
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-zope.security
  (package
    (name "python-zope.security")
    (version "4.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.security" version))
        (sha256
         (base32
          "1zzaggsq4d9pslzh1h1i9qizsrykrm91iyqzi1dz0vw5rixyaj4l"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.component" ,python-zope.component)
       ("python-zope.i18nmessageid" ,python-zope.i18nmessageid)
       ("python-zope.interface" ,python-zope.interface)
       ("python-zope.location" ,python-zope.location)
       ("python-zope.proxy" ,python-zope.proxy)
       ("python-zope.schema" ,python-zope.schema)))
    (native-inputs
     `(("python-btrees" ,python-btrees)
       ("python-zope.component" ,python-zope.component-bootstrap)
       ("python-zope.configuration" ,python-zope.configuration-bootstrap)
       ("python-zope.location" ,python-zope.location-bootstrap)
       ("python-zope.testing" ,python-zope.testing)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.security")
    (synopsis "Zope Security Framework")
    (description "The security framework provides a generic mechanism to
implement security policies on Python objects.")
    (license zpl2.1)))

(define-public python-zope.security-bootstrap
  (package
    (inherit python-zope.security)
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(;("python-zope.component" ,python-zope.component-bootstrap)
       ("python-zope.i18nmessageid" ,python-zope.i18nmessageid)
       ("python-zope.interface" ,python-zope.interface)
       ;("python-zope.location" ,python-zope.location)
       ("python-zope.proxy" ,python-zope.proxy-bootstrap)
       ("python-zope.schema" ,python-zope.schema)))
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-manuel
  (package
    (name "python-manuel")
    (version "1.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "manuel" version))
        (sha256
         (base32
          "1bdzay7j70fly5fy6wbdi8fbrxjrrlxnxnw226rwry1c8a351rpy"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (native-inputs
     `(("python-zope.testing" ,python-zope.testing)))
    (home-page "https://pypi.python.org/pypi/manuel")
    (synopsis "Build tested documentation")
    (description
     "Manuel lets you mix and match traditional doctests with custom test syntax.")
    (license license:asl2.0)))

(define-public python-zope.copy
  (package
    (name "python-zope.copy")
    (version "4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.copy" version))
        (sha256
         (base32
          "06m75434krl57n6p73c2qj55k5i3fixg887j8ss01ih6zw4rvfs7"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-zope.component" ,python-zope.component-bootstrap)
       ("python-zope.location" ,python-zope.location-bootstrap)
       ("python-zope.testing" ,python-zope.testing)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/zope.copy")
    (synopsis "Pluggable object copying mechanism")
    (description
     "This package provides a pluggable mechanism for copying persistent objects.")
    (license zpl2.1)))

(define-public python-zope.copy-bootstrap
  (package
    (inherit python-zope.copy)
    (arguments `(#:tests? #f))
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-btrees
  (package
    (name "python-btrees")
    (version "4.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "BTrees" version))
        (sha256
         (base32
          "0bmkpg6z5z47p21340nyrfbdv2jkfp80yv085ndgbwaas1zi7ac9"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-persistent" ,python-persistent)
       ("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-persistent" ,python-persistent)
       ("python-transaction" ,python-transaction)
       ("python-zope.testrunner" ,python-zope.testrunner)))
    (home-page "https://github.com/zopefoundation/BTrees")
    (synopsis "Scalable persistent object containers")
    (description
     "This package contains a set of persistent object containers built around a
modified BTree data structure.  The trees are optimized for use inside ZODB's
\"optimistic concurrency\" paradigm, and include explicit resolution of
conflicts detected by that mechanism.")
    (license zpl2.1)))

(define-public python-transaction
  (package
    (name "python-transaction")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "transaction" version))
        (sha256
         (base32
          "17wz1y524ca07vr03yddy8dv0gbscs06dbdywmllxv5rc725jq3j"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-zope.interface" ,python-zope.interface)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/zopefoundation/transaction")
    (synopsis "Transaction management for Python")
    (description "This package contains a generic transaction implementation
for Python.  It is mainly used by the ZODB.")
    (license zpl2.1)))
