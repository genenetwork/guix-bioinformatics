(define-module (gn packages r-shiny)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages javascript))

(define-public r-shiny-gn
  (deprecated-package "r-shiny-gn" r-shiny))

(define-public js-strftime-0.9
  (deprecated-package "js-strftime" js-strftime))

(define-public js-es5-shim-2
  (deprecated-package "js-es5-shim" js-es5-shim))
