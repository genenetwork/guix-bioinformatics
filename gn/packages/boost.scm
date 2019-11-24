(define-module (gn packages boost)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages boost))

(define-public boost-static
  (package
    (inherit boost)
    (name "boost-static")
    (arguments
     (substitute-keyword-arguments (package-arguments boost)
       ((#:make-flags flags)
        `(cons "link=static" (delete "link=shared" ,flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'provide-libboost_python
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (with-directory-excursion (string-append out "/lib")
                   (symlink "libboost_python27.a" "libboost_python.a"))
                 #t)))))))))
