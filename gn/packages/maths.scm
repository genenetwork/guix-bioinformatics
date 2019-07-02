(define-module (gn packages maths)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages texinfo)
  #:use-module (srfi srfi-1))

(define-public octave-3.4.3
  (package
    (inherit octave-cli)
    (name "octave")
    (version "3.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://gnu/octave/octave-"
                            version ".tar.gz"))
        (sha256
         (base32
          "04lh3crzwpramvfvxq34n2r29pmfl37rssj853nz9a3pyq7wrrir"))
        (patches (search-patches "gnulib-gets.patch"
                                 "octave-nested-class.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments octave-cli)
       ((#:configure-flags cf)
        `(cons "--enable-docs=no" ; docs fail to build
               ,cf))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-configure-script
             (lambda _
               (substitute* '("configure"
                              "src/DLD-FUNCTIONS/__delaunayn__.cc"
                              "src/DLD-FUNCTIONS/__voronoi__.cc"
                              "src/DLD-FUNCTIONS/convhulln.cc")
                 (("qhull/qhull.h") "libqhull/libqhull.h")
                 (("qhull/qhull_a.h") "libqhull/qhull_a.h"))
               (copy-file (assoc-ref %build-inputs "fseeko.c")
                          "libgnu/fseeko.c")
               #t))
           (replace 'configure-makeinfo
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/help.cc"
                 (("\"makeinfo\"")
                  (string-append
                    "\"" (assoc-ref inputs "texinfo") "/bin/makeinfo\"")))
               #t))))))
    (native-inputs
     `(("fseeko.c" ,(origin
                      (method url-fetch)
                      (uri "https://git.savannah.gnu.org/gitweb/?p=gnulib.git;a=blob_plain;f=lib/fseeko.c;hb=d40db5e23197dcd105fa3c0dc6633b51af3c08d9")
                      (file-name "gnulib-fseeko.c-0.0.0-1-d40db5e23")
                      (sha256
                       (base32
                        "1ifa5200pskgdzd6qi4nicwyjjkc415bs403mjrrzv8cdrf6hp3k"))))
       ,@(fold alist-delete (package-native-inputs octave-cli)
               '("lzip"))))
    (inputs
     `(("glpk" ,glpk-4.48)
       ("gperf" ,gperf)
       ,@(fold alist-delete (package-inputs octave-cli)
               ;; suitesparse provides cholmod_common_struct, may need older version
               '("glpk" "suitesparse"))))))

;; 4.49 is the last version with _glp_lpx_simplex exported
;; 4.49 is the version where all lpx_ routines were removed
(define-public glpk-4.48
  (package
    (inherit glpk)
    (version "4.48")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1cddqsdcfwavdklg7hsfifppsry81dx3c17wzk6r22mjjpwcihmb"))))))
