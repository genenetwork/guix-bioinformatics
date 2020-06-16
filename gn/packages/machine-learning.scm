(define-module (gn packages machine-learning)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages machine-learning))

(define-public tensorflow-native
  (package
    (inherit tensorflow)
    (name "tensorflow-native")
    (arguments
     (substitute-keyword-arguments (package-arguments tensorflow)
       ((#:substitutable? _ #f) #f)
       ;((#:phases phases)
       ; `(modify-phases ,phases
       ;    (add-after 'unpack 'hardcode-multicore-usage
       ;      (lambda _
       ;        (substitute* "tensorflow/core/protobuf/config.proto"
       ;          ;(("num_threads = 1") "num_threads = 28") ; 56/2 for penguin2
       ;          ;(("intra_op_parallelism_threads = 2")
       ;          ; "intra_op_parallelism_threads = 0")
       ;          ;(("inter_op_parallelism_threads = 5")
       ;          ; "inter_op_parallelism_threads = 0")
       ;          )
       ;        #t))))
       ((#:configure-flags flags)
        `(cons
           "-Dtensorflow_OPTIMIZE_FOR_NATIVE_ARCH=ON"
           (delete "-Dtensorflow_OPTIMIZE_FOR_NATIVE_ARCH=OFF"
                   ,flags)))))))

(define-public tensowflow-native-instead-of-tensorflow
  (package-input-rewriting/spec `(("tensorflow" . ,(const tensorflow-native)))))
