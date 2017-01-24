;; Bioinformatics module

(define-module (gn packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  ;; #:use-module (guix build-system ruby)
  #:use-module (guix build-system r)
  #:use-module (guix build-system trivial)
  #:use-module (gn packages statistics)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages check)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gn packages ldc)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages node)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1))

(define-public contra
  (package
    (name "contra")
    (version "2.0.6")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "mirror://sourceforge/contra-cnv/CONTRA.v" version ".tar.gz"))
      (sha256
       (base32
        "0agpcm2xh5f0i9n9sx1kvln6mzdksddmh11bvzj6bh76yw5pnw91"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("python" ,python-2)
       ("r" ,r)
       ;; ("r-dnacopy" ,r-dnacopy) <-- missing in Pjotr's tree
       ("bedtools" ,bedtools)
       ("samtools" ,samtools)))
    (arguments
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build) ; We can use Guix's BEDtools instead.
         (replace 'install
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/contra")))
               (mkdir-p bin)
               (mkdir-p doc)
               (and
                (zero? (system* "cp" "--recursive" "scripts" bin))
                (zero? (system* "cp" "contra.py" bin))
                (zero? (system* "cp" "baseline.py" bin))
                ;; There's only a pre-built PDF available.
                (zero? (system* "cp" "CONTRA_User_Guide.2.0.pdf" doc)))))))))
    (home-page "http://contra-cnv.sourceforge.net/")
    (synopsis "Tool for copy number variation (CNV) detection for targeted
resequencing data")
    (description "CONTRA is a tool for copy number variation (CNV) detection
for targeted resequencing data such as those from whole-exome capture data.
CONTRA calls copy number gains and losses for each target region with key
strategies including the use of base-level log-ratios to remove GC-content
bias, correction for an imbalanced library size effect on log-ratios, and the
estimation of log-ratio variations via binning and interpolation.  It takes
standard alignment formats (BAM/SAM) and outputs in variant call format
(VCF 4.0) for easy integration with other next generation sequencing analysis
package.")
    (license license:gpl3+)))

(define boost-delly
  (package (inherit boost)
    (name "boost-delly")
    (version "1.57.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "0rs94vdmg34bwwj23fllva6mhrml2i7mvmlb11zyrk1k5818q34i"))))))

(define-public delly
  (package
    (name "delly")
    (version "0.7.2")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/tobiasrausch/delly/archive/v"
            version ".tar.gz"))
      (sha256
       (base32 "173mmg43dbxqkyq0kiffz63xbmggr2kzd55mwxci9yfh5md1zprn"))
      (patches (list (search-patch "delly-use-system-libraries.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     `(("boost" ,boost-delly) ; Use version 1.57.0 instead.
       ("htslib" ,htslib)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)))
    (arguments
     `(#:tests? #f ; There are no tests to run.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda _
             (let ((bin (string-append (assoc-ref %outputs "out") "/bin")))
               (install-file "src/cov" bin)
               (install-file "src/delly" bin)
               (install-file "src/extract" bin)
               (install-file "src/iover" bin)
               (install-file "src/stats" bin)))))))
    (home-page "https://github.com/tobiasrausch/delly")
    (synopsis "Integrated structural variant prediction method")
    (description "Delly is an integrated structural variant prediction method
that can discover and genotype deletions, tandem duplications, inversions and
translocations at single-nucleotide resolution in short-read massively parallel
sequencing data.  It uses paired-ends and split-reads to sensitively and
accurately delineate genomic rearrangements throughout the genome.  Structural
variants can be visualized using Delly-maze and Delly-suave.")
    (license license:gpl3)))

(define-public freec
  (package
    (name "control-freec")
    (version "8.7")
    (source (origin
      (method url-fetch)
      (uri "http://bioinfo-out.curie.fr/projects/freec/src/FREEC_Linux64.tar.gz")
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "12sl7gxbklhvv0687qjhml1z4lwpcn159zcyxvawvclsrzqjmv0h"))))
    (build-system gnu-build-system)
    ;; The source code's filename indicates only a 64-bit Linux build.
    ;; We need to investigate whether this is true.
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There's no configure phase because there are no external
         ;; dependencies.
         (delete 'configure)
         ;; There are no tests.
         (delete 'check)
         (replace
          'unpack
          (lambda* (#:key source #:allow-other-keys)
            (and
             (zero? (system* "mkdir" "source"))
             (with-directory-excursion "source"
               (zero? (system* "tar" "xvf" source))))))
         (replace
          'build
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "source"
              (zero? (system* "make")))))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "source/freec" bin)))))))
    (home-page "http://bioinfo-out.curie.fr/projects/freec/")
    (synopsis "Tool for detection of copy-number changes and allelic imbalances
(including LOH) using deep-sequencing data")
    (description "Control-FREEC automatically computes, normalizes, segments
copy number and beta allele frequency (BAF) profiles, then calls copy number
alterations and LOH.  The control (matched normal) sample is optional for whole
genome sequencing data but mandatory for whole exome or targeted sequencing
data.  For whole genome sequencing data analysis, the program can also use
mappability data (files created by GEM). ")
    (license license:gpl2+)))

(define-public tabixpp
  (package
   (name "tabixpp")
   (version "1.0.0")
   (source (origin
     (method url-fetch)
     (uri (string-append "https://github.com/ekg/tabixpp/archive/v"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1s0lgks7qlvlhvcjhi2wm18nnza1bwcnic44ij7z8wfg88h4ivwn"))))
   (build-system gnu-build-system)
   (inputs
    `(("htslib" ,htslib)
      ("zlib" ,zlib)))
   (arguments
    `(#:tests? #f ; There are no tests to run.
      #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is no configure phase.
        ;; The build phase needs overriding the location of htslib.
        (replace 'build
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((htslib-ref (assoc-ref inputs "htslib")))
              (zero?
               (system* "make"
                 (string-append "HTS_LIB=" htslib-ref "/lib/libhts.a")
                 "HTS_HEADERS=" ; No need to check for headers here.
                 (string-append "LIBPATH=-L. -L" htslib-ref "/include"))))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "tabix++" bin)))))))
   (home-page "https://github.com/ekg/tabixpp")
   (synopsis "C++ wrapper around tabix project")
   (description "This is a C++ wrapper around the Tabix project which abstracts
some of the details of opening and jumping in tabix-indexed files.")
   (license license:expat)))

;; This version works with FreeBayes while the released version doesn't. The
;; released creates a variable with the name "vcf" somewhere, which is also the
;; name of a namespace in vcflib.
(define-public tabixpp-freebayes
  (let ((commit "bbc63a49acc52212199f92e9e3b8fba0a593e3f7"))
    (package (inherit tabixpp)
      (name "tabixpp-freebayes")
      (version (string-append "0-1." (string-take commit 7)))
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ekg/tabixpp/archive/"
                            commit ".tar.gz"))
        (file-name (string-append name "-" version "-checkout.tar.gz"))
        (sha256
         (base32 "1s06wmpgj4my4pik5kp2lc42hzzazbp5ism2y4i2ajp2y1c68g77")))))))

(define-public smithwaterman
  ;; TODO: Upgrading smithwaterman breaks FreeBayes.
  (let ((commit "203218b47d45ac56ef234716f1bd4c741b289be1"))
    (package
      (name "smithwaterman")
      (version (string-append "0-1." (string-take commit 7)))
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ekg/smithwaterman/archive/"
                            commit ".tar.gz"))
        (file-name (string-append name "-" version "-checkout.tar.gz"))
        (sha256
         (base32 "1lkxy4xkjn96l70jdbsrlm687jhisgw4il0xr2dm33qwcclzzm3b"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "smithwaterman" bin)))))))
      (home-page "https://github.com/ekg/smithwaterman")
      (synopsis "Implementation of the Smith-Waterman algorithm")
      (description "Implementation of the Smith-Waterman algorithm.")
      ;; The project contains a license file for the GPLv2.  The source files
      ;; do not contain a license notice, so GPLv2-only is assumed here.
      (license license:gpl2))))

(define-public multichoose
  (package
    (name "multichoose")
    (version "1.0.3")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/ekg/multichoose/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "0xy86vvr3qrs4l81qis7ia1q2hnqv0xcb4a1n60smxbhqqis5w3l"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)
       ("node" ,node)))
    (arguments
     `(#:tests? #f ; There are no tests to run.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               ;; TODO: There are Python modules for these programs too.
               (install-file "multichoose" bin)
               (install-file "multipermute" bin)))))))
    (home-page "https://github.com/ekg/multichoose")
    (synopsis "Library for efficient loopless multiset combination generation
algorithm")
    (description "A library implements an efficient loopless multiset
combination generation algorithm which is (approximately) described in
\"Loopless algorithms for generating permutations, combinations, and other
combinatorial configurations.\" G Ehrlich - Journal of the ACM (JACM),
1973. (Algorithm 7.)")
    (license license:expat)))

(define-public fsom
  (let ((commit "a6ef318fbd347c53189384aef7f670c0e6ce89a3"))
    (package
      (name "fsom")
      (version (string-append "0-1." (string-take commit 7)))
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ekg/fsom/archive/"
                            "a6ef318fbd347c53189384aef7f670c0e6ce89a3" ".tar.gz"))
        (file-name (string-append name "-" version "-checkout.tar.gz"))
        (sha256
         (base32 "0q6b57ppxfvsm5cqmmbfmjpn5qvx2zi5pamvp3yh8gpmmz8cfbl3"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "fsom" bin)))))))
      (home-page "https://github.com/ekg/fsom")
      (synopsis "Program for managing SOM (Self-Organizing Maps) neural networks")
      (description "Program for managing SOM (Self-Organizing Maps) neural networks.")
      (license license:gpl3))))

(define-public filevercmp
  (let ((commit "1a9b779b93d0b244040274794d402106907b71b7"))
    (package
      (name "filevercmp")
      (version (string-append "0-1." (string-take commit 7)))
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ekg/filevercmp/archive/"
                            commit ".tar.gz"))
        (file-name "filevercmp-src.tar.gz")
        (sha256
         (base32 "0yp5jswf5j2pqc6517x277s4s6h1ss99v57kxw9gy0jkfl3yh450"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "filevercmp" bin)))))))
      (home-page "https://github.com/ekg/filevercmp")
      (synopsis "Program to compare version strings")
      (description "A program to compare version strings.  It intends to be a
replacement for strverscmp.")
      (license license:gpl3+))))

(define-public fastahack
  (let ((commit "c68cebb4f2e5d5d2b70cf08fbdf1944e9ab2c2dd"))
    (package
      (name "fastahack")
      (version (string-append "0-1." (string-take commit 7)))
      (source (origin
        (method url-fetch)
        (uri (string-append "https://github.com/ekg/fastahack/archive/"
                            commit ".tar.gz"))
        (file-name (string-append name "-" version "-checkout.tar.gz"))
        (sha256
         (base32 "0j25lcl3jk1kls66zzxjfyq5ir6sfcvqrdwfcva61y3ajc9ssay2"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; There are no tests to run.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; There is no configure phase.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "fastahack" bin)))))))
      (home-page "https://github.com/ekg/fastahack")
      (synopsis "Program for indexing and sequence extraction from FASTA files")
      (description "Fastahack is a small application for indexing and extracting
sequences and subsequences from FASTA files.  The included Fasta.cpp library
provides a FASTA reader and indexer that can be embeddedinto applications which
would benefit from directly reading subsequences from FASTA files.  The library
automatically handles index file generation and use.")
      ;; There is no specific license for fastahack.
      ;; A part of the program is licensed GPLv2.
      (license (list license:non-copyleft license:gpl2)))))

(define-public vcflib
  (let ((commit "3ce827d8ebf89bb3bdc097ee0fe7f46f9f30d5fb"))
    (package
      (name "vcflib")
      (version (string-append "1.0.2-1." (string-take commit 7)))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/vcflib/vcflib/archive/"
                "5ac091365fdc716cc47cc5410bb97ee5dc2a2c92" ".tar.gz"))
         (file-name "vcflib-5ac0913.tar.gz")
         (sha256
          (base32 "0ywshwpif059z5h0g7zzrdfzzdj2gr8xvwlwcsdxrms3p9iy35h8"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("htslib" ,htslib)
         ("zlib" ,zlib)
         ("python" ,python-2)
         ("perl" ,perl)
         ("r" ,r)
         ("node" ,node)
         ("tabixpp-src" ,(package-source tabixpp-freebayes))
         ("smithwaterman-src" ,(package-source smithwaterman))
         ("multichoose-src" ,(package-source multichoose))
         ("fsom-src" ,(package-source fsom))
         ("filevercmp-src" ,(package-source filevercmp))
         ("fastahack-src" ,(package-source fastahack))
         ("intervaltree-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ekg/intervaltree/archive/"
                   "dbb4c513d1ad3baac516fc1484c995daf9b42838" ".tar.gz"))
             (file-name "intervaltree-src.tar.gz")
             (sha256
              (base32 "19prwpn2wxsrijp5svfqvfcxl5nj7zdhm3jycd5kqhl9nifpmcks"))))))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                        (assoc-ref inputs source)
                                        "--strip-components=1"))))))
                 (and
                  (unpack "intervaltree-src" "intervaltree")
                  (unpack "fastahack-src" "fastahack")
                  (unpack "filevercmp-src" "filevercmp")
                  (unpack "fsom-src" "fsom")
                  (unpack "multichoose-src" "multichoose")
                  (unpack "smithwaterman-src" "smithwaterman")
                  (unpack "tabixpp-src" "tabixpp")))))
           (add-after 'unpack-submodule-sources 'fix-makefile
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("Makefile")
                 (("^GIT_VERSION.*") "GIT_VERSION = v1.0.0"))))
           (replace
            'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (with-directory-excursion "tabixpp"
                (zero? (system* "make")))
              (zero? (system* "make" "CC=gcc"
                (string-append "CFLAGS=\"" "-Itabixpp "
                  "-I" (assoc-ref inputs "htslib") "/include " "\"") "all"))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    ;;(include (string-append (assoc-ref outputs "out") "/include"))
                    (lib (string-append (assoc-ref outputs "out") "/lib")))
                (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "bin" ".*"))
                ;; The header files do not correspond to libvcflib.a, therefore
                ;; I left them out.
                ;;(for-each (lambda (file)
                ;;           (install-file file include))
                ;;         (find-files "src" "\\.h$"))
                (install-file "libvcflib.a" lib)))))))
      (home-page "https://github.com/vcflib/vcflib/")
      (synopsis "Library for parsing and manipulating VCF files")
      (description "Vcflib provides methods to manipulate and interpret
sequence variation as it can be described by VCF. It is both an API for parsing
and operating on records of genomic variation as it can be described by the VCF
format, and a collection of command-line utilities for executing complex
manipulations on VCF files.")
      (license license:expat))))

(define-public bash-tap
  (package
    (name "bash-tap")
    (version "1.0.2")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/illusori/bash-tap/archive/"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "0qs1qi38bl3ns4mpagcawv618dsk2q1lgrbddgvs0wl3ia12cyz5"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)
                     ("tar" ,tar)
                     ("gzip" ,gzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                         (path (string-append (assoc-ref %build-inputs "gzip") "/bin"))
                         (bin (string-append %output "/bin"))
                         (source (string-append (assoc-ref %build-inputs "source"))))
                     (setenv "PATH" path)
                     (mkdir-p bin)
                     (with-directory-excursion bin
                       (zero? (system* tar "xvf" source
                                       "--strip-components=1"
                                       "--no-anchored"
                                       "bash-tap"
                                       "bash-tap-bootstrap"
                                       "bash-tap-mock")))))))
    (home-page "http://www.illusori.co.uk/projects/bash-tap/")
    (synopsis "Bash port of a Test::More/Test::Builder-style TAP-compliant
test library")
    (description "Bash TAP is a TAP-compliant Test::More-style testing library
for Bash shell scripts and functions.  Along with the Test::More-style testing
helpers it provides helper functions for mocking commands and functions and
in-process output capturing.")
    ;; The author didn't specify a license.
    (license license:public-domain)))

(define-public freebayes
  (let ((commit "3ce827d8ebf89bb3bdc097ee0fe7f46f9f30d5fb")
        (revision "1"))
    (package
      (name "freebayes")
      (version (string-append "1.0.2-" revision "." (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/ekg/freebayes.git")
          (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32 "1sbzwmcbn78ybymjnhwk7qc5r912azy5vqz2y7y81616yc3ba2a2"))))
      (build-system gnu-build-system)
      (inputs
       `(("zlib" ,zlib)
         ("htslib" ,htslib)))
      (native-inputs
       `(("bc" ,bc) ; Needed for running tests.
         ("samtools" ,samtools) ; Needed for running tests.
         ("parallel" ,parallel) ; Needed for running tests.
         ("procps" ,procps) ; Needed for running tests.
         ("bamtools" ,bamtools)
         ("cmake" ,cmake)
         ("python" ,python-2)
         ("node" ,node)
         ("r" ,r)
         ("perl" ,perl)
         ("bamtools-src" ,(package-source bamtools))
         ("vcflib-src" ,(package-source vcflib))
         ;; These are submodules for the vcflib version used in freebayes
         ("tabixpp-src" ,(package-source tabixpp-freebayes))
         ("smithwaterman-src" ,(package-source smithwaterman))
         ("multichoose-src" ,(package-source multichoose))
         ("fsom-src" ,(package-source fsom))
         ("filevercmp-src" ,(package-source filevercmp))
         ("fastahack-src" ,(package-source fastahack))
         ("intervaltree-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ekg/intervaltree/archive/"
                   "dbb4c513d1ad3baac516fc1484c995daf9b42838" ".tar.gz"))
             (file-name "intervaltree-src.tar.gz")
             (sha256
              (base32 "19prwpn2wxsrijp5svfqvfcxl5nj7zdhm3jycd5kqhl9nifpmcks"))))
         ;; These submodules are needed to run the tests.
         ("bash-tap-src" ,(package-source bash-tap))
          ;; ,(origin
          ;;   (method url-fetch)
          ;;   (uri (string-append "https://github.com/illusori/bash-tap/archive/"
          ;;                       "c38fbfa401600cc81ccda66bfc0da3ea56288d03" ".tar.gz"))
          ;;   (file-name "bash-tap-src.tar.gz")
          ;;   (sha256
          ;;    (base32 "07ijb1p0aa65ajpg9nkghc183iha6lwiydkckay8pghapa01j6nz"))))
         ("test-simple-bash-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ingydotnet/test-simple-bash/archive/"
                                "124673ff204b01c8e96b7fc9f9b32ee35d898acc" ".tar.gz"))
            (file-name "test-simple-bash-src.tar.gz")
            (sha256
             (base32 "016xf3wbgqbav9dncvfdx5k0f10z5xwq8jdszajzmcvnhz5wis14"))))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                                 (assoc-ref inputs source)
                                                 "--strip-components=1"))))))
                 (and
                  (unpack "bamtools-src" "bamtools")
                  (unpack "vcflib-src" "vcflib")
                  ;;(unpack "intervaltree-src" "intervaltree")
                  (unpack "fastahack-src" "vcflib/fastahack")
                  (unpack "filevercmp-src" "vcflib/filevercmp")
                  (unpack "fsom-src" "vcflib/fsom")
                  (unpack "intervaltree-src" "vcflib/intervaltree")
                  (unpack "multichoose-src" "vcflib/multichoose")
                  (unpack "smithwaterman-src" "vcflib/smithwaterman")
                  (unpack "tabixpp-src" "vcflib/tabixpp")
                  (unpack "test-simple-bash-src" "test/test-simple-bash")
                  (unpack "bash-tap-src" "test/bash-tap")))))
           (add-after 'unpack-submodule-sources 'fix-makefile
             (lambda* (#:key inputs #:allow-other-keys)
               ;; We don't have the .git folder to get the version tag from.
               ;; For this checkout of the code, it's v1.0.0.
               (substitute* '("vcflib/Makefile")
                 (("^GIT_VERSION.*") "GIT_VERSION = v1.0.0"))))
           (replace 'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (and
               ;; Compile Bamtools before compiling the main project.
               (with-directory-excursion "bamtools"
                 (system* "mkdir" "build")
                 (with-directory-excursion "build"
                   (and (zero? (system* "cmake" "../"))
                        (zero? (system* "make")))))
               ;; Compile vcflib before we compiling the main project.
               (with-directory-excursion "vcflib"
                 (with-directory-excursion "tabixpp"
                   (let ((htslib-ref (assoc-ref inputs "htslib")))
                     (zero?
                      (system* "make" "HTS_HEADERS="
                               (string-append "HTS_LIB=" htslib-ref "/lib/libhts.a")
                               (string-append "LIBPATH=-L. -L" htslib-ref "/include")))))
                 (zero? (system* "make" "CC=gcc"
                   (string-append "CFLAGS=\"" "-Itabixpp "
                     "-I" (assoc-ref inputs "htslib") "/include " "\"") "all")))
               (with-directory-excursion "src"
                 (zero? (system* "make"))))))
           (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "bin/freebayes" bin)
                (install-file "bin/bamleftalign" bin))))
           ;; There are three tests that fail.  All because of the -P
           ;; (--perl-regexp) option in grep, which is not compiled into the
           ;; version of grep in Guix.
           (replace 'check
            (lambda* (#:key inputs #:allow-other-keys)
              (system* "make" "test"))))))
      (home-page "https://github.com/ekg/freebayes")
      (synopsis "Haplotype-based variant detector")
      (description "FreeBayes is a Bayesian genetic variant detector designed to
find small polymorphisms, specifically SNPs (single-nucleotide polymorphisms),
indels (insertions and deletions), MNPs (multi-nucleotide polymorphisms), and
complex events (composite insertion and substitution events) smaller than the
length of a short-read sequencing alignment.")
      (license license:expat))))

(define-public r-biocpreprocesscore
  (package
    (name "r-biocpreprocesscore")
    (version "1.34.0")
    (source (origin
              (method url-fetch)
              (uri (bioconductor-uri "preprocessCore" version))
              (sha256
               (base32
                "0bcfi26ahs4ybagvz29vlp1wldhhbwwh73xz6v6xb07nakyzb6cf"))))
    (properties
     `((upstream-name . "BiocpreprocessCore")
       (r-repository . bioconductor)))
    (build-system r-build-system)
    (home-page "http://bioconductor.org/packages/preprocessCore")
    (synopsis "Preprocess functions for Bioconductor")
    (description
     "A library of core preprocessing routines.")
    (license license:lgpl2.0+)))

(define-public r-wgcna
  (let ((commit "425bc170cc0873ddbd414675ac40f6d4d724c7cb"))
(package
  (name "r-wgcna")
  (version (string-append "1.49-" commit))
  (source (origin
           (method git-fetch)
           (uri (git-reference
                 ;; (url "https://github.com/genenetwork/WGCNA.git")
                 (url "https://github.com/pjotrp/WGCNA.git")
                 (commit commit)))
           (file-name (string-append name "-" commit))
           (sha256
            (base32
             "1zqnsb8s3065rq1y2y3l79zi8wmdwjkcjls96ypycrb7pmdil58j"))))
  (properties `((upstream-name . "WGCNA")))
  (build-system r-build-system)
  (propagated-inputs
   `( ;; ("r-annotationdbi" ,r-annotationdbi)
     ; ("r-biocparallel" ,r-biocparallel)
     ("r-doparallel" ,r-doparallel)
     ("r-dynamictreecut" ,r-dynamictreecut)
     ("r-fastcluster" ,r-fastcluster)
     ("r-foreach" ,r-foreach)
     ("r-go-db" ,r-go-db)
     ; ("r-grdevices" ,r-grdevices)
     ("r-hmisc" ,r-hmisc)
     ("r-impute" ,r-impute)
     ("r-matrixstats" ,r-matrixstats)
     ; ("r-parallel" ,r-parallel)
     ("r-biocpreprocesscore" ,r-biocpreprocesscore)
     ; ("r-splines" ,r-splines)
     ; ("r-stats" ,r-stats)
     ; ("r-survival" ,r-survival)
     ; ("r-utils" ,r-utils)
     ))
    (arguments
     `(
       #:tests? #f))   ; no 'setup.py test'
  (home-page
    "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
  (synopsis
    "Weighted gene correlation network analysis (wgcna)")
  (description
    "Functions necessary to perform Weighted Correlation Network
Analysis on high-dimensional data.  Includes functions for rudimentary
data cleaning, construction of correlation networks, module
identification, summarization, and relating of variables and modules
to sample traits.  Also includes a number of utility functions for
data manipulation and visualization.")
  (license license:gpl2+))))

(define-public plink2
  (package
    (name "plink2")
    (version "1.90b3")
    (source
     (origin
      (method url-fetch)
      ;; https://github.com/chrchang/plink-ng/archive/v1.90b3.tar.gz
       (uri (string-append
             "https://github.com/chrchang/plink-ng/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "03fzib1al5qkr9vxv63wxmv6y2pfb1rmir0h8jpi72r87hczqjig"))
       (patches (list (search-patch "plink-ng-Makefile-zlib.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (zero? (system* "make" "-f" "Makefile.std"))
                   ))
        (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink2" bin)
                      #t))))))
    (inputs
     `(("zlib" ,zlib)
       ("openblas" ,openblas)
       ;; ("atlas" ,atlas)
       ;; ("lapack" ,lapack)
       ("gfortran" ,gfortran)
       ))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://www.cog-genomics.org/plink2")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    ;; Code is released under GPLv2, except for fisher.h, which is under
    ;; LGPLv2.1+
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public plink-ng
  (let ((commit "5d1db4313ba0cc976562da233db4aced78975d10"))
  (package
    (name "plink-ng")
    (version (string-append "1.90b3-" commit )) ; Aug 11, 2016
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/plink-ng.git")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "1366li3ks9076bblvd1rpzkjq4j8f8f08lhga4c1ckrkil3xww4m"))))
            ;; no longer (patches (list (search-patch "plink-ng-Makefile-zlib-git.patch")))))
    (inputs
     `(("zlib" ,zlib)
       ("openblas" ,openblas)
       ;; ("atlas" ,atlas) ; openblas replaces atlas
       ("lapack" ,lapack)  ; lapack is disabled in GUIX openblas
       ;; ("gfortran" ,gfortran)
       ;; ("python" ,python-2)   ;; for tests - currently disabled
       ))
    (native-inputs
     `(("unzip" ,unzip)))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no "check" target. Some of the python-based tests fail
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (zero? (system* "make" "-f" "Makefile.guix"))
                   ))
        (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out")
                                              "/bin/")))
                      (install-file "plink2" bin)
                      #t))))))
    (home-page "https://www.cog-genomics.org/plink2")
    (synopsis "Whole genome association analysis toolset")
    (description
     "PLINK is a whole genome association analysis toolset, designed to
perform a range of basic, large-scale analyses in a computationally efficient
manner.  The focus of PLINK is purely on analysis of genotype/phenotype data,
so there is no support for steps prior to this (e.g. study design and
planning, generating genotype or CNV calls from raw data).  Through
integration with gPLINK and Haploview, there is some support for the
subsequent visualization, annotation and storage of results.")
    (license license:gpl3+))))

(define-public gemma-git
  (let ((commit "2de4bfab3"))
  (package
    (name "gemma-git")
    (version (string-append "0.9.5-" commit ))
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/genenetwork/GEMMA.git")
                   (commit commit)))
             (file-name (string-append name "-" commit))
             (sha256
              (base32
               "1drffdgwbzgiw9sf55ghl3zjv58f8i9kfz0zys5mp6n06syp4ira"))))
    (inputs `(
              ("gsl" ,gsl)
              ("lapack" ,lapack)
              ("zlib" ,zlib)
              ))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '(" FORCE_DYNAMIC=1")
       #:phases
        (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'bin-mkdir
                     (lambda _
                       (mkdir-p "bin")
                       ))
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                           (let ((out (assoc-ref outputs "out")))
                             (install-file "bin/gemma" (string-append out "/bin"))))))
       #:tests? #f))
    (home-page "")
    (synopsis "Tool for genome-wide efficient mixed model association")
    (description "GEMMA is the software implementing the Genome-wide
Efficient Mixed Model Association algorithm for a standard linear
mixed model and some of its close relatives for genome-wide
association studies (GWAS).")
    (license license:gpl3))))

(define-public sambamba
  (let ((commit "5a33d571339c966477c1f70ed08f64051f7b41c1"))
    (package
      (name "sambamba")
      (version (string-append "0.6.5-" (string-take commit 7)))
      (source (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pjotrp/sambamba.git")
              (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "05nlhwjw17igcwiz4pq0r4f8flrqcy4065fhx4nhpc0g65p70mi5"))))
      (build-system gnu-build-system)
      (outputs '("out"
                 "debug"))  ;retain debug symbols - note that -O2 is used
      (native-inputs
       `(("ldc" ,ldc)
         ("lz4" ,lz4)
         ("coreutils" ,coreutils) ; for env
         ("zlib" ,zlib)
         ("perl" ,perl) ; Needed for htslib
         ("ruby" ,ruby) ; Needed for htslib
         ("python" ,python-2) ; Needed for htslib
         ("gcc" ,gcc)
         ("which" ,which)
         ("htslib-src"
          ,(origin
             (method url-fetch)
             (uri "https://github.com/lomereiter/htslib/archive/2f3c3ea7b301f9b45737a793c0b2dcf0240e5ee5.tar.gz")
             ;;(uri "https://github.com/samtools/htslib/archive/1.3.tar.gz")
             (file-name "htslib-0.2.0-rc10-271-g2f3c3ea-dirty.tar.gz")
             (sha256
              (base32 "0bl6w856afnbgdsw8bybsxpqsyf2ba3f12rqh47hhpxvv866g08w"))))
              ;;(base32 "1bqkif7yrqmiqak5yb74kgpb2lsdlg7y344qa1xkdg7k1l4m86i9"))
             ;;(patches (list (search-patch "htslib-add-cram_to_bam.patch")))))
         ("biod-src"
          ,(origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/biod/BioD.git")
                   (commit "1248586b54af4bd4dfb28ebfebfc6bf012e7a587")))
             (file-name (string-append "biod-src-" (string-take commit 7) ".tar.gz"))
             (sha256
              (base32 "1m8hi1n7x0ri4l6s9i0x6jg4z4v94xrfdzp7mbizdipfag0m17g3"))))))
      (arguments
       `(#:tests? #f  ; no tests available
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-after 'unpack 'unpack-htslib-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The current build compiles htslib statically into the
               ;; executable.  On top of that, we need to patch the latest
               ;; version of htslib to have it working with Sambamba.
               (and (with-directory-excursion "htslib"
                      (zero? (system* "tar" "xvf" (assoc-ref inputs "htslib-src")
                                      "--strip-components=1")))
                    (zero? (system* "rm" "-r" "BioD"))
                    (zero? (system* "ln" "--symbolic" "--no-target-directory"
                                    (assoc-ref inputs "biod-src") "BioD")))))
           (replace
            'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (zero? (system* "make" "-f" "Makefile.guix" "sambamba-ldmd2"
                       (string-append "LDC_LIB_PATH="
                                             (assoc-ref inputs "ldc")
                                             "/lib")))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (install-file "build/sambamba" bin)))))))
      (home-page "https://github.com/lomereiter/sambamba")
      (synopsis "Fast tool for working with SAM, BAM and CRAM files written in D.")
      (description
       "Sambamba is a high performance modern robust and fast
tool (and library), written in the D programming language, for working
with SAM, BAM and CRAM files.  Current parallelised functionality is
an important subset of samtools functionality, including view, index,
sort, markdup, and depth.")
      (license license:gpl2+))))

(define-public vcflib
  (let ((commit "3ce827d8ebf89bb3bdc097ee0fe7f46f9f30d5fb"))
    (package
      (name "vcflib")
      (version (string-append "v1.0.2-" (string-take commit 7)))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/vcflib/vcflib/archive/"
                "5ac091365fdc716cc47cc5410bb97ee5dc2a2c92" ".tar.gz"))
         (file-name "vcflib-5ac0913.tar.gz")
         (sha256
          (base32 "0ywshwpif059z5h0g7zzrdfzzdj2gr8xvwlwcsdxrms3p9iy35h8"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("htslib" ,htslib)
         ("zlib" ,zlib)
         ("python" ,python-2)
         ("perl" ,perl)
         ("tabixpp-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ekg/tabixpp/archive/"
                  "bbc63a49acc52212199f92e9e3b8fba0a593e3f7" ".tar.gz"))
            (file-name "tabixpp-src.tar.gz")
            (sha256
             (base32 "1s06wmpgj4my4pik5kp2lc42hzzazbp5ism2y4i2ajp2y1c68g77"))))
         ("intervaltree-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ekg/intervaltree/archive/"
                   "dbb4c513d1ad3baac516fc1484c995daf9b42838" ".tar.gz"))
             (file-name "intervaltree-src.tar.gz")
             (sha256
              (base32 "19prwpn2wxsrijp5svfqvfcxl5nj7zdhm3jycd5kqhl9nifpmcks"))))
         ("smithwaterman-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ekg/smithwaterman/archive/"
                  "203218b47d45ac56ef234716f1bd4c741b289be1" ".tar.gz"))
            (file-name "smithwaterman-src.tar.gz")
            (sha256
             (base32 "1lkxy4xkjn96l70jdbsrlm687jhisgw4il0xr2dm33qwcclzzm3b"))))
         ("multichoose-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ekg/multichoose/archive/"
                  "73d35daa18bf35729b9ba758041a9247a72484a5" ".tar.gz"))
            (file-name "multichoose-src.tar.gz")
            (sha256
             (base32 "07aizwdabmlnjaq4p3v0vsasgz1xzxid8xcxcw3paq8kh9c1099i"))))
         ("fsom-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ekg/fsom/archive/"
                  "a6ef318fbd347c53189384aef7f670c0e6ce89a3" ".tar.gz"))
            (file-name "fsom-src.tar.gz")
            (sha256
             (base32 "0q6b57ppxfvsm5cqmmbfmjpn5qvx2zi5pamvp3yh8gpmmz8cfbl3"))))
         ("filevercmp-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ekg/filevercmp/archive/"
                  "1a9b779b93d0b244040274794d402106907b71b7" ".tar.gz"))
            (file-name "filevercmp-src.tar.gz")
            (sha256
             (base32 "0yp5jswf5j2pqc6517x277s4s6h1ss99v57kxw9gy0jkfl3yh450"))))
         ("fastahack-src"
          ,(origin
            (method url-fetch)
            (uri (string-append "https://github.com/ekg/fastahack/archive/"
                  "c68cebb4f2e5d5d2b70cf08fbdf1944e9ab2c2dd" ".tar.gz"))
            (file-name "fastahack-src.tar.gz")
            (sha256
             (base32 "0j25lcl3jk1kls66zzxjfyq5ir6sfcvqrdwfcva61y3ajc9ssay2"))))))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                        (assoc-ref inputs source)
                                        "--strip-components=1"))))))
                 (and
                  (unpack "intervaltree-src" "intervaltree")
                  (unpack "fastahack-src" "fastahack")
                  (unpack "filevercmp-src" "filevercmp")
                  (unpack "fsom-src" "fsom")
                  (unpack "intervaltree-src" "intervaltree")
                  (unpack "multichoose-src" "multichoose")
                  (unpack "smithwaterman-src" "smithwaterman")
                  (unpack "tabixpp-src" "tabixpp")))))
           (add-after 'unpack-submodule-sources 'fix-makefile
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("Makefile")
                 (("^GIT_VERSION.*") "GIT_VERSION = v1.0.0"))))
           (replace
            'build
            (lambda* (#:key inputs make-flags #:allow-other-keys)
              (with-directory-excursion "tabixpp"
                (zero? (system* "make")))
              (zero? (system* "make" "CC=gcc"
                (string-append "CFLAGS=\"" "-Itabixpp "
                  "-I" (assoc-ref inputs "htslib") "/include " "\"") "all"))))
           (replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (lib (string-append (assoc-ref outputs "out") "/lib")))
                (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "bin" ".*"))
                (install-file "libvcflib.a" lib)))))))
      (home-page "https://github.com/vcflib/vcflib/")
      (synopsis "Library for parsing and manipulating VCF files")
      (description "Vcflib provides methods to manipulate and interpret
sequence variation as it can be described by VCF. It is both an API for parsing
and operating on records of genomic variation as it can be described by the VCF
format, and a collection of command-line utilities for executing complex
manipulations on VCF files.")
      (license license:expat))))

(define-public pindel
  (package
   (name "pindel")
   (version "0.2.5b8")
   (source (origin
     (method url-fetch)
     (uri (string-append "https://github.com/genome/pindel/archive/v"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "06bsf0psxwf7h5p3j97xkh9k5qrwhxh6xn942y1j1m2inyhgs8bz"))))
   (build-system gnu-build-system)
   (inputs
    `(("samtools" ,samtools)
      ("htslib" ,htslib)
      ("zlib" ,zlib)))
   (native-inputs
    `(("cppcheck" ,cppcheck)
      ("python" ,python-2)
      ("perl" ,perl)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (delete 'configure) ; There is no configure phase.
        ;; The build phase needs to run 'make' twice for the reasons described
        ;; below.
        (replace 'build
          (lambda* (#:key inputs #:allow-other-keys)
            ;; The first run creates a Makefile.local file.  Make will report
            ;; the failure to find Makefile.local, but we can ignore this error.
            (system* "make" (string-append "SAMTOOLS=" (assoc-ref inputs "samtools")))
            ;; The second run actually compiles the program.  Now Makefile.local
            ;; is available, and we should treat an exiting make with an error as
            ;; a true error.
            (zero? (system* "make"))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "src/pindel" bin)
              (install-file "src/pindel2vcf" bin)
              (install-file "src/pindel2vcf4tcga" bin)
              (install-file "src/sam2pindel" bin))))
        ;; There are multiple test targets, so in order to run all
        ;; tests, we must run the separate make targets.
        (replace 'check
          (lambda* (#:key inputs #:allow-other-keys)
            (and
             (zero? (system* "make" "acceptance-tests"))
             (zero? (system* "make" "coverage-tests"))
             (zero? (system* "make" "cppcheck"))
             (zero? (system* "make" "functional-tests"))
             (zero? (system* "make" "regression-tests"))))))))
   (home-page "https://github.com/genome/pindel")
   (synopsis "Structural variants detector for next-gen sequencing data")
   (description "Pindel can detect breakpoints of large deletions, medium sized
insertions, inversions, tandem duplications and other structural variants at
single-based resolution from next-gen sequence data.  It uses a pattern growth
approach to identify the breakpoints of these variants from paired-end short
reads.")
   (license license:gpl3+)))

(define-public varscan
  (package
    (name "varscan")
    (version "2.4.1")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/dkoboldt/varscan/releases/download/v"
            version "/VarScan.v" version ".source.jar"))
      (sha256
       (base32 "0y45ympkza7qwcbcisg006286pwjbr5978n03hx5nvl09f0mapk8"))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda _
             (mkdir "source")
             (chdir "source")
             (and
              ;; Unpack the Java archive containing the source files.
              (zero? (system* "jar" "xf" (assoc-ref %build-inputs "source")))
              ;; Remove existing compiled output.
              (with-directory-excursion "net/sf/varscan/"
                (for-each (lambda (file)
                            (unless (string= (string-take-right file 5) ".java")
                              (zero? (system* "rm" file))))
                          (find-files "." #:directories? #f))))))
         (replace 'build
           (lambda _
             (let ((classes '()))
             (and
              ;; Compile the source files.
              (with-directory-excursion "net/sf/varscan/"
                (for-each (lambda (file)
                            (when (string= (string-take-right file 5) ".java")
                              (zero? (system* "javac" file))
                              (cons ))
                          (find-files "." #:directories? #f)))
              ;; Construct the new Java archive.
              (zero? (system* "jar" "cfm" "varscan-2.4.1.jar"
                              "META-INF/MANIFEST.MF"
                              "net/sf/varscan/*.java")))))))
        (replace 'install
          (lambda _
            (let ((out (string-append (assoc-ref %outputs "out")
                                      "/share/java/varscan/")))
              (mkdir-p out)
              (install-file "varscan-2.4.1.jar" out)))))))
    (home-page "http://dkoboldt.github.io/varscan/")
    (synopsis "Variant detection in massively parallel sequencing data")
    (description "")
    ;; Free for non-commercial use by academic, government, and
    ;; non-profit/not-for-profit institutions
    (license license:non-copyleft)))
