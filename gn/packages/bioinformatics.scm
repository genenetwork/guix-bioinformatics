;; Bioinformatics module

(define-module (gn packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages statistics))

(define-public contra
  (package
    (name "contra")
    (version "2.0.6")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "mirror://sourceforge/contra-cnv/CONTRA.V"
            (version-major+minor version) "/CONTRA.v" version ".tar.gz"))
      (sha256
       (base32
        "0agpcm2xh5f0i9n9sx1kvln6mzdksddmh11bvzj6bh76yw5pnw91"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (delete-file "BEDTools.v2.11.2.tar.gz") #t))))
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
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/contra")))
               (copy-recursively "scripts" (string-append bin "/scripts"))
               (install-file "contra.py" bin)
               (install-file "baseline.py" bin)
               ;; There's only a pre-built PDF available.
               (install-file "CONTRA_User_Guide.2.0.pdf" doc))
             #t)))))
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

(define-public plink-ng-gn
  (let ((commit "5d1db4313ba0cc976562da233db4aced78975d10"))
  (package
    (name "plink-ng-gn")
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

(define-public pindel
  (package
   (name "pindel")
   (version "0.2.5b8")
   (source (origin
     (method git-fetch)
     (uri (git-reference
            (url "https://github.com/genome/pindel.git")
            (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "16a32fbgv1n58nfcxa1nyphrdrad80sgpinfa9p028n6plwycpww"))))
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
            (invoke "make")))
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
          (lambda _
            (for-each (lambda (target)
                        (invoke "make" target))
                      '("acceptance-tests" "coverage-tests" "cppcheck"
                        "functional-tests" "regression-tests")))))))
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
     `(#:tests? #f ; build.xml does not exist
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda _
             (mkdir "source")
             (chdir "source")
             ;; Unpack the Java archive containing the source files.
             (invoke "jar" "xf" (assoc-ref %build-inputs "source"))
             ;; Remove existing compiled output.
             (with-directory-excursion "net/sf/varscan/"
               (for-each (lambda (file)
                           (delete-file file))
                         (find-files "." "^.java$" #:directories? #f)))
             #t))
         (replace 'build
           (lambda _
             ;; Compile the source files.
             (with-directory-excursion "net/sf/varscan/"
               (for-each (lambda (file)
                           (invoke "javac" file))
                         (find-files "." ".java$" #:directories? #f)))
             ;; Construct the new Java archive.
             (apply invoke "jar" "cfm"
                    (string-append "varscan-" ,version ".jar")
                    "META-INF/MANIFEST.MF"
                    (find-files "net/sf/varscan" ".java$"))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (string-append (assoc-ref outputs "out")
                                      "/share/java/varscan/")))
              (install-file (string-append "varscan-" ,version ".jar") out))
            #t)))))
    (home-page "https://dkoboldt.github.io/varscan/")
    (synopsis "Variant detection in massively parallel sequencing data")
    (description "Variant detection in massively parallel sequencing data.")
    ;; Free for non-commercial use by academic, government, and
    ;; non-profit/not-for-profit institutions
    (license license:non-copyleft)))

(define-public edirect-gn
  (package
    (inherit edirect)
    (name "edirect-gn")
    (arguments
      (substitute-keyword-arguments (package-arguments edirect)
        ((#:phases phases)
         `(modify-phases ,phases
         ;   (replace 'build
         ;     (lambda* (#:key inputs #:allow-other-keys)
         ;       (let ((go (string-append (assoc-ref inputs "go") "/bin/go")))
         ;         (invoke go "build" "xtract.go"))))
            (add-after 'unpack 'patch-programs
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((gzip (assoc-ref inputs "gzip")))
                  (substitute* '("index-bioc"
                                 "pm-index"
                                 "pm-invert"
                                 "pm-stash"
                                 "rchive.go"
                                 "run-ncbi-converter")
                    (("gunzip") (string-append gzip "/bin/gunzip")))
                  (substitute* (find-files "." "^e")
                    (("exec perl") "exec"))
                  (substitute* '("xtract" "rchive")
                    ;; or add current directory to PATH
                    ((".*PATH.*") "")))
                #t))
            (replace 'install
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                      (xtract.linux (assoc-ref inputs "xtract.Linux"))
                      (rchive.linux (assoc-ref inputs "rchive.Linux")))
                  (for-each
                    (lambda (file)
                      (install-file file bin))
                    '("archive-pubmed" "asp-cp" "asp-ls" "download-pubmed"
                      "edirect.pl" "efetch" "epost" "fetch-pubmed" "ftp-cp"
                      "ftp-ls" "has-asp" "pm-prepare" "pm-refresh" "pm-stash"
                      "rchive" "xtract"))
                  (copy-file xtract.linux (string-append bin "/xtract.Linux"))
                  (copy-file rchive.linux (string-append bin "/rchive.Linux"))
                  (chmod (string-append bin "/xtract.Linux") #o555)
                  (chmod (string-append bin "/rchive.Linux") #o555))
                #t))
            (replace 'wrap-program
              (lambda* (#:key outputs #:allow-other-keys)
                ;; Make sure 'edirect.pl' finds all perl inputs at runtime.
                (let ((out (assoc-ref outputs "out"))
                      (path (getenv "PERL5LIB")))
                  (for-each
                    (lambda (file)
                      (wrap-program (string-append out "/bin/" file)
                                    `("PERL5LIB" ":" prefix (,path))))
                    '("edirect.pl" "asp-ls" "ftp-cp" "ftp-ls")))
                #t))))))
    (inputs
     `(("gzip" ,gzip)
       ,@(package-inputs edirect)))
    (native-inputs
     `(
       ;("go" ,go)
       ("xtract.Linux"
        ,(origin
           (method url-fetch)
           (uri (string-append "ftp://ftp.ncbi.nlm.nih.gov/entrez/entrezdirect/"
                               "versions/" (package-version edirect) "/xtract.Linux"))
           (file-name (string-append "xtract.Linux-" (package-version edirect)))
           (sha256
            (base32
             "0fx6arpn38spnwszmvkkpa3498qrrlglg2l9jw91icgqbyjjq9wq"))))
       ("rchive.Linux"
        ,(origin
           (method url-fetch)
           (uri (string-append "ftp://ftp.ncbi.nlm.nih.gov/entrez/entrezdirect/"
                               "versions/" (package-version edirect) "/rchive.Linux"))
           (file-name (string-append "rchive.Linux-" (package-version edirect)))
           (sha256
            (base32
             "134y0zprplqlplc6qmcjb97411bxkwghmq3z0qjgh0dgdbzjq1w3"))))))
    (native-search-paths
     ;; Ideally this should be set for LWP somewhere.
     (list (search-path-specification
            (variable "PERL_LWP_SSL_CA_FILE")
            (file-type 'regular)
            (separator #f)
            (files '("/etc/ssl/certs/ca-certificates.crt")))))
    ;; Due to the precompiled binaries we download:
    (supported-systems '("x86_64-linux"))))
