# guix-bioinformatics

Bioinformatics packages for GNU Guix that are used in
http://genenetwork.org/.  See
[Guix Notes](https://github.com/pjotrp/guix-notes/blob/master/HACKING.org)
for installing and hacking GNU Guix.

Simply set the GUIX_PACKAGE_PATH to point to the root of this directory
before running Guix. E.g.

    git clone https://github.com/genenetwork/guix-bioinformatics.git
    export GUIX_PACKAGE_PATH=$PWD/guix-bioinformatics/
    guix package -A cwl

or using a checked out Guix repo with

    env GUIX_PACKAGE_PATH=$genenetwork/guix-bioinformatics/ ./pre-inst-env guix package -A cwl

Some (or most) of these package definitions should make it upstream
into the GNU Guix repository when tested and stable.

## LLVM D compiler

Install with

    git clone https://github.com/genenetwork/guix-bioinformatics.git
    export GUIX_PACKAGE_PATH=$PWD/guix-bioinformatics/
    guix package -i ldc
    
    ldc2 --version

    LDC - the LLVM D compiler (0.16.0):
    based on DMD v2.067.1 and LLVM 3.6.2
    Default target: x86_64-unknown-linux-gnu
    Host CPU: corei7-avx
    http://dlang.org - http://wiki.dlang.org/LDC
    
    Registered Targets:
    aarch64    - AArch64 (little endian)
    aarch64_be - AArch64 (big endian)
    amdgcn     - AMD GCN GPUs
    arm        - ARM
    arm64      - ARM64 (little endian)
    armeb      - ARM (big endian)
    cpp        - C++ backend
    hexagon    - Hexagon
    mips       - Mips
    mips64     - Mips64 [experimental]
    mips64el   - Mips64el [experimental]
    mipsel     - Mipsel
    msp430     - MSP430 [experimental]
    nvptx      - NVIDIA PTX 32-bit
    nvptx64    - NVIDIA PTX 64-bit
    ppc32      - PowerPC 32
    ppc64      - PowerPC 64
    ppc64le    - PowerPC 64 LE
    r600       - AMD GPUs HD2XXX-HD6XXX
    sparc      - Sparc
    sparcv9    - Sparc V9
    systemz    - SystemZ
    thumb      - Thumb
    thumbeb    - Thumb (big endian)
    x86        - 32-bit X86: Pentium-Pro and above
    x86-64     - 64-bit X86: EM64T and AMD64
    xcore      - XCore


## Common Workflow Language (CWL)

Install the common workflow language tool cwltool with

    git clone https://github.com/genenetwork/guix-bioinformatics.git
    export GUIX_PACKAGE_PATH=$PWD/guix-bioinformatics/
    guix package -i python2-cwltool
    
    cwtool --version
      1.0.20150916041152

## R/qtl

R-qtl is now part of main stream GNU Guix:

    guix package -i r-qtl r

# Development tips

## Override individual packages

The cheerful way of overriding a version of a package:

    (use-modules (guix) (gnu packages emacs))

    (package
      (inherit emacs)
      (name "emacs-snapshot")
      (source "/path/to/some-file-or-directory.tar.gz"))

and then run:

    guix package --install-from-file=that-file.scm


## LICENSE

These package descriptions (so-called Guix expressions) are
distributed by the same license as GNU Guix, i.e. GPL3+
