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

## Slurm and munge

Install slurm with

    git clone https://github.com/genenetwork/guix-bioinformatics.git
    export GUIX_PACKAGE_PATH=$PWD/guix-bioinformatics/
    guix package -i slurm-llnl

    ~/.guix-profile/sbin/slurmd -C -D
      ClusterName=(null) NodeName=selinunte CPUs=4 Boards=1 SocketsPerBoard=1 CoresPerSocket=2 ThreadsPerCore=2 RealMemory=7890 TmpDisk=29909

## Module system

Install the module environment with

    git clone https://github.com/genenetwork/guix-bioinformatics.git
    export GUIX_PACKAGE_PATH=$PWD/guix-bioinformatics/
    guix package -i environment-modules

    modulecmd --version
      VERSION=3.2.10
      DATE=2012-12-21

## python2-numarray 1.5.2

Install python2-numarray package with

    git clone https://github.com/genenetwork/guix-bioinformatics.git
    export GUIX_PACKAGE_PATH=$PWD/guix-bioinformatics/
    guix package -i python2-numarray

## Common Workflow Language (CWL)

Install the common workflow language tool cwltool with

    git clone https://github.com/genenetwork/guix-bioinformatics.git
    export GUIX_PACKAGE_PATH=$PWD/guix-bioinformatics/
    guix package -i python2-cwltool
    
    cwtool --version
      1.0.20150916041152

# Packages moved from here to main line GNU Guix

## LLVM D compiler

The LLVM D compiler was added to GNU Guix main stream!

    guix package -i ldc
    
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
