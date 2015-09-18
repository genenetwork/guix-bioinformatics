# guix-bioinformatics

Bioinformatics packages for GNU Guix that are used in http://genenetwork.org/.
See [Guix Notes](https://github.com/pjotrp/guix-notes/blob/master/HACKING.org) for hacking GNU Guix.

Set the GUIX_PACKAGE_PATH to point to the root of this directory
before running Guix. E.g.

    env GUIX_PACKAGE_PATH=$genenetwork/guix-bioinformatics/ guix package -A cwl

or from the checked out Guix repo

    env GUIX_PACKAGE_PATH=$genenetwork/guix-bioinformatics/ ./pre-inst-env guix package -A cwl

Some of these package definitions should make it upstream into the GNU
Guix repository when tested and stable.

## Install Common Workflow Language (CWL)

Install the common workflow language tool cwltool with

    export GUIX_PACKAGE_PATH=$genenetwork/guix-bioinformatics/
    guix package -i python2-cwltool
    cwtool --version
      1.0.20150916041152

## Install R/qtl

    export GUIX_PACKAGE_PATH=$genenetwork/guix-bioinformatics/
    guix package -i r-qtl r
    R
      library(qtl)
