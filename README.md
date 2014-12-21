# EDWIN48
This is an effort to modernize the Edwin codebase in MIT Scheme. The goal is to make it run on
modern Schemes.

## Dependencies
Edwin48 depends on a set of libraries that are referenced by this git repo using the git submodule
mechanism. To download those dependencies, after the initial checkout, please run

    git submodule init
    git submodule update

In order to load the edwin48 code, you'll need an up to date installation of
[scsh](https://github.com/scheme/scsh). See the installation instructions there for details.

## Running Edwin48
See the `load.scm` script in the top level directory or `cosmacs/load.scm`
