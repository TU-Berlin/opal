### configuration for the ocs script -*- makefile -*-

## packages which must be installed before (no need for transitive closure)
REQUIRED = pkg.browser

## name of the package (for messages)
PKGNAME = pkg.emacs

## short description
PKGDESCR = emacs mode for editing of Opal and Opal-related files

## version
PKGVERSION = 2.3b

## Package base
PKGBASE = $OCSSRC/emacs

## Commands to be executed before installation of the package
PKGPREHOOK = :

## any flags for the ocs command
PKGOCSFLAGS = 


