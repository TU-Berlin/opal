### configuration for the ocs script -*- makefile -*-

## packages which must be installed before (no need for transitive closure)
REQUIRED = 

## name of the package (for messages)
PKGNAME = pkg.vim

## short description
PKGDESCR = syntax highlighting for vim editor

## version
PKGVERSION = 2.3o_pre

## Package base
PKGBASE = $OCSSRC/vim

## Commands to be executed before installation of the package
PKGPREHOOK = :

## any flags for the ocs command
PKGOCSFLAGS = 


