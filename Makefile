### kleine Hilfe fuer gmake
### $@ == target
### $% == target member
### $< == erstes Datei, von der target abhaengt
### $? == neuere Dateien, von denen Target abhaengt
### $^ == alle Dateien, von denen Target abhaengt
### $+ == wie $^ , aber mit Wiederholungen
### $* == Stamm


.PHONY: install default usage

default: usage

## define these variables to match your system
#.. Version of the Opal distribution
VERSION=2.3b.beta
#.. Path to the source of the distribution
OCSSRC=/home/kd/ocs/src
#.. Path to the place where the distribution is to be installed
OCSHOME=/usr/local/ocs-$(VERSION)
#.. Description of your system (see also in $OCSSRC/om/specs/Specs.basic)
#.. use something like `uname -s`-`uname -m`
OSARCH=linux2-i586
#.. Path to the GNU sed program
SED=/usr/bin/sed
#.. Packages which are to be installed (in the given order!)
INITPACKAGES = pkg.om
MINPACKAGES = pkg.opalimports lib.opal_base pkg.genmake \
	      lib.oc_shared lib.oc_oc1 lib.oc_oc2 pkg.oc1 pkg.oc2 
MINSRCPACKAGES	=	pkg.examples
STDPACKAGES =  lib.opal_parserlight lib.opal_readline \
		lib.opal_tcl lib.opal_tk lib.opal_win \
		pkg.browser pkg.emacs pkg.dosfop pkg.doc pkg.ordinatrice \
		pkg.oasys pkg.tivi2 
PACKAGES = $(MINPACKAGES) $(STDPACKAGES)

## don't change anything beyond this line

OCSADMIN=$(OCSHOME)/bin/ocsadmin -ocshome $(OCSHOME) -ocssrc $(OCSSRC) -ocsspecs $(OCSSRC)/om/specs
OCSADMININIT=$(OCSSRC)/om/scripts/ocsadmin -ocssrc $(OCSSRC) -ocshome $(OCSHOME) -ocsspecs $(OCSSRC)/om/specs/$(OSARCH)

install: init $(PACKAGES)
	$(OCSADMIN) install $(MINSRCPACKAGES)

init:
	$(OCSADMININIT) init -sed $(SED) $(VERSION)
	$(OCSADMIN) install $(INITPACKAGES)

pkg.% lib.%:
	$(OCSADMIN) ocs $@
	$(OCSADMIN) install $@

boot: $(MINPACKAGES)
	$(OCSADMIN) install $(MINSRCPACKAGES)

complete: $(STDPACKAGES)

distclean:
	$(OCSADMIN) ocs -command cleanobjall $(MINPACKAGES)
	$(OCSADMIN) ocs -command cleanall $(STDPACKAGES)

usage:
	@echo "usage: first edit the variables at the beginning of Makefile"
	@echo "then call 'make install'"