## Makefile to install OCS from the sources

.PHONY: install default usage

default: usage

## define these variables to match your system
#.. Path to the source of the distribution
OCSSRC=/home/shome/kd/ocs/src
#.. Absolute path to the ProjectDefs file in this directory
OCSPROJECT=/home/shome/kd/ocs/ProjectDefs
#.. Path to the place where the distribution is to be installed
OCSHDIR=ocs-$(VERSION)
OCSHOME=/usr/local/$(OCSHDIR)
#.. Description of your system (see also in $OCSSRC/om/specs/Specs.basic)
#.. use something like `uname -s`-`uname -m`
OSARCH=linux2-i586
#.. Path to the GNU sed program
SED=/usr/bin/sed
#.. Path to the GNU tar program
TAR=/bin/tar
#.. Path to the GNU zip program
GZIP=/usr/bin/gzip -f 
#.. Non-vital components of the Opal system
STDPACKAGES =  lib.opal_parserlight lib.opal_readline \
		lib.opal_tcl lib.opal_tk lib.opal_win \
		 pkg.dynamite lib.oc_reflections \
		pkg.browser pkg.emacs pkg.dosfop pkg.doc pkg.ordinatrice \
		pkg.oasys pkg.tivi2 

## don't change anything beyond this line ##############################

#.. Version of the Opal distribution
VERSION=2.3d.gamma

#.. name of the distribution archives
SRCDISTR=ocs-$(VERSION)-src
SHAREDDISTR=ocs-$(VERSION)-shared
BINDISTR=ocs-$(VERSION)-bin-$(OSARCH)

#.. Packages which are to be installed (in the given order!)
INITPACKAGES = pkg.om
MINPACKAGES = pkg.opalimports lib.opal_base pkg.genmake \
	      lib.oc_shared lib.oc_oc1 lib.oc_oc2 pkg.oc1 pkg.oc2 
MINSRCPACKAGES	=	pkg.examples
PACKAGES = $(MINPACKAGES) $(STDPACKAGES)

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

pkg.examples:
	$(OCSADMIN) install $@

boot: $(MINPACKAGES)
	$(OCSADMIN) install $(MINSRCPACKAGES)

complete: $(STDPACKAGES)

# distclean:
# 	$(OCSADMININIT) ocs -command cleanobjall $(MINPACKAGES)
#	$(OCSADMININIT) ocs -command cleanall $(MINSRCPACKAGES) $(STDPACKAGES)

distclean:
	(cd $(OCSSRC)/om; ocs cleanobjall)
	(cd $(OCSSRC)/oc; ocs cleanobjall)
	(cd $(OCSSRC)/lib; ocs cleanobjall)
	(cd $(OCSSRC)/oc/dynamite; ocs cleanall)
	(cd $(OCSSRC)/oc/reflections; ocs cleanall)
	(cd $(OCSSRC)/lib/Tools/OpalWin; ocs cleanall)
	(cd $(OCSSRC)/lib/Tools/OpalWinAdditions; ocs cleanall)
	(cd $(OCSSRC)/lib/Tools/ParserLight; ocs cleanall)
	(cd $(OCSSRC)/lib/Tools/Readline; ocs cleanall)
	(cd $(OCSSRC)/lib/Tools/Tcl; ocs cleanall)
	(cd $(OCSSRC)/lib/Tools/Tk; ocs cleanall)
	(cd $(OCSSRC)/browser; ocs cleanall)
	(cd $(OCSSRC)/dosfop; ocs cleanall)
	(cd $(OCSSRC)/emacs; ocs cleanall)
	(cd $(OCSSRC)/oasys; ocs cleanall)
	(cd $(OCSSRC)/ordinatrice; ocs cleanall)
	(cd $(OCSSRC)/tivi2; ocs cleanall)

preparedist:
	(cd $(OCSSRC)/lib; ocs cleanall; export OCSSRC=$(OCSSRC); ocs -P $(OCSSRC)/ProjectDefs.bootstrap)
	(cd $(OCSSRC)/om; ocs cleanall; export OCSSRC=$(OCSSRC); ocs -P $(OCSSRC)/ProjectDefs.bootstrap)
	(cd $(OCSSRC)/oc; ocs cleanall; export OCSSRC=$(OCSSRC); ocs -P $(OCSSRC)/ProjectDefs.bootstrap)

sourcedistr:
	cp -f ignore1  /tmp; cp -f ignore2  /tmp; \
	cd $(OCSSRC)/../.. ; \
	$(TAR) -X /tmp/ignore1 -X /tmp/ignore2 -cvf $(SRCDISTR).tar ocs/src; \
	$(TAR) -X /tmp/ignore1 \
		-X /tmp/ignore2 -rvf $(SRCDISTR).tar ocs/examples; \
	$(TAR) -X /tmp/ignore1 -rvf $(SRCDISTR).tar ocs/doc ocs/Makefile ocs/ignore1 ocs/ignore2 ; \
	$(GZIP) $(SRCDISTR).tar; \
	rm /tmp/ignore1; rm /tmp/ignore2

bindistr:
	$(TAR) -cvf $(OCSSRC)/../../$(BINDISTR).tar -C $(OCSHOME)/.. $(OCSHDIR)/bin $(OCSHDIR)/dosfop $(OCSHDIR)/packages $(OCSHDIR)/lib ; \
	$(GZIP) $(OCSSRC)/../../$(BINDISTR).tar

shareddistr:
	$(TAR) -cvf $(OCSSRC)/../../$(SHAREDDISTR).tar -C $(OCSHOME)/.. $(OCSHDIR)/VERSION $(OCSHDIR)/doc $(OCSHDIR)/etc $(OCSHDIR)/examples $(OCSHDIR)/man ; \
	$(GZIP) $(OCSSRC)/../../$(SHAREDDISTR).tar

usage:
	@echo "usage: first edit the variables at the beginning of Makefile"
	@echo "then call 'make install'"