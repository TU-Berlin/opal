# Makefile for building and installing the OCS distribution
# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/admin/release/Makefile,v 1.1.1.1 1998-06-16 16:01:07 wg Exp $

# You need GNU-make to run this Makefile.

# You should not need to edit this file.

include ProjectDefs.build


OCSOM  := $(OCS)/src/om
OCSPROG := $(OCSOM)/scripts/ocs
OCSPROJECT  := $(OCS)/ProjectDefs.build
BOOTLIB := $(OCS)/src/lib/boot



RERUNVARS = "OCS=$(OCS)" \
	    "OCSOM=$(OCSOM)" \
	    "OCSPROJECT=$(OCSPROJECT)" \
	    "BOOTLIB=$(BOOTLIB)"

all: boot properties build install


config:
	@./config_OCS 

help ?:
	@echo "gmake [ all ] | boot | properties | build | install |";\ 
	 echo "              config | clean | help"; \
	 echo "\`gmake' is \`GNU-make'"
	 
boot:
	@echo; echo "*** Going to bootstrap the compiler and some utilities"; \
	       echo "This may take some time (1 hour or more) for a fresh"; \
	       echo "distribution."; date; \
	_systems="$(BOOTSYSTEMS)"; \
	for s in $$_systems; do \
	  echo "-- Booting system \`$$s'"; \
	  (cd $$s; $(MAKE) -r -f Makefile.boot $(RERUNVARS) boot) || exit 1; \
	done

build:	
	@echo; echo "*** Going to build the libraries and some utilities"; \
	       echo "This is another test for the bootstrapped compiler!"; \
	       date; \
	_systems="$(BUILDSYSTEMS)"; \
	OCS=$(OCS); export OCS; OCSOM=$(OCSOM); export OCSOM; \
	OCSPROJECT=$(OCSPROJECT); export OCSPROJECT; \
	for s in $$_systems; do \
	  echo "-- Building system \`$$s'"; \
	  (cd $$s; $(OCSPROG) $(RERUNVARS) ocs=expocs all) || exit 1; \
	done

properties:	
	@echo; echo "*** Going to generate properties"; \
	       echo "This is a test for the bootstrapped compiler!"; \
	       date; \
	_systems="$(PROPERTYSYSTEMS)"; \
	OCS=$(OCS); export OCS; OCSOM=$(OCSOM); export OCSOM; \
	OCSPROJECT=$(OCSPROJECT); export OCSPROJECT; \
	for s in $$_systems; do \
	  echo "-- generating properties for \`$$s'"; \
	  (cd $$s; $(OCSPROG) $(RERUNVARS) ocs=expocs \
		"REL_GENOPTIONS=-prop" check) || exit 1; \
	done

install: 
	@echo; echo "*** Installing OCS at your site"; \
	_systems="$(INSTALLBOOTSYSTEMS)"; \
	for s in $$_systems; do \
	  echo "-- Installing booted system \`$$s'"; \
	  (cd $$s; $(MAKE) -r -f Makefile.boot $(RERUNVARS) install) || exit 1; \
	done; \
	_systems="$(INSTALLSYSTEMS)"; \
	OCS=$(OCS); export OCS; OCSOM=$(OCSOM); export OCSOM; \
	OCSPROJECT=$(OCSPROJECT); export OCSPROJECT; \
	for s in $$_systems; do \
	  echo "-- Installing system \`$$s'"; \
	  (cd $$s; $(OCSPROG) $(RERUNVARS) ocs=expocs install) || exit 1; \
	done; \
	$(OCSOM)/etc/xinstall $(INSTALLBINFLAGS) \
	 VERSION $(INSTALLBINPATH)/.. || exit 1; \
	echo; echo "*** Installation done"
	
clean: 
	@echo; echo "*** Cleaning up derived objects"; \
	_systems="$(BOOTSYSTEMS)"; \
	for s in $$_systems; do \
	  echo "-- Cleaning booted system \`$$s'"; \
	  (cd $$s; $(MAKE) -r -f Makefile.boot $(RERUNVARS) clean) || exit 1; \
	done; \
	_systems="$(BUILDSYSTEMS)"; \
	OCS=$(OCS); export OCS; OCSOM=$(OCSOM); export OCSOM; \
	OCSPROJECT=$(OCSPROJECT); export OCSPROJECT; \
	for s in $$_systems; do \
	  echo "-- Cleaning builded system \`$$s'"; \
	  (cd $$s; $(OCSPROG) $(RERUNVARS) ocs=expocs clean) || exit 1; \
	done

.PHONY: boot build properties install help
