# Makefile for booting OPAL library
# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/lib/Makefile.boot,v 1.1.1.1 1998-06-16 15:59:55 wg Exp $

include $(OCSPROJECT)

include $(OCSOM)/specs/Specs.os.$(os)
include $(OCSOM)/specs/Specs.opt.$(opt)
include $(OCSOM)/specs/Specs.debug.$(debug)
include $(OCSOM)/specs/Specs.ocs.boot

OS := $(patsubst %.c,%.o,\
		$(filter-out %.hc.c genmttab.c _ostart.c, $(wildcard *.c)))
HS := $(wildcard *.h)

SYSTEM=boot
SYS_CCFLAGS=$(VAR_CCFLAGS)

boot: lib$(SYSTEM).a

lib$(SYSTEM).a: $(OS) $(HS) genmttab.c
	@echo "Generating method tables"; \
	$(CC) $(CCFLAGS) $(SYS_CCFLAGS) -o genmttab genmttab.c; \
	./genmttab ; \
	$(CC) $(CCFLAGS) $(SYS_CCFLAGS) -c _mttab_*.c ; \
	echo "Archiving boot library"; \
	$(RM) -f $@ ; \
	$(ARQ) $@ _mttab_*.o $(OS); \
	$(RM) -f _mttab_* genmttab ; \
	$(RANLIB) $@

%.o: %.c $(HS)
	@echo "Generating object code for $*"; \
	$(CC) -I. $(CCFLAGS) $(SYS_CCFLAGS) -o $*.o -c $*.c

clean:
	$(RM) -f $(OS) _mttab_* genmttab lib$(SYSTEM).a

install: _ostart.c
	$(OCSOM)/etc/xinstall $(INSTALLOBJECTFLAGS) \
	   _ostart.c $(INSTALLOBJECTPATH)/opal-opt || exit 1; \







