# Makefile for booting library of OC 
# $Id$

include $(OCSPROJECT)

include $(OCSOM)/specs/Specs.os.$(os)
include $(OCSOM)/specs/Specs.opt.$(opt)
include $(OCSOM)/specs/Specs.debug.$(debug)
include $(OCSOM)/specs/Specs.ocs.boot

OS := $(patsubst %.c,%.o,$(wildcard *.c))

SYSTEM=shared
SYS_CCFLAGS=$(VAR_CCFLAGS)

boot: lib$(SYSTEM).a

lib$(SYSTEM).a: $(OS)
	@echo "Archiving $(SYSTEM)" ; \
	$(RM) -f lib$(SYSTEM).a ; \
	$(ARQ) lib$(SYSTEM).a $(OS) && \
	$(RANLIB) lib$(SYSTEM).a

%.o: %.c $(HDEPS)
	@echo "Generating object code for $*"; \
	$(CC) $(CCFLAGS) $(SYS_CCFLAGS) -o $*.o -c $*.c

clean:
	$(RM) -f $(OS) lib$(SYSTEM).a

install:

