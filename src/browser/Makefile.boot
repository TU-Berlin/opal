# Makefile for booting browser 
# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/browser/Makefile.boot,v 1.1.1.1 1998-06-16 15:59:54 wg Exp $

include $(OCSPROJECT)

include $(OCSOM)/specs/Specs.os.$(os)
include $(OCSOM)/specs/Specs.opt.$(opt)
include $(OCSOM)/specs/Specs.debug.$(debug)
include $(OCSOM)/specs/Specs.ocs.boot


OS := $(patsubst %.c,%.o,$(wildcard *.c))

TOPCOM=browser
TOPSTRUCT=Browser

SYS_CCFLAGS=$(VAR_CCFLAGS)
SYS_LDFLAGS=$(VAR_LDFLAGS)
SYS_LDLIBS=-lshared
SYS_LDLIBPATH=-L../../oc/boot/shared
SYS_LDLIBDEPS=../../oc/boot/shared/libshared.a

boot: $(TOPCOM)

$(TOPCOM): $(OS) _$(TOPSTRUCT)_$(TOPCOM).o $(SYS_LDLIBDEPS) $(LDLIBDEPS)
	@echo "Linking $@"; \
	$(LD) $(LDFLAGS) $(SYS_LDFLAGS) -o $(TOPCOM) \
	    _$(TOPSTRUCT)_$(TOPCOM).o $(OS) \
	    $(SYS_LDLIBS)  $(LDLIBS) \
	    $(SYS_LDLIBPATH) $(LDLIBPATH)

_$(TOPSTRUCT)_$(TOPCOM).o: $(TOPSTRUCT).h $(OSTART) $(HDEPS)
	@echo "Generating startup code for $(TOPCOM) ..."; \
	$(CC) $(CCFLAGS) $(SYS_CCFLAGS) \
		-Dcommand=__A$(TOPSTRUCT)_A$(TOPCOM) -Dinit=init_A$(TOPSTRUCT) \
		-Dinclude='"$(TOPSTRUCT).h"' \
		-o _$(TOPSTRUCT)_$(TOPCOM).o -c $(OSTART)

%.o: %.c $(HDEPS)
	@echo "Generating object code for $*"; \
	$(CC) $(CCFLAGS) $(SYS_CCFLAGS) -o $*.o -c $*.c

clean:
	$(RM) -f $(OS) _$(TOPSTRUCT)_$(TOPCOM).o $(TOPCOM)

install: $(TOPCOM)
	$(OCSOM)/etc/xinstall $(INSTALLBINFLAGS) $(TOPCOM) $(INSTALLBINPATH)
