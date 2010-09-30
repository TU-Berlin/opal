# Makefile for booting opalimports 
# $Id$

include $(OCSPROJECT)

include $(OCSOM)/specs/Specs.os.$(os)
include $(OCSOM)/specs/Specs.opt.$(opt)
include $(OCSOM)/specs/Specs.debug.$(debug)
include $(OCSOM)/specs/Specs.ocs.boot

SYS_CCFLAGS=$(VAR_CCFLAGS)

boot: opalimports

opalimports: lex.yy.c
	$(CC) $(CCFLAGS) $(SYS_CCFLAGS) -o opalimports lex.yy.c $(LEXLIBS)

lex.yy.c: opalimports.l
	$(LEX) opalimports.l

clean:
	$(RM) -f lex.yy.c opalimports

install: opalimports
	$(OCSOM)/etc/xinstall $(INSTALLBINFLAGS) opalimports $(INSTALLBINPATH)
