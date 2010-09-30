# OM 'ocs' specifications 
# special variant for booting C sources of distribution
# $Id$

BOOTLIB		:= $(OCS)/src/lib/boot

OC1             := 
OC2             := 
GENMAKE		:=
OPALIMPORTS	:=
OCSYSPARAM  	:= 
OC1FLAGS    	:=
OC2FLAGS    	:=
CCFLAGS     	:= -I. -I$(BOOTLIB) $(CINCPATH)
OSTART      	:= $(BOOTLIB)/_ostart.c
HDEPS		:= $(BOOTLIB)/BUILTIN.h $(CHDEPS)
LDLIBS      	:= -lboot $(CLDLIBS) $(OSLDLIBS)
LDLIBPATH   	:= -L$(BOOTLIB) $(CLDLIBPATH) $(OSLDLIBPATH)
LDLIBDEPS   	:= $(BOOTLIB)/libboot.a $(CLDLIBDEPS)
GENSTDSYS       := 
