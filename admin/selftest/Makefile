## Makefile to perform a selftest

PWD = `pwd`
RM = /bin/rm
TIME = /usr/bin/time

.PHONY: default clean cleanall

default:
	TOCS=$(PWD) $(TIME) ./selftest > log

clean:
	$(RM) -r -f bin man 

cleanall: clean
	$(RM) -r -f src