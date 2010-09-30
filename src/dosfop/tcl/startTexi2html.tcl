#!/usr/local/bin/wish -f
# $Id$

wm withdraw .

source $env(DOSFOP)/tcl/global.tcl
source $env(DOSFOP)/tcl/filehelp.tcl
source $env(DOSFOP)/tcl/pureExec.tcl

################
# run texi2html #
################

del temp

# start updating buffer
#
send dosfop_tcl {updateBuffer}

# has to be catched, because window need not exist!
catch {send dosfop_tcl {disable .outputFrame.texi2html}}

send dosfop_tcl {.outputFrame.stateText configure -text "running texi2html translation ..."}

pureTexi2html 1

send dosfop_tcl {.outputFrame.stateText configure -text "html translation finished !"}

# has to be catched, because window must not exist!
catch {send dosfop_tcl {enable .outputFrame.texi2html}}

send dosfop_tcl {set terminated 1}
del temp

beep

