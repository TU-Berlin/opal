#!/usr/local/bin/wish -f
# $Id$

wm withdraw .

source $env(DOSFOP)/tcl/global.tcl
source $env(DOSFOP)/tcl/filehelp.tcl
source $env(DOSFOP)/tcl/pureExec.tcl

################
# run makeinfo #
################

del temp

# start updating buffer
#
send dosfop_tcl {updateBuffer}

# has to be catched, because window need not exist!
catch {send dosfop_tcl {disable .outputFrame.makeinfo}}

send dosfop_tcl {.outputFrame.stateText configure -text "Running Makeinfo Translation..."}

pureMakeinfo 1

# has to be catched, because window must not exists!
catch {send dosfop_tcl {enable .outputFrame.makeinfo}}

send dosfop_tcl {set terminated 1}
del temp

send dosfop_tcl {.outputFrame.stateText configure -text "Makeinfo Translation Finished !"}
beep

