#!/usr/local/bin/wish -f
# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl/startMakeinfo.tcl,v 1.1.1.1 1998-06-16 16:00:31 wg Exp $

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

