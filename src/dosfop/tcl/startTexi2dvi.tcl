#!/usr/local/bin/wish -f
# $Id$

wm withdraw .

source $env(DOSFOP)/tcl/global.tcl
source $env(DOSFOP)/tcl/filehelp.tcl
source $env(DOSFOP)/tcl/pureExec.tcl

################
# run texi2dvi #
################

del temp

# start updating buffer
#

send dosfop_tcl {updateBuffer}

# has to be caught, because window must not exists!
catch {send dosfop_tcl {disable .outputFrame.makeinfo}}

send dosfop_tcl {.outputFrame.stateText configure -text "Running Texi2dvi Translation..."}

pureTexi2dvi 1

send dosfop_tcl {.outputFrame.stateText configure -text "Texinfo Translation Finished !"}

# has to be caught, because window must not exists!
catch {send dosfop_tcl {enable .outputFrame.makeinfo}}

send dosfop_tcl {set terminated 1}
del temp

beep

