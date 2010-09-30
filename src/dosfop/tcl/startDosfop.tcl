#!/usr/local/bin/wish -f
# $Id$

wm withdraw .

source $env(DOSFOP)/tcl/global.tcl
source $env(DOSFOP)/tcl/filehelp.tcl
source $env(DOSFOP)/tcl/pureExec.tcl

#exec $env(DOSFOP)/bin/startConsistencyCheck

# if errors occurred on consistency check command then exit dosfop translation:
#
#if {[file size temp] != 0} then {exit}

#########################
# construct config file #
#########################

send dosfop_tcl {.outputFrame.stateText configure -text "Constructing Config File..."}
send dosfop_tcl {.outputFrame.cancel configure -command doCancelConsistencyCheck}
send dosfop_tcl {constructConfigFile [pwd]} 

##################
# execute DOSFOP #
##################

del temp

send dosfop_tcl {set terminated 0}
send dosfop_tcl {updateBuffer}

send dosfop_tcl {.outputFrame.stateText configure -text  "running DOSFOP translation..."}
#send dosfop {.outputFrame.cancel configure -command "destroy .outputFrame"}
send dosfop_tcl {.outputFrame.cancel configure -command doCancelDOSFOPExecution}

pureDosfop 1

#############
# terminate #
#############

send dosfop_tcl {.outputFrame.stateText configure -text "DOSFOP translation finished!"}
beep

send dosfop_tcl {set terminated 1}

del temp

# send dosfop {.outputFrame.texi2dvi configure -text "make printable documentation" \
# 	-command doStartTexi2dvi}
# send dosfop {pack .outputFrame.texi2dvi -in .outputFrame.buttonFrame \
# 	-side left -padx 2m -pady 2m}

# send dosfop {.outputFrame.makeinfo configure -text "make hypertext" \
# 	-command doStartMakeinfo}
# send dosfop {pack .outputFrame.makeinfo -in .outputFrame.buttonFrame \
# 	-side left -padx 2m -pady 2m}

# send dosfop {.outputFrame.texi2html configure -text "make HTML document" \
# 	-command doStartTexi2html}
# send dosfop {pack .outputFrame.texi2html -in .outputFrame.buttonFrame \
# 	-side left -padx 2m -pady 2m}


#
# process keeps running and has to be killed for termination !
# 

##
