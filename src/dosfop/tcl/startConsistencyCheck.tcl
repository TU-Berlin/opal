#!/usr/local/bin/wish -f
# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl/startConsistencyCheck.tcl,v 1.1.1.1 1998-06-16 16:00:31 wg Exp $

wm withdraw .

source $env(DOSFOP)/tcl/global.tcl
source $env(DOSFOP)/tcl/filehelp.tcl
source $env(DOSFOP)/tcl/pureExec.tcl

del temp
del ErrorOutput

#########################
# construct config file #
#########################

send dosfop_tcl {.outputFrame.stateText configure -text "Constructing Config File..."}
send dosfop_tcl {.outputFrame.cancel configure -command doCancelConsistencyCheck}
send dosfop_tcl {constructConfigFile [pwd]} 

#####################
# check consistency #
#####################


# start updating buffer
#
send dosfop_tcl {updateBuffer}

send dosfop_tcl {.outputFrame.stateText configure -text "Checking Consistency..."}

pureCheckConsistency 1

#############
# terminate #
#############

send dosfop_tcl {set terminated 1}

if {[file size ErrorOutput] == 0} then {
  send dosfop_tcl {.outputFrame.stateText configure \
               -text "Result: Configuration is Consistent !"}
  beep
  exec mv ErrorOutput temp
} else {
  
  # the temp file is the standard file for output so:
  #
  exec mv ErrorOutput temp

  send dosfop_tcl {.outputFrame.stateText configure \
                -text "Result: There are Inconsistencies !"}
  beep

  send dosfop_tcl {set terminated 0}
  send dosfop_tcl {updateBuffer}
  send dosfop_tcl {set terminated 1}

}

# leave the application and return to caller context
#

exit

