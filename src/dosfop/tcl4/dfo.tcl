#!/usr/tcl/bin/wish -f
# $Id$
### some tcl commands for integrating dosfop into the oasys-environment
### we are using dosfop- as prefix for names used by procedures or variables 
### from this file 


uplevel #0 set dfoVersion 1.32
uplevel #0 {puts "dfo version $dfoVersion"}


### initialize DOSFOP environment variable

#proc dosfop-init-env { } {
    global env
# if DOSFOP is set, all is fine
# else if OCSDIR is set, we try OCSDIR/dosfop
# else we try /usr/ocs/dosfop
# if tries do not succeed, we abort with error
if { [llength [array names env DOSFOP]] == 0 } then {
    puts -nonewline "dfo warning: DOSFOP environment variable is not set"
    if { [llength [array names env OCSDIR]] == 1 } then {
	set try "$env(OCSDIR)/dosfop"
    } else {
	set try "/usr/ocs/dosfop"
    }
    if { [file exists $try] && [file isdirectory $try] } then {
	puts ", using $try for DOSFOP"
	set env(DOSFOP) $try
    } else {
	error "could not determine place of dosfop"
    }
}

### read in all necessary tcl-files

# global variables
source $env(DOSFOP)/tcl4/dosfop-global.tcl
# basic file handling procs
source $env(DOSFOP)/tcl4/dosfop-filehelp.tcl
# handling regular expressions
source $env(DOSFOP)/tcl4/dosfop-regexp.tcl

# initialising DOSFOP environment
source $env(DOSFOP)/tcl4/dosfop-init.tcl
# handling subsystems
source $env(DOSFOP)/tcl4/dosfop-subsystem.tcl
# handling structure
source $env(DOSFOP)/tcl4/dosfop-structure.tcl
# updating DOSFOP environment
source $env(DOSFOP)/tcl4/dosfop-update.tcl
# generate global config file
source $env(DOSFOP)/tcl4/dosfop-config.tcl
# the translation proper
source $env(DOSFOP)/tcl4/dosfop-translation.tcl
# integration in oasys
source $env(DOSFOP)/tcl4/dosfop-oasys.tcl
# }
