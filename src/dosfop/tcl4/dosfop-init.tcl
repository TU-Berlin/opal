### initialising the DOSFOP environment

# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl4/dosfop-init.tcl,v 1.1.1.1 1998-06-16 16:00:44 wg Exp $

uplevel #0 append dosfopVersions {dosfop-init\ 1.01\n}

# former name environmentPrepared
# test whether environment is initialised
# generates an error, if not with errorInfo set approprioately
proc dosfop-init-is-initialised {} {
  global env dosfopRoot

  set errorMessage ""
  set checkStatus 1

  if {[file isdirectory $dosfopRoot/doc] == 0} then {
    set checkStatus 0
    append errorMessage "doc subdirectory does not exist\n"
  }       

  if {[file isdirectory $dosfopRoot/DOSFOP] == 0} then {
    set checkStatus 0
    append errorMessage "DOSFOP subdirectory does not exist\n"
  }       

  if {[file isfile $dosfopRoot/DOSFOP/Toplevel.config] == 0} then {
    set checkStatus 0
    append errorMessage "DOSFOP Toplevel configuration file does not exist\n"
  }       

  if {[file isfile $dosfopRoot/DOSFOP/Toplevel.structures.config] == 0} then {
    set checkStatus 0
    append errorMessage "DOSFOP Toplevel structures configuration file does not exist\n"
  }       

#  if {[file isfile $dosfopRoot/DOSFOP/Library.config] == 0} then {
#    set checkStatus 0
#    append errorMessage "DOSFOP Library configuration file does not exist\n"
#  }       

    if { $checkStatus == 1} then {
	return $checkStatus
    } else { 
	return -code error -errorinfo $errorMessage $checkStatus
    }
}

#### Initialise DOSFOP

proc dosfop-init { } {
    global dosfopRoot env

    # set up a list of pairs { regexp-from regexp-to } which describes the 
    # modifications necessary for the Toplevel.config file
    set modifiers [list [list DIRDEFAULT [pwd] ] [list OUTPUTDEFAULT $dosfopRoot/DOSFOP/[file tail [pwd]].texi] [list Main * ] [list \{\} \{[file tail [pwd]]\}] ]
    # a command to set up the Library.config file
#    set LibConfigCmd [list dosfop-cp $env(DOSFOP)/defaults/Library.config $dosfopRoot/DOSFOP]
    # as above for the options.data file
    set OptionsCmd [list dosfop-cp $env(DOSFOP)/defaults/options.data   $dosfopRoot/DOSFOP]
    # as above for the Toplevel.config file
    set ToplevelCmd [list dosfop-cp-modify \
		  $env(DOSFOP)/defaults/Toplevel.config.default \
		  $dosfopRoot/DOSFOP/Toplevel.config \
		  $modifiers ]
    # as above for the Toplevel.structures.config file
    set TopConfigCmd [list dosfop-cp $env(DOSFOP)/defaults/Toplevel.structures.config.default $dosfopRoot/DOSFOP/Toplevel.structures.config]

    dosfop-init-dirs [list $dosfopRoot/doc $dosfopRoot/DOSFOP ]
    dosfop-init-files [list \
			   [list options.data $OptionsCmd ] \
			   [list Toplevel.config $ToplevelCmd] \
			   [list Toplevel.structures.config $TopConfigCmd] \
			  ]
#			   [list Library.config $LibConfigCmd ] \
}


# check all directories in argument list
proc dosfop-init-dirs { l } {

    foreach d $l {
	if { ! [file isdirectory $d] } then {
	    puts "creating directory $d"
	    exec mkdir $d
	}
    }
}

# check all necessary files in list
# each list is a two element list: 
# first element is name of file in DOSFOP directory to be checked, 
# second is command to be evaluated for its creation
proc dosfop-init-files { l } {
    global env

    foreach fp $l {
	set ff [lindex $fp 0]
	if { ! [file exists "DOSFOP/$ff"]} then {
	    puts "creating file $ff"
	    eval [lindex $fp 1]
	}
    }
}
