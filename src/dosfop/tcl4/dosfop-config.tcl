### dosfop handling of the global config file

# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl4/dosfop-config.tcl,v 1.1.1.1 1998-06-16 16:00:44 wg Exp $

uplevel #0 append dosfopVersions {dosfop-config\ 1.09\n}

## former name constructConfigFile path
## construct config file from parts

proc dosfop-config { } {
  global dosfopRoot dosfopLibraryConfig
		    
  set globalConfig [dosfop-readFile "$dosfopRoot/DOSFOP/Toplevel.config"]

  set subsystemList [dosfop-getNameList \
			 "$dosfopRoot/DOSFOP/Toplevel.subsystems.names"]

  set toplevelStructuresConfigGlobal \
      [dosfop-readFile "$dosfopRoot/DOSFOP/Toplevel.structures.config"]
  set toplevelStructuresNames \
      [dosfop-getNameList "$dosfopRoot/DOSFOP/Toplevel.structures.names"]
  set toplevelStructuresConfig \
      [dosfop-config-structures $toplevelStructuresNames]

  set subsystemConfig [dosfop-config-subsystems $subsystemList]

    if [file exists "$dosfopRoot/DOSFOP/Library.config"] {
	set libraryConfig [dosfop-readFile "$dosfopRoot/DOSFOP/Library.config"]
    } else {
	set libraryConfig [dosfop-readFile $dosfopLibraryConfig]
    }

  set fd [open "$dosfopRoot/DOSFOP/config" w]
  puts $fd $globalConfig
  puts $fd STRUCTURES\n
  puts $fd $toplevelStructuresConfigGlobal
  puts $fd $toplevelStructuresConfig
  puts $fd $subsystemConfig
  puts $fd $libraryConfig

  close $fd
}

# former name constructSubsystemConfig {path subsystemList

proc dosfop-config-subsystems {subsystemList} {
    global dosfopRoot

    set result ""
    foreach actSubsystemName $subsystemList {
	set actSubsystemConfig \
	    [dosfop-readFile \
		 "$dosfopRoot/DOSFOP/$actSubsystemName.subsystems.config"]
	set actSubsystemStructuresConfigGlobal \
	    [dosfop-readFile \
		 "$dosfopRoot/DOSFOP/$actSubsystemName.structures.config"]
	
	set actSubsystemStructuresNames \
	    [dosfop-getNameList \
		 "$dosfopRoot/DOSFOP/$actSubsystemName.structures.names"]
	set actSubsystemStructuresConfig \
	    [dosfop-config-structures $actSubsystemStructuresNames]
	
	set subSubsystemNames \
	    [dosfop-getNameList "$dosfopRoot/DOSFOP/$actSubsystemName.subsystems.names"]
	
	set subSubsystemsConfig [dosfop-config-subsystems $subSubsystemNames]
	
	append result    "SUBSYSTEM\n"
	append result $actSubsystemConfig
	append result \STRUCTURES\n
	append result $actSubsystemStructuresConfigGlobal
	append result $actSubsystemStructuresConfig
	append result $subSubsystemsConfig
	append result \END_SUBSYSTEM\n
    }
 
    return $result
}

# former name constructStructuresConfig {path structureList} 
proc dosfop-config-structures {structureList} {
    global dosfopRoot

    set result ""
    foreach actStructureName $structureList {
	set actStructureConfig \
	    [dosfop-readFile "$dosfopRoot/DOSFOP/$actStructureName.config"]
	
	append result $actStructureConfig    
    }

    return $result
}    
    
### get information from the config file
# the output filename as specified by the user
# see dosfop-filehelp/dosfop-output-file-name

# state of structure index flag
proc dosfop-structure-index-flag { } {
    global dosfop_reg dosfopRoot

    return [dosfop-fileMatch $dosfop_reg(structure_index) \
		$dosfopRoot/DOSFOP/config]
}

# state of language
proc dosfop-language-flag { } {
    global dosfop_reg dosfopRoot

    return [dosfop-fileMatch $dosfop_reg(language) \
		$dosfopRoot/DOSFOP/config]
}

# state of arbitrary flag
proc dosfop-config-flag { flag } {
    global dosfop_reg dosfopRoot

    return [dosfop-fileMatch $dosfop_reg($flag) \
		$dosfopRoot/DOSFOP/config]
}
    

### tell whether config file needs to be constructed
# touch file to trigger config change
proc dosfop-mark-config-change { } {
    global dosfopRoot

    exec touch $dosfopRoot/DOSFOP/lastChange
}

# lastChange newer than config file ?
proc dosfop-config-old { } {
    global dosfopRoot

    if { ! [file exists $dosfopRoot/DOSFOP/lastChange] } then {
	return 1
    } elseif { ! [file exists $dosfopRoot/DOSFOP/config] } then {
	return 1
    } else {
	return [expr [file mtime $dosfopRoot/DOSFOP/lastChange] > \
		      [file mtime $dosfopRoot/DOSFOP/config]]
    }
}