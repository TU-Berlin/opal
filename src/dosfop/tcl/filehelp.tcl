# $Id$
source $env(DOSFOP)/tcl/global.tcl
source $env(DOSFOP)/tcl/regexprs.tcl

#####################################################################################
# transformation internal name <-> external-file-name #
#######################################################

proc space2underline name {
  regsub -all " " $name "_" result
  return $result
}
proc underline2space name {
  regsub -all "_" $name " " result
  return $result
}

#####################################################################################
# location to different filenames #
###################################

proc loc2SubsFile location {
  return "DOSFOP/[space2underline $location].subsystems.names"
}
proc loc2SubsConfigFile location {
  return "DOSFOP/[space2underline $location].subsystems.config"
}
proc loc2StructsFile location {
  return "DOSFOP/[space2underline $location].structures.names"
}
proc loc2StructsConfigFile location {
  return "DOSFOP/[space2underline $location].structures.config"
}
proc struct2StructConfigFile name {
  return "DOSFOP/$name.config"
}


###################################################################################
# get infos from configuration #
################################

proc location2Path location {
  global reg

  if {[string compare $location "Toplevel"] == 0} then {
    set filename [struct2StructConfigFile Toplevel]
  } else {
    set filename [loc2SubsConfigFile $location]
  }

  set content [readFile $filename]
  return [getMatch $reg(location) $content]
}


proc getOutputFileName {} {
  global reg

  set filename [struct2StructConfigFile Toplevel]
  set content [readFile $filename]
  return [getMatch $reg(output_file) $content]
}  

proc getProjectName {} {
  global reg

  set filename [struct2StructConfigFile Toplevel]
  set content [readFile $filename]
  return [getMatch $reg(project_name) $content]
}  

proc getROOTname {} {
  return [file rootname [file tail [getOutputFileName]]]
}

proc getDVIname {} {
  return [getROOTname].dvi
}

proc getINFOname {} {
  # vorher  [file dirname [getOutputFileName]]/[getTopstructureName].info 
  return [getROOTname].info
}

proc getHTMLname {} {
  return [getROOTname].html
}

proc getSRCname {} {
  return [file dirname [file dirname [getOutputFileName]]]
}

proc getDOCname {} {
    return [getSRCname]/doc
}


proc getTopstructureName {} {
  global reg

  set filename [struct2StructConfigFile Toplevel]
  set content [readFile $filename]
  return [getMatch $reg(top_structure) $content]
}  

proc getStructureIndexFlag {} {
  global reg

  set filename [struct2StructConfigFile Toplevel]
  set content [readFile $filename]
  return [getMatch $reg(structure_index) $content]
}  

proc getLanguageFlag {} {
  global reg

  set filename [struct2StructConfigFile Toplevel]
  set content [readFile $filename]
  return [getMatch $reg(language) $content]
}  

proc getConfigFlag { flag } {
  global reg

  set filename [struct2StructConfigFile Toplevel]
  set content [readFile $filename]
  return [getMatch $reg($flag) $content]
}  


#############################################################################
# initialization #
##################

proc initSubsystemConfig {name location} {

  set fd [open "DOSFOP/[space2underline $name].subsystems.config" w]
  puts $fd "name = \{$name\}" 
  puts $fd "directory = $location"
  close $fd

  set fd [open "DOSFOP/[space2underline $name].structures.config" w]
  puts $fd "OPTIONAL" 
  close $fd

}

proc initStructureConfig name {
  set fd [open "DOSFOP/[space2underline $name].config" w]
  puts $fd "STRUCTURE"
  puts $fd "name = \{$name\}" 
  puts $fd "OPTIONAL"
  close $fd
}


###############################################################################
# complete deletion of components #
###################################

proc deleteSubsystemConfig name {

  # delete configuration of subsystem to be deleted
  set filename [loc2SubsConfigFile $name]
  if {[file exists $filename]} then {
    exec mv $filename $filename.old
  }

  # delete subsystems' names of subsystem to be deleted
  set filename [loc2StructsFile $name]
  if {[file exists $filename]} then {
    exec mv $filename $filename.old
  }
  # delete structures' names of subsystem to be deleted
  set filename [loc2StructsConfigFile $name]
  if {[file exists $filename]} then {
    exec mv $filename $filename.old
  }

  #delete all references to the subsystem to be deleted
  foreach actFilename [glob -nocomplain [loc2SubsFile *]] {

    set actContent [readFile $actFilename]
    set match [regsub "$name\n" $actContent "" newContent]

    if {$match == 1}  then {
      exec mv $actFilename $actFilename.old 
      writeFile $actFilename $newContent
    }
  }
}

proc deleteStructureConfig name {
  set filename [struct2StructConfigFile $name]
  exec mv $filename "$filename.old"

  #delete all references to the structure to be deleted
  foreach actFilename [glob -nocomplain [loc2StructsFile *]] {

    set actContent [readFile $actFilename]
    set match [regsub "$name\n" $actContent "" newContent]

    if {$match == 1}  then {
      exec mv $actFilename $actFilename.old 
      writeFile $actFilename $newContent
    }
  }
}

###############################################################################
# complete renaming of components #
###################################

proc renameSubsystemConfig {oldName newName} {

  # rename configuration of subsystem to be renamed
  set filename [loc2SubsConfigFile $oldName]
  if {[file exists $filename]} then {
    exec mv $filename [loc2SubsConfigFile $newName]
  }

  # rename subsystems' names of subsystem to be renamed
  set filename [loc2StructsFile $oldName]
  if {[file exists $filename]} then {
    exec mv $filename [loc2StructsFile $newName]
  }
  # rename structures' names of subsystem to be renamed
  set filename [loc2StructsConfigFile $oldName]
  if {[file exists $filename]} then {
    exec mv $filename [loc2StructsConfigFile $newName]
  }

  # delete all references to the subsystem to be deleted
  foreach actFilename [glob -nocomplain [loc2SubsFile *]] {
    
    puts $actFilename
    set actContent [readFile $actFilename]
    set match [regsub "$oldName\n" $actContent "$newName\n" newContent]

    if {$match == 1}  then {
      exec mv $actFilename $actFilename.old 
      writeFile $actFilename $newContent
    }
  }
}

proc renameStructureConfig  {oldName newName} {
  set filename [struct2StructConfigFile $oldName]
  exec mv $filename [struct2StructConfigFile $newName]

  #delete all references to the structure to be deleted
  foreach actFilename [glob -nocomplain [loc2StructsFile *]] {

    set actContent [readFile $actFilename]
    set match [regsub "$oldName\n" $actContent "$newName\n" newContent]

    if {$match == 1}  then {
      exec mv $actFilename $actFilename.old 
      writeFile $actFilename $newContent
    }
  }
}


#################################################################################
# file-name completion like C-shell #
#####################################

proc completion settingType {
  global setting

  set compl [glob -nocomplain "$setting($settingType)*"]
  if {[llength [split $compl " "]] == 1} then {
    set setting($settingType) [lindex $compl 0]
  } else {
    beep
    flush stdout
  }
}


######################################################################################
# handling of settings #
########################
######################################
# extracting the predefined-settings #
######################################
# based on the given "optionList" this function tries to extract the specified
# option from the given file. For each element in "optionList" there has to be 
# a corresponding regular expression in the global "reg" array. For each element
# a (at least empty "") setting is put in the "setting" array. 
#
proc readSettings {filename optionList} {
  global reg
  global setting

  set fdin [open $filename r]
  set content [read $fdin]

  foreach i $optionList {
    set setting($i) [getMatch $reg($i) $content]
  }
}

##############################################
# extracting the predefined-context-settings #
##############################################
# based on the given "optionList" this function tries to extract the specified
# option from the given str. For each element in "optionList" there has to be 
# a corresponding regular expression in the global "reg" array. For each element
# a (at least empty "") setting is put in the "setting" array. 
#
proc extractContextSettings {str optionList} {
  global reg
  global setting

  foreach i $optionList {
    set setting($i,context) [getMatch $reg($i) $str]
  }
}

################################
# writing out the new settings #
################################

proc saveConfig {filename option_ordering} {
  global setting
  global out

  set fdout [open $filename w+]
  foreach i $option_ordering {
    if {(($setting($i) != "" ) && ($setting($i) != " " )) || ($i == "survey") } then {
      regsub xxx $out($i) $setting($i) result
      puts $fdout $result
    }
  }
  close $fdout
}	

# common exit for all configurators:

proc cancelConfig {} {
  destroy .
  exit
}

###################################################################################
# construct config-file from parts #
####################################

proc constructConfigFile path {
    global env

  set globalConfig [readFile "$path/DOSFOP/Toplevel.config"]

  set subsystemList [getNameList "$path/DOSFOP/Toplevel.subsystems.names"]

  set toplevelStructuresConfigGlobal [readFile "$path/DOSFOP/Toplevel.structures.config"]
  set toplevelStructuresNames [getNameList "$path/DOSFOP/Toplevel.structures.names"]
  set toplevelStructuresConfig [constructStructuresConfig $path $toplevelStructuresNames]

  set subsystemConfig [constructSubsystemConfig $path $subsystemList]

    if [file exists "$path/DOSFOP/Library.config"] {
	set libraryConfig [readFile "$path/DOSFOP/Library.config"]
    } else {
	set libraryConfig [readFile "$env(DOSFOP)/defaults/Library.config"]
    }

  set fd [open "$path/DOSFOP/config" w]
  puts $fd $globalConfig
  puts $fd STRUCTURES\n
  puts $fd $toplevelStructuresConfigGlobal
  puts $fd $toplevelStructuresConfig
  puts $fd $subsystemConfig
  puts $fd $libraryConfig

  close $fd
}   

proc constructSubsystemConfig {path subsystemList} {
  if {[llength $subsystemList] == 0} then { 
    return ""
  } else {
    set actSubsystemName [lindex $subsystemList 0]
   
    set actSubsystemConfig [readFile "$path/DOSFOP/$actSubsystemName.subsystems.config"]
    set actSubsystemStructuresConfigGlobal \
   	  [readFile "$path/DOSFOP/$actSubsystemName.structures.config"]
   
    set actSubsystemStructuresNames \
   	  [getNameList "$path/DOSFOP/$actSubsystemName.structures.names"]

     set actSubsystemStructuresConfig \
   	  [constructStructuresConfig $path $actSubsystemStructuresNames]
  
    set subSubsystemNames [getNameList "$path/DOSFOP/$actSubsystemName.subsystems.names"]

    set subSubsystemsConfig [constructSubsystemConfig $path $subSubsystemNames]
 
    set restSubsystemsConfig \
   	  [constructSubsystemConfig $path [lrange $subsystemList 1 end]] 
 
 
    set result    "SUBSYSTEM\n"
    append result $actSubsystemConfig
    append result \STRUCTURES\n
    append result $actSubsystemStructuresConfigGlobal
    append result $actSubsystemStructuresConfig
    append result $subSubsystemsConfig
    append result \END_SUBSYSTEM\n
    append result $restSubsystemsConfig
 
    return $result
  }
}

proc constructStructuresConfig {path structureList} {
  if {[llength $structureList] == 0} then { 
    return ""
  } else {
    set actStructureName [lindex $structureList 0]
    set actStructureConfig [readFile "$path/DOSFOP/$actStructureName.config"]

    set restStructuresConfig \
          [constructStructuresConfig $path [lrange $structureList 1 end]]

    set    result $actStructureConfig    
    append result $restStructuresConfig

    return $result
  }
}    
    
proc getNameList {filename} {
  set list ""
  if [file exists $filename] then {
    set fd [open $filename r]
    while {[eof $fd] != 1} {  
      gets $fd name
	set name [string trim $name]
      if {[string length $name] != 0} then {
	  lappend list $name
      }
    }
    close $fd
  }
  return $list
}


###################################################################################
# basic file-IO #
#################

proc readFile {filename} {
  if {[file exists $filename]} then {
    set fd [open $filename r]
    set result [read $fd]
    close $fd
    return $result
  } else {
    return ""
  }
}

proc writeFile {filename content} {
  set fd [open $filename w]
  puts -nonewline $fd $content
  close $fd
}

proc appendFile {filename content} {
  set fd [open $filename a]
  puts $fd $content
  close $fd
}

proc del filename {
  if {[file exists $filename]} then {exec rm $filename}
}

###################
# misc
###################

proc markConfigChange { } {
  exec touch [getSRCname]/DOSFOP/lastChange
}
