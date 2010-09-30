### handling of subsystems by DOSFOP
# $Id$


uplevel #0 append dosfopVersions {dosfop-subsystem\ 1.04\n}

# former name deleteSubsystemConfig
# delete subsystem from DOSFOP database
proc dosfop-subsystem-delete name {

  # delete configuration of subsystem to be deleted
  dosfop-backup [dosfop-loc2SubsConfigFile $name]

  # delete subsystems' names of subsystem to be deleted
  dosfop-backup [dosfop-loc2StructsFile $name]

  # delete structures' names of subsystem to be deleted
  dosfop-backup [dosfop-loc2StructsConfigFile $name]

  #delete all references to the subsystem to be deleted
  foreach actFilename [glob -nocomplain [dosfop-loc2SubsFile *]] {

    set actContent [dosfop-readFile $actFilename]
    set match [regsub "$name\n" $actContent "" newContent]

    if {$match == 1}  then {
      dosfop-backup $actFilename 
      writeFile $actFilename $newContent
    }
  }
  dosfop-mark-config-change
}

# former name initSubsystemConfig
# initialize necessary files for a new subsystem
proc dosfop-subsystem-init {name location} {
  global dosfopRoot

    set rname [dosfop-space2underline $name]
    dosfop-writeFile $dosfopRoot/DOSFOP/$rname.structures.config "OPTIONAL\n"
    dosfop-writeFile $dosfopRoot/DOSFOP/$rname.structures.names ""
    dosfop-writeFile $dosfopRoot/DOSFOP/$rname.subsystems.names ""
    dosfop-writeFile $dosfopRoot/DOSFOP/$rname.subsystems.config \
	"  name = {$name}
  directory = $location
SURVEY
| |
OPTIONAL
"

    dosfop-mark-config-change

}

# add a new subsytem to the dosfop database
proc dosfop-subsystem-new { name location parentname } {
    global dosfopRoot
  
    dosfop-subsystem-init $name $location
    dosfop-appendFile $dosfopRoot/DOSFOP/$parentname.subsystems.names $name
}


# does subsystem exist?
# name is user name, not path 
proc dosfop-subsystem-exist { name } {
    return [glob -nocomplain [dosfop-loc2SubsConfigFile $name]] != {}
}

# return path of canonical parent of subsystem sub (pathname)
# puts "dosfop-subsystem-parent"
proc dosfop-subsystem-parent { sub PathsKnown } {
    global dosfopRoot 

    set candParent [file dirname $sub]
    set idx [lsearch $PathsKnown $candParent]
    if { $idx == -1 } then {
	return $dosfopRoot
    } else {
	return $candParent
    }
}

# puts "dosfop-current-subssytems"
proc dosfop-current-subsystems { } {
    global dosfopRoot

    set cands [glob -nocomplain "$dosfopRoot/DOSFOP/*.subsystems.names"]
    set subnames { }
    foreach c $cands {
	set fid [open $c "r"]
	set csubnames [split [read $fid] "\n"]
	foreach sub $csubnames {
	    set ssub [string trim $sub]
	    if { [ string length $ssub ] > 0 } then {
		lappend subnames $ssub
	    }
	}
	close $fid
#	puts "<$c>:$csubnames"
    }
    return $subnames
}

# input: a list of subsystem names known to dosfop
# output: an array in array set/get format from paths to names
proc dosfop-current-subsystems-paths { l } {
    global dosfopRoot

    foreach s $l {
	set fid [open "$dosfopRoot/DOSFOP/$s.subsystems.config" "r"]
	regexp "directory = (.*)\nSURVEY" [read $fid] m dir
#	puts "***\n$m\n***\n$dir\n***"
	set spArray($dir) $s
	close $fid
    }
    return [array get spArray]
}
