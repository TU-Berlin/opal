### handling structures in the DOSFOP database

# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl4/dosfop-structure.tcl,v 1.1.1.1 1998-06-16 16:00:44 wg Exp $

uplevel #0 append dosfopVersions {dosfop-structure\ 1.06\n}

# does structure exist in dosfop database?
proc dosfop-structure-exist { struct } {
    return [glob -nocomplain [dosfop-struct2StructConfigFile $struct]] != {}
}

# former name deleteStructure
# delete structure from database
proc dosfop-structure-delete { struct } {

    dosfop-backup [dosfop-struct2StructConfigFile $struct]  

    foreach actFilename [glob -nocomplain [dosfop-loc2StructsFile *]] {

	set actContent [dosfop-readFile $actFilename]
	set match [regsub "$name\n" $actContent "" newContent]
	
	if {$match == 1}  then {
	    dosfop-backup $actFilename 
	    dosfop-writeFile $actFilename $newContent
	}
    }
    dosfop-mark-config-change
}

# former name initStructureConfig
# set config for a new structure of name name
proc dosfop-structure-init { name } {
    global dosfopRoot

    set rname [dosfop-space2underline $name]
    dosfop-writeFile $dosfopRoot/DOSFOP/$rname.config \
"STRUCTURE
name = \{$name\}
OPTIONAL
" 
    dosfop-mark-config-change
}

# add a new structure to the dosfop database
proc dosfop-structure-new { name subname } {
    global dosfopRoot

    dosfop-structure-init $name
    dosfop-appendFile $dosfopRoot/DOSFOP/$subname.structures.names $name
}


# construct a list of all currently known structures to DOSFOP
# we're not doing this hierarchically, we rather just read in
# all XX.structures.names files
proc dosfop-current-structures { } {
    global dosfopRoot

    set cands [glob -nocomplain "$dosfopRoot/DOSFOP/*.structures.names"]
    set strnames { }
    foreach c $cands {
	set fid [open $c "r"]
	set cstrnames [split [read $fid] "\n"]
	foreach s $cstrnames {
	    set ss [string trim $s]
	    if { [string length $ss] > 0 } then {
		lappend strnames $ss
	    } 
	}
	close $fid
    }
    return $strnames
}
    
