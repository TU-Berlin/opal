### some tcl commands for integrating dosfop into the oasys-environment
### we are using dosfop- as prefix for names used by procedures or variables 
### from this file 

# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl4/dosfop-update.tcl,v 1.1.1.1 1998-06-16 16:00:44 wg Exp $

uplevel #0 append dosfopVersions {dosfop-update\ 1.04\n}

# update dosfop database according to oasys database
proc dosfop-update { } {
    global dosfopRoot 

    ## read information from oasys
    puts "get information from oasys ... "
    # dosfopLibPaths: list of library paths
    set dosfopLibPaths { }
    foreach p [split [oasys-intern-path] ","] {
	if { [lindex $p 0] == [lindex $p 1] } then {
	    lappend dosfopLibPaths [dosfop-expand [lindex $p 1]]}
    }
    # dosfopStructs: array, path -> list of structures in path
    # dosfopSPath: array, structure name -> path
    foreach un [oasys-units file] {
	if { [regexp "(.*)/(.*)\.sign" $un m p u] == 1} then {
	    lappend dosfopStructs([dosfop-expand $p]) $u
	    set dosfopSPath($u) [dosfop-expand $p]
	} 
    }
    
    ## read information from dosfop
    puts "get information from dosfop ..."
    # knownSubsystems: all subsystems known to dosfop
    set knownSubsystems [dosfop-current-subsystems]
    # PathsKnownSubsystems: array, path -> subsystem name
    array set PathsKnownSubsystems \
         [dosfop-current-subsystems-paths [dosfop-current-subsystems]]
    set PathsKnownSubsystems($dosfopRoot) Toplevel
    # knownStructs: list of structures known to dosfop
    set knownStructs [dosfop-current-structures]

    puts "checking subsystems ..."
    foreach sub [dosfop-lsort-len [array names dosfopStructs]] {
	if { [lsearch $dosfopLibPaths $sub] != -1 } then {
#	    puts "ignoring library subsystem $sub"
	} elseif { [llength [array names PathsKnownSubsystems $sub]] == 1} then {
#	    puts "already known to DOSFOP: $sub"
	} else {
	    puts "new subsystem $sub"
	    dosfop-subsystem-new [file tail $sub ] $sub \
		$PathsKnownSubsystems([dosfop-subsystem-parent $sub \
		     [array names PathsKnownSubsystems]])
	    set PathsKnownSubsystems($sub) [file tail $sub]
	    lappend knownSubsystems $sub
	}
    }

    puts "checking units ..."
    foreach un [array names dosfopSPath] {
	if { [lsearch $dosfopLibPaths $dosfopSPath($un)] != -1 } then {   
#	    puts "ignoring library structure $un"
	} elseif { [lsearch -exact $knownStructs $un] != -1 } then {
#	    puts "already known structure $un"
	} else {
	    puts "new structure $un"
	    dosfop-structure-new [file tail $un] $PathsKnownSubsystems($dosfopSPath($un))
	    lappend knownStructs $un
	}
    }

}


## Auxiliary procedures ####################

# print contents of an array
proc dosfop-print-array { name } {
    upvar $name a
    foreach i [lsort [array names a]] {
	puts "$name.$i = $a($i)"
    }
}

# sort list increasing by length
proc dosfop-lsort-len { l } {
    return [lsort -command dosfop-compare-len $l]
}

proc dosfop-compare-len {l1 l2} {
    return [expr [string length $l1] - [string length $l2]]
}


