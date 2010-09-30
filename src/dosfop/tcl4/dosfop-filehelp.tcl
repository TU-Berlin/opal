### proc's for filehandling

# $Id$ 

uplevel #0 append dosfopVersions {dosfop-filehelp\ 1.07\n}

#######################################################
# transformation internal name <-> external-file-name #
#######################################################

proc dosfop-space2underline name {
  regsub -all " " $name "_" result
  return $result
}
proc dosfop-underline2space name {
  regsub -all "_" $name " " result
  return $result
}

###################################
# location to different filenames #
###################################

proc dosfop-loc2SubsFile location {
  global dosfopRoot

  return "$dosfopRoot/DOSFOP/[space2underline $location].subsystems.names"
}
proc dosfop-loc2SubsConfigFile location {
  return "$dosfopRoot/DOSFOP/[space2underline $location].subsystems.config"
}
proc dosfop-loc2StructsFile location {
  return "$dosfopRoot/DOSFOP/[space2underline $location].structures.names"
}
proc dosfop-loc2StructsConfigFile location {
  return "$dosfopRoot/DOSFOP/[space2underline $location].structures.config"
}
proc dosfop-struct2StructConfigFile name {
  return "$dosfopRoot/DOSFOP/$name.config"
}


### some special file names (formerly with get...)

# (formerly getOutputFilename)
# filename of intermediate texinfo output
proc dosfop-output-file-name { } {
    global dosfop_reg dosfopRoot

    return [dosfop-fileMatch \
		$dosfop_reg(output_file) $dosfopRoot/DOSFOP/config]
}

# basename of output file name
proc dosfop-output-basename { } { 
    return  [file rootname [dosfop-output-file-name]]
}

# basename in doc directory
proc dosfop-doc-basename { } {
    return [dosfop-doc-dir]/[file tail [dosfop-output-basename]]
}

# directory where to put docuemntation
proc dosfop-doc-dir { } {
    global dosfopRoot

    return $dosfopRoot/doc
}

#####################################
# file-name completion like C-shell #
#####################################

proc dosfop-completion settingType {
  global setting

  set compl [glob -nocomplain "$setting($settingType)*"]
  if {[llength [split $compl " "]] == 1} then {
    set setting($settingType) [lindex $compl 0]
  } else {
    beep
    flush stdout
  }
}

#################
# basic file-IO #
#################

proc dosfop-readFile {filename} {
  if {[file exists $filename]} then {
    set fd [open $filename r]
    set result [read $fd]
    close $fd
    return $result
  } else {
    return ""
  }
}

proc dosfop-writeFile {filename content} {
  set fd [open $filename w]
  puts -nonewline $fd $content
  close $fd
}

proc dosfop-appendFile {filename content} {
  set fd [open $filename a]
  puts $fd $content
  close $fd
}

proc dosfop-rm filename {
  if {[file exists $filename]} then {exec rm $filename}
}

proc dosfop-cp { src dest } {
    if {[file exists $src]} then {exec cp $src $dest}
}

proc dosfop-mv { src dest } {
    if {[file exists $src]} then {exec mv $src $dest}
}

proc dosfop-backup { filename } {
    if {[file exists $filename]} then {
	exec mv $filename $filename.old
    }
}



# mods is a list of pairs { from-regexp to-regexp }
proc dosfop-cp-modify { src dest mods } {

    set contents [dosfop-readFile $src]
    foreach mod $mods {
	regsub -all [lindex $mod 0] $contents [lindex $mod 1] contents
    }
    dosfop-writeFile $dest $contents
}

# expand argument path
proc dosfop-expand { p } {
    if [file exists $p] then {
	set oldpath [pwd]
	cd $p
	set newpath [pwd]
	cd $oldpath
	return $newpath 
    } else {
	return ""
    }    	    
}

# former getNamelist
# return list of names contained in file
proc dosfop-getNameList {filename} {
  set list ""
  if [file exists $filename] then {
    set fd [open $filename r]
    while {[eof $fd] != 1} {  
      gets $fd name
      if {[string length $name] != 0} then {
        lappend list $name
      }
    }
    close $fd
  }
  return $list
}


###################
# misc            #
###################

proc dosfop-markConfigChange { } {
  exec touch [getSRCname]/DOSFOP/lastChange
}
