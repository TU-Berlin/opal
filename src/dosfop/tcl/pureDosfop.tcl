#! /usr/local/bin/wish -f
## managing dosfop tools without window system

source $env(DOSFOP)/tcl/pureExec.tcl

proc outputHelp { x } {
  global argv0

puts "usage: $argv0 \[option\]
possible options:
-addonly <structure name> ...    add structure to only list
-cc	                         checkConsistency
-clearonly                       clear only list
-config                          construct config file
-configascii                     delete binary config file
-configbin                       switch to binary config file
-dosfop                          dosfop  (default)
-dvi	                         make printable documentation
-info                            make info hypertext
-html                            make WWW hypertext
-usage	                         this information

"

exit $x
}

if {$argc == 0} {
	# dosfop aufrufen
	pureDosfop 0
}
if {$argc == 1} {
	set select [lindex $argv 0]
	if {$select == "-cc"} { catch {pureCheckConsistency 0} result
				      puts $result
	} elseif {$select == "-clearonly"} { catch {pureClearOnly} result
					  puts $result
	} elseif {$select == "-config"} { catch {pureConstructConfig} result
					  puts $result
	} elseif {$select == "-configascii"} { catch {pureConfigAscii} result
					  puts $result
	} elseif {$select == "-configbin"} { catch {pureConfigBin} result
					  puts $result
	} elseif {$select == "-dosfop"} { catch {pureDosfop 0} result
					  puts $result
	} elseif {$select == "-dvi"} { catch {pureTexi2dvi 0} result
				       puts $result
	} elseif {$select == "-info"} { catch {pureMakeinfo 0} result
					puts $result
	} elseif {$select == "-html"} { catch {pureTexi2html 0} result
					puts $result
	} elseif {$select == "-qhtml"} { catch {pureTexi2qhtml 0} result
					puts $result
	} elseif {$select == "-usage"} { outputHelp 0 
	} else {puts "wrong usage! unknown option $select" 
		outputHelp 1
	}
}
if {$argc > 1} {
	set select [lindex $argv 0]
	if {$select == "-addonly"} { catch {pureAddOnly [lrange $argv 1 end]} result
				puts $result
	} else {
	    puts "wrong usage! unknown option $select"
	    outputHelp 1
	}
}

exit 
