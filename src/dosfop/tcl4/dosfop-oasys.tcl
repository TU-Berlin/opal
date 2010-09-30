#### oasys commands for documentation
# $Id$

uplevel #0 append dosfopVersions {dosfop-oasys\ 1.08\n}


# true, if some unchecked units exist
proc dosfop-unchecked { } {

    set uinfo [oasys-units full]
    foreach u [split $uinfo "\n"] {
	if { ! [regexp checked $u] } then {
	    return [expr [string length [string trim $u]] > 0]
	}
    }
    return 0
}

# true if texi is older than config
proc dosfop-old-texi { } {
    global dosfopRoot

    if {![file exists $dosfopRoot/DOSFOP/config]} then {return 1}
    if {![file exists [dosfop-output-file-name]]} then {return 1}

    return [expr [file mtime $dosfopRoot/DOSFOP/config] > \
		[file mtime [dosfop-output-file-name]]]
}

# generate new texi, if necessary
proc dosfop-check-texi { } {
    global dosfopRoot

    # first reload new or changed units
    eval oasys-reload [oasys-units]
    set dosfopRoot [pwd]
    dosfop-init
    dosfop-update
    set translate [expr [dosfop-unchecked] || \
		       [dosfop-config-old] || [dosfop-old-texi]]
    if { [dosfop-unchecked] } then { oasys-check }
    if { $translate } then {
	dosfop-config
	dosfop-translation
    }
}

##############################
# oasys-commands

oasys-proc \
    {doc-dvi} \
    {Generate documentation in dvi format.} \
    {CMD} \
    {dosfop-check-texi; dosfop-dvi}

oasys-proc \
    {doc-info} \
    {Generate documentation in info format.} \
    {CMD} \
    {dosfop-check-texi; dosfop-info}

oasys-proc \
    {doc-html} \
    {Generate documentation in html format.} \
    {CMD} \
    {dosfop-check-texi; dosfop-html}

oasys-proc \
    {doc-help} \
    {Help for doc-commands.} \
    {CMD} \
    {puts "Generation of documentation with the dosfop system.

doc-dvi    Generate documentation in dvi  format.
doc-info   Generate documentation in info format.
doc-html   Generate documentation in html format.

All of these commands first reload units which have been changed and check 
these units afterwards, because generation of documentation requires 
Interopal files. Units and subsystems which are not yet part of the dosfop 
database are inserted with default configurations. Existing configurations 
remain unchanged. Iff the configuration of any structures or subsystems has 
changed or any units had to be reloaded or the dosfop database has changed, 
the intermediate texinfo file is newly generated. From this texinfo file, the 
documentation is generated in the desired format. 

The results are placed in the doc directory.

Changes in the configuration have not been integrated into oasys; use the 
dosfop program to do this."
    }


