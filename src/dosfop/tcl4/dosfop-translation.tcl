### calling the dosfop-translator itself
### and subsequent tools for processing 

# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl4/dosfop-translation.tcl,v 1.1.1.1 1998-06-16 16:00:44 wg Exp $

uplevel #0 append dosfopVersions {dosfop-translation\ 1.24\n}


### the dosfop translation proper
# call dosfop translation, output to stdout / stderr
proc dosfop-translation { } {	
    global dosfopTranslationProg dosfopRoot

    set dVal [ catch { exec $dosfopTranslationProg >@stdout 2>@stderr } dRet ]
    puts $dRet;
    exec cat diagnostics >@ stderr;
    if {$dVal != 0} {
	puts "DOSFOP: generation of texi intermediate output aborted."
	return -code error $dVal
    }
    
    dosfop-mv diagnostics $dosfopRoot/DOSFOP
}

# call dosfop translation, output to given file
proc dosfop-translation-redirect { fname } {
    global dosfopTranslationProg

    exec $dosfopTranslationProg 2> $fname
    exec cat diagnostics >> $fname

    dosfop-mv diagnostics $dosfopRoot/DOSFOP
}


### translation to dvi, 
# output to stderr / stdout
proc dosfop-dvi { } { dosfop-dvi-intern "" }

# output to filename
proc dosfop-dvi-redirect { filename } { dosfop-dvi-intern $filename }

# if filename is the empty string outputs to stdout / stderr
proc dosfop-dvi-intern { filename } {
    global dosfopRoot dosfop2dvi env dosfop2info

    set outputFileName [dosfop-output-file-name]
    dosfop-set-switches dvi
    cd $dosfopRoot/DOSFOP
    set env(TEXINDEX) dosfopTexindex
    set env(TEXINPUTS) $dosfopRoot/DOSFOP:$env(DOSFOP)/tex:$env(TEXINPUTS)
    set env(MAKEINFO) "dosfopMacroExpander"
    if {[string length $filename] == 0} then {
	set retCode [catch {exec $dosfop2dvi --verbose $outputFileName >@stdout 2>@stderr } result]
    } else {
	set retCode [catch {exec $dosfop2dvi --verbose $outputFileName >& $filename } result]
    }
    cd $dosfopRoot
    if {$retCode != 0} then {   
	puts "DOSFOP: translation to dvi aborted:"
	puts $result
	return -code error $retCode
    }
    dosfop-mv [dosfop-output-basename].dvi [dosfop-doc-dir]
}

## translation to info

# output to stderr / stdout
proc dosfop-info { } {dosfop-info-intern ""}
# output to filename
proc dosfop-info-redirect { filename } {dosfop-info-intern $filename}

# if filename is the empty string outputs to stdout / stderr
proc dosfop-info-intern { filename } {
    global dosfopRoot dosfop2info env

    dosfop-set-switches info
    set outputFileName [dosfop-output-file-name]
    set outputFileNameNew "${outputFileName}.new"
    # remove old files
    cd [dosfop-doc-dir]
    exec touch sentinel.info;  # ensure presence of one .html-file
    eval [concat [list exec rm] [glob *.info *.info-\[0-9\] *.info-\[0-9\]\[0-9\] ]]
    # now expand macros
    set incldir "$env(DOSFOP)/tex"
    set retCode [ catch {exec dosfopMacroExpander --verbose -I $dosfopRoot/DOSFOP -I $incldir -E $outputFileNameNew $outputFileName 2>@ stderr } ]
    if {$retCode != 0} {
	catch { exec rm $outputFileNameNew }
	puts "DOSFOP: macroexpansion returned with errors; translation to info aborted"
	cd $dosfopRoot
	return -code error $retCode
    }
    cd $dosfopRoot
    if {[string length $filename] == 0} then {
	set retCode [catch {exec $dosfop2info --no-validate --no-warn $outputFileNameNew -o [dosfop-doc-basename].info -I $dosfopRoot/DOSFOP -I $env(DOSFOP)/tex >@ stderr} result]
    } else {
	set retCode [catch {exec $dosfop2info --no-validate --no-warn $outputFileNameNew -o [dosfop-doc-basename].info -I $dosfopRoot/DOSFOP -I $env(DOSFOP)/tex >@ $filename} result]
    }
    exec rm $outputFileNameNew
    if {$retCode != 0} then {   
	puts "DOSFOP: errors/warnings during translation to info:"
	puts "$result"
	return -code error $retCode
    }
    
}


### translation to html

# output to stderr / stdout
proc dosfop-html { } { dosfop-html-intern "" }
# output to filename
proc dosfop-html-redirect { filename } { dosfop-html-intern $filename }
# if filename is the empty string outputs to stdout / stderr
proc dosfop-html-intern { filename } {
    global dosfop2html dosfopRoot env

    dosfop-set-switches html
    set outputFileName [dosfop-output-file-name]
    set outputFileNameNew "${outputFileName}.new"
    if {[string compare [dosfop-config-flag language] english] == 0} {
	set toc Table\ of\ Contents
    } else {
	set toc Inhaltsverzeichnis 
    }
    cd [dosfop-doc-dir]
    # remove old files
    exec touch sentinel.html;  # ensure presence of one .html-file
    eval [concat [list exec rm] [glob *.html]]
    set incldir "$env(DOSFOP)/tex"
    # now expand macros
    set retCode [ catch {exec dosfopMacroExpander --verbose -I $dosfopRoot/DOSFOP -I $incldir -E $outputFileNameNew $outputFileName 2>@ stderr } ]
    if {$retCode != 0} {
	catch { exec rm $outputFileNameNew }
	puts "DOSFOP: macroexpansion returned with errors; translation to HTML aborted"
	cd $dosfopRoot
	return -code error $retCode
    }
    dosfop-backup $outputFileName
    dosfop-mv $outputFileNameNew $outputFileName
    if {[string length $filename] == 0} then {
	set retCode [ catch {exec $dosfop2html -toc_name $toc -I $incldir -split_node -menu -verbose $outputFileName >@stdout 2>@stderr }]
    } else {
      set retCode [ catch {exec $dosfop2html -toc_name $toc -I $incldir -split_node -menu -verbose $outputFileName >& temp } ]
    }
    exec rm $outputFileName
    exec mv $outputFileName.old $outputFileName
    cd $dosfopRoot
    if {$retCode != 0} {
	puts "DOSFOP: translation to HTML aborted"
	return -code error $retCode
    }
}





### setting switches for the translation
proc dosfop-set-switches { kind } {
    global dosfopRoot

    # variable for switches
    set dosfop_switches(dvi,htmlflag) "@clear html"
    set dosfop_switches(dvi,colon)    "@set colon :"
    set dosfop_switches(info,htmlflag) "@clear html"
    set dosfop_switches(info,colon)    "@set colon ;"
    set dosfop_switches(html,htmlflag) "@set html"
    set dosfop_switches(html,colon)    "@set colon :"
    
    set swF $dosfopRoot/DOSFOP/dosfop.switches

    dosfop-writeFile $swF [concat "@c written: " [exec date] ]
    dosfop-appendFile $swF ""
    dosfop-appendFile $swF $dosfop_switches($kind,htmlflag)
    dosfop-appendFile $swF $dosfop_switches($kind,colon)

    if {[string compare [dosfop-structure-index-flag] on] == 0} {
	dosfop-appendFile $swF "@set structureindexflag"
  }
    if {[string compare [dosfop-config-flag functionality_index] off] == 0} {
	dosfop-appendFile $swF "@clear dfDomainIndexFlag"
	dosfop-appendFile $swF "@clear dfCodomainIndexFlag"
	dosfop-appendFile $swF "@clear dfPropertyIndexFlag"
    } else {
	dosfop-appendFile $swF "@set dfDomainIndexFlag"
	dosfop-appendFile $swF "@set dfCodomainIndexFlag"
	dosfop-appendFile $swF "@set dfPropertyIndexFlag"
    }
    if {[string compare [dosfop-language-flag] english] == 0} {
	dosfop-appendFile $swF "@set dfEnglish"
  }
    if {[string compare [dosfop-language-flag] german] == 0} {
	dosfop-appendFile $swF "@set dfGerman"
  }
    if {[string compare [dosfop-config-flag single_node] off] == 0} {
      dosfop-appendFile $swF "@clear dfSingleNode"
  } else {
      dosfop-appendFile $swF "@set dfSingleNode"
  }

    dosfop-appendFile $swF "@set dfProjectName [dosfop-config-flag project_name]"
    dosfop-appendFile $swF "@set dfAuthors [dosfop-config-flag authors]"
    dosfop-appendFile $swF "@set dfDate [dosfop-config-flag date]"
    dosfop-appendFile $swF "@set dfSort[dosfop-config-flag sort_structures]"
}

