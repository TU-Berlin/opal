## this file contains the calls for the pure DOSFOP tools i.e. without
## any commands depending on a certain window environemnt

source $env(DOSFOP)/tcl/filehelp.tcl
source $env(DOSFOP)/tcl/output.tcl

proc pureSetDosfopSwitches { info html } {

  global setting
  
  writeFile DOSFOP/dosfop.switches [concat "@c written: " [exec date]]
  appendFile DOSFOP/dosfop.switches ""

  if ($html) {
      appendFile DOSFOP/dosfop.switches "@set html"
  } else {
      appendFile DOSFOP/dosfop.switches "@clear html"
  }
  if ($info) {
      appendFile DOSFOP/dosfop.switches "@set colon ;" 
  } else { 
      appendFile DOSFOP/dosfop.switches "@set colon :"
  }
  if {[string compare [getStructureIndexFlag] on] == 0} {
      appendFile DOSFOP/dosfop.switches "@set structureindexflag"
  }
  if {[string compare [getConfigFlag functionality_index] off] == 0} {
      appendFile DOSFOP/dosfop.switches "@clear dfDomainIndexFlag"
      appendFile DOSFOP/dosfop.switches "@clear dfCodomainIndexFlag"
      appendFile DOSFOP/dosfop.switches "@clear dfPropertyIndexFlag"
  } else {
      appendFile DOSFOP/dosfop.switches "@set dfDomainIndexFlag"
      appendFile DOSFOP/dosfop.switches "@set dfCodomainIndexFlag"
      appendFile DOSFOP/dosfop.switches "@set dfPropertyIndexFlag"
  }
  if {[string compare [getLanguageFlag] english] == 0} {
      appendFile DOSFOP/dosfop.switches "@set dfEnglish"
  }
  if {[string compare [getLanguageFlag] german] == 0} {
      appendFile DOSFOP/dosfop.switches "@set dfGerman"
  }
  if {[string compare [getConfigFlag single_node] off] == 0} {
      appendFile DOSFOP/dosfop.switches "@clear dfSingleNode"
  } else {
      appendFile DOSFOP/dosfop.switches "@set dfSingleNode"
  }
  
  appendFile DOSFOP/dosfop.switches "@set dfProjectName [getConfigFlag project_name]"
  appendFile DOSFOP/dosfop.switches "@set dfAuthors [getConfigFlag authors]"
  appendFile DOSFOP/dosfop.switches "@set dfDate [getConfigFlag date]"
  appendFile DOSFOP/dosfop.switches "@set dfSort[getConfigFlag sort_structures]"
  
}

proc pureCheckConsistency {redirect} {
  global env

  set configPath "[pwd]/"
  if ($redirect) {
      exec $env(DOSFOP)/bin/checkConsistency $configPath 2>temp >ErrorOutput
  } else {
      set retCode [ catch {exec $env(DOSFOP)/bin/checkConsistency $configPath >@stdout 2>@stderr} ]
      if {$retCode != 0} {
        puts "DOSFOP: check consistency aborted."
        exit $retCode
      }
  }
}

proc pureDosfop {redirect} {
  global env

    if ($redirect) {
	exec $env(DOSFOP)/bin/dosfopTranslator 2> temp
	exec cat diagnostics >> temp
    } else {
#	set dVal [ catch { exec $env(DOSFOP)/bin/dosfopTranslator >@ stderr } dRet ]
	set dVal [ catch { exec $env(DOSFOP)/bin/dosfopTranslator >@stdout 2>@stderr } dRet ]	
        puts $dRet;
	exec cat diagnostics >@ stderr;
        if {$dVal != 0} {
           puts "DOSFOP: generation of texi intermediate output aborted."
           exit $dVal
        }
    }
    catch { exec mv diagnostics DOSFOP }
}

proc pureTexi2dvi {redirect} {
    global env

    set outputFileName [getOutputFileName]
    pureSetDosfopSwitches 0 0
    cd DOSFOP
    if {[string range $outputFileName 0 0] == "/"} {
	set up ""
    } else {
	set outputFileName "../$outputFileName"
	set up "..:" 
    }
    set env(TRUETEXINDEX) $env(TEXINDEX)
    set env(TEXINDEX) $env(DOSFOP)/bin/dosfopTexindex
    set env(TEXINPUTS) .:$up$env(DOSFOP)/tex:$env(TEXINPUTS)
    set env(MAKEINFO) "$env(PERL) $env(DOSFOP)/bin/dosfopMacroExpander"
    if ($redirect) {
	set retCode [catch {exec $env(DOSFOP)/bin/texi2dvi --verbose $outputFileName >& ../temp  }]
    } else {
	set retCode [catch {exec $env(DOSFOP)/bin/texi2dvi --verbose $outputFileName >@stdout 2>@stderr }]
    } 
    if {$retCode != 0} {
      puts "DOSFOP: translation to dvi aborted."
      exit $retCode
    }
    cd ..
    catch {exec mv DOSFOP/[getDVIname] [getDOCname]/[getDVIname]}
}

proc pureMakeinfo {redirect} {
  global env

    pureSetDosfopSwitches 1 0 
    set oldName [getOutputFileName].old
    set newName [getOutputFileName].new
    if ($redirect) {
	catch {exec $env(PERL) $env(DOSFOP)/bin/dosfopMacroExpander --verbose -I [getSRCname]/DOSFOP -I $env(DOSFOP)/tex -E $newName [getOutputFileName] >& temp }
    } else {
	catch {exec $env(PERL) $env(DOSFOP)/bin/dosfopMacroExpander --verbose -I [getSRCname]/DOSFOP -I $env(DOSFOP)/tex -E $newName [getOutputFileName] 2>@ stderr }
    }
    exec mv [getOutputFileName] $oldName
    exec mv $newName [getOutputFileName]
    if ($redirect) {
	catch {exec $env(MAKEINFO) --verbose --no-validate --no-warn -I DOSFOP -I $env(DOSFOP)/tex [getOutputFileName] -o [getDOCname]/[getINFOname]  >>& temp}
    } else {
	exec $env(MAKEINFO) --verbose --no-validate --no-warn -I DOSFOP -I $env(DOSFOP)/tex [getOutputFileName] -o [getDOCname]/[getINFOname]  >@ stderr
    }
    exec mv $oldName [getOutputFileName]
#  del DOSFOP/dosfop.switches
}

proc pureTexi2html {redirect} {
    pureTexi2htmlX $redirect "" "" ""
}

proc pureTexi2qhtml {redirect} {
    pureTexi2htmlX $redirect "-quick" "--only" "DOSFOP/only" 
}

proc pureTexi2htmlX {redirect extra1 extra2 extra3} {
  global env
    
    pureSetDosfopSwitches 0 1
    set outputFileName [getOutputFileName]
    set oldName [getOutputFileName].old
    set newName [getOutputFileName].new
    if ($redirect) {
	exec $env(PERL) $env(DOSFOP)/bin/dosfopMacroExpander --verbose -I DOSFOP -I $env(DOSFOP)/tex -E $newName $extra2 $extra3 [getOutputFileName] >& temp
    } else {
	exec $env(PERL) $env(DOSFOP)/bin/dosfopMacroExpander --verbose -I DOSFOP -I $env(DOSFOP)/tex -E $newName $extra2 $extra3 [getOutputFileName] 2>@ stderr
    }
    exec mv [getOutputFileName] $oldName
    exec mv $newName [getOutputFileName]

  if {[string compare [getConfigFlag language] english] == 0} {
      set toc Table\ of\ Contents
  } else {
      set toc Inhaltsverzeichnis 
  }

    cd doc
    if {[string range $outputFileName 0 0] == "/"} {
    } else {
	set outputFileName "../$outputFileName"
    }

  if ($redirect) {
      set retCode [ catch {exec $env(PERL) $env(DOSFOP)/bin/texi2html -toc_name $toc -I ../DOSFOP -I $env(DOSFOP)/tex -split_node -menu -verbose $extra1 $outputFileName  >>& ../temp } ]
  } else {
      set retCode [ catch {exec $env(PERL) $env(DOSFOP)/bin/texi2html -toc_name $toc -I ../DOSFOP -I $env(DOSFOP)/tex -split_node -menu -verbose $extra1 $outputFileName >@stdout 2>@stderr }]
  }
  if {$retCode != 0} {
      puts "DOSFOP: translation to HTML aborted"
      exit $retCode
  }

#  del DOSFOP/dosfop.switches
#  catch {exec sh -c "mv [glob [getSRCname]/[file rootname [file tail [getOutputFileName]]]*.html] [getDOCname]"}
    cd ..
    exec mv $oldName [getOutputFileName]
}

proc pureConstructConfig { } {
    constructConfigFile [pwd]
}

# arrange for binary configuration
# set up files DOSFOP/config and DOSFOP/config.bin such that the config 
# (ascii) file is newer. dosfop will then by itself use the ascii config
# and write a new binary configuration
proc pureConfigBin { } {
    writeFile "DOSFOP/config.bin" ""
    after 1200
    exec touch "DOSFOP/config" 
}

# remove the config.bin file
proc pureConfigAscii { } {
  exec rm "DOSFOP/config.bin"
}

proc pureAddOnly { structlist } {
    foreach s $structlist {
	appendFile "DOSFOP/only" $s
    }
}

proc pureClearOnly { } {
  exec rm "DOSFOP/only" 
}
