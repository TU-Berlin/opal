# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl/regexprs.tcl,v 1.1.1.1 1998-06-16 16:00:31 wg Exp $
##############################
# regexprs.tcl
##############################
# needs the following modules: 
# -
#
#############################################
# regular expressions for basic lexical units
#############################################

set nl "\n"
set tab "\t"
set white "\[ $tab$nl\]*"

set specialString {[a-zA-Z0-9 \t\n!#$%&*-\./:=\?_`,]*}

set levelStringReg      {([a-zA-Z]+)}
set subsystemStringReg  {([a-zA-Z0-9 !#$%&*-\./:=\?_`,]+)}
set structureStringReg  {([a-zA-Z0-9#$%&*-\./:=\?_`,]+)}
set macroStringReg      {(@[a-zA-Z]+)}
 
# hier nochmal genau gucken !!!!!
set dirspecials {[a-zA-Z0-9!#$%&*-\./:=\?_`,]}

set dir      (($dirspecials)+)
set dirOrOff (($dirspecials)+|off)

##############################################
# regular expressions for lexical alternatives
##############################################

set setting0 on|off
set setting1 on_medium_verb|on_no_verb|$setting0
set setting2 on_no_libs|on_no_libs_medium_verb|on_no_libs_no_verb|$setting1
set setting3 on_low_verb|on_no_libs_low_verb|$setting2
set setting4 on_no_sub|on_no_struct|$setting0
set setting5 on_no_libs|$setting0
set setting6 top_down|bottom_up|alphabetically|user_defined|off


##############################################################
# regular expressions with brackets for indication of matching
##############################################################

set onoff           ($setting0)
set fun_setting     ($setting1)
set appl_setting    ($setting2)
set hv_setting      ($setting4)
set imp_ref_setting ($setting5)
set uft_setting     ($setting3)
set lang_setting    (english|german)
set lev_setting     (off|(\{$specialString\}|,)+)
set texinfo_content  {\|([^|]*)\|}
set sort_str_setting    ($setting6)


###########################################
# regular expressions for setting detection
###########################################

set reg(name)       "name$white=$white\{($specialString)\}"
set reg(location)   "directory$white=$white$dir"

set reg(top_structure) "top_structure$white=$white\{($specialString)\}"
set reg(output_file)   "output_file$white=$white$dirOrOff"
set reg(project_name)  "project_name$white=$white\{($specialString)\}"
set reg(authors)       "authors$white=$white\{($specialString)\}"
set reg(date)          "date$white=$white\{($specialString)\}"

set reg(functionality_index) "functionality_index$white=$white$fun_setting"
set reg(application_index)   "application_index$white=$white$appl_setting"
set reg(concept_index)       "concept_index$white=$white$onoff" 
set reg(structure_index)       "structure_index$white=$white$onoff" 

set reg(subsystems_include) "subsystems_include$white=$white$onoff" 
set reg(library_include)    "library_include$white=$white$onoff" 
set reg(properties_include) "properties_include$white=$white$onoff" 
set reg(only_interfaces)    "only_interfaces$white=$white$onoff" 

set reg(import_references)    "import_references$white=$white$imp_ref_setting"
set reg(used_function_tables) "used_function_tables$white=$white$uft_setting"
set reg(hierarchy_visu)       "hierarchy_visu$white=$white$hv_setting"
set reg(sort_structures)      "sort_structures$white=$white$sort_str_setting"
set reg(language)             "language$white=$white$lang_setting"
set reg(new_pages)            "new_pages$white=$white$onoff"
set reg(levels)               "levels$white=$white$lev_setting"
set reg(drop_empty_lines)     "drop_empty_lines$white=$white$onoff"
set reg(single_node)          "single_node$white=$white$onoff"

set reg(survey)               "SURVEY$white$texinfo_content"

set macroreg "$macroStringReg$white=$white$texinfo_content"


####################################################################################
# given a regular expression and a string getMatch returns the matching part for the
# regular expression enclosed in brackets
####################################################################################

proc getMatch {actRegexp str} {
  set match " "
  regexp $actRegexp $str _ match
  return $match
}

## return default Off, if not found
proc getMatchD {actRegexp str} {
  set match " "
    if [regexp $actRegexp $str _ match] then { 
	return $match 
    } else { 
	return "off" 
    } 
}


proc check {toCheck regularExpr} {
  set matched [regexp $regularExpr $toCheck matchvar]
  if $matched then {
    return [expr [string compare $toCheck $matchvar] == 0]
  } else {
    return 0
  }
}














