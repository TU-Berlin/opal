### global variables of dosfop

# $Id$

uplevel #0 {
# versions of modules
set dosfopVersions "dosfop-global 1.10\n"

# root directory of the project
set dosfopRoot [pwd]

# name of the dosfop translation program
set dosfopTranslationProg $env(DOSFOP)/bin/dosfopTranslator

#name of the tex2dvi program
set dosfop2dvi $env(DOSFOP)/bin/texi2dvi

#name of the makeinfo program
set dosfop2info makeinfo

#name of the makeinfo program
set dosfop2html $env(DOSFOP)/bin/texi2html

#name of the default Library.config
set dosfopLibraryConfig $env(DOSFOP)/defaults/Library.config

#############################################
# regular expressions for basic lexical units
#############################################
# renamed: prefix dosfop- and changed _ to - 

set dosfop_nl "\n"
set dosfop_tab "\t"
set dosfop_white "\[ $dosfop_tab$dosfop_nl\]*"

set dosfop_specialString {[a-zA-Z0-9 \t\n!#$%&*-\./:=\?_`,]*}

set dosfop_levelStringReg      {([a-zA-Z]+)}
set dosfop_subsystemStringReg  {([a-zA-Z0-9 !#$%&*-\./:=\?_`,]+)}
set dosfop_structureStringReg  {([a-zA-Z0-9#$%&*-\./:=\?_`,]+)}
set dosfop_macroStringReg      {(@[a-zA-Z]+)}
 
# hier nochmal genau gucken !!!!!
set dosfop_dirspecials {[a-zA-Z0-9!#$%&*-\./:=\?_`,]}

set dosfop_dir      (($dosfop_dirspecials)+)
set dosfop_dirOrOff (($dosfop_dirspecials)+|off)

##############################################
# regular expressions for lexical alternatives
##############################################

set dosfop_setting0 on|off
set dosfop_setting1 on_medium_verb|on_no_verb|$dosfop_setting0
set dosfop_setting2 on_no_libs|on_no_libs_medium_verb|on_no_libs_no_verb|$dosfop_setting1
set dosfop_setting3 on_low_verb|on_no_libs_low_verb|$dosfop_setting2
set dosfop_setting4 on_no_sub|on_no_struct|$dosfop_setting0
set dosfop_setting5 on_no_libs|$dosfop_setting0
set dosfop_setting6 top_down|bottom_up|alphabetically|user_defined|off


##############################################################
# regular expressions with brackets for indication of matching
##############################################################

set dosfop_onoff           ($dosfop_setting0)
set dosfop_fun_setting     ($dosfop_setting1)
set dosfop_appl_setting    ($dosfop_setting2)
set dosfop_hv_setting      ($dosfop_setting4)
set dosfop_imp_ref_setting ($dosfop_setting5)
set dosfop_uft_setting     ($dosfop_setting3)
set dosfop_lang_setting    (english|german)
set dosfop_lev_setting     (off|(\{$dosfop_specialString\}|,)+)
set dosfop_texinfo_content  {\|([^|]*)\|}
set dosfop_sort_str_setting    ($dosfop_setting6)


###########################################
# regular expressions for setting detection
###########################################

set dosfop_reg(name)       "name$dosfop_white=$dosfop_white\{($dosfop_specialString)\}"
set dosfop_reg(location)   "directory$dosfop_white=$dosfop_white$dosfop_dir"

set dosfop_reg(top_structure) "top_structure$dosfop_white=$dosfop_white\{($dosfop_specialString)\}"
set dosfop_reg(output_file)   "output_file$dosfop_white=$dosfop_white$dosfop_dirOrOff"
set dosfop_reg(project_name)  "project_name$dosfop_white=$dosfop_white\{($dosfop_specialString)\}"
set dosfop_reg(authors)       "authors$dosfop_white=$dosfop_white\{($dosfop_specialString)\}"
set dosfop_reg(date)          "date$dosfop_white=$dosfop_white\{($dosfop_specialString)\}"

set dosfop_reg(functionality_index) "functionality_index$dosfop_white=$dosfop_white$dosfop_fun_setting"
set dosfop_reg(application_index)   "application_index$dosfop_white=$dosfop_white$dosfop_appl_setting"
set dosfop_reg(concept_index)       "concept_index$dosfop_white=$dosfop_white$dosfop_onoff" 
set dosfop_reg(structure_index)       "structure_index$dosfop_white=$dosfop_white$dosfop_onoff" 

set dosfop_reg(subsystems_include) "subsystems_include$dosfop_white=$dosfop_white$dosfop_onoff" 
set dosfop_reg(library_include)    "library_include$dosfop_white=$dosfop_white$dosfop_onoff" 
set dosfop_reg(properties_include) "properties_include$dosfop_white=$dosfop_white$dosfop_onoff" 
set dosfop_reg(only_interfaces)    "only_interfaces$dosfop_white=$dosfop_white$dosfop_onoff" 

set dosfop_reg(import_references)    "import_references$dosfop_white=$dosfop_white$dosfop_imp_ref_setting"
set dosfop_reg(used_function_tables) "used_function_tables$dosfop_white=$dosfop_white$dosfop_uft_setting"
set dosfop_reg(hierarchy_visu)       "hierarchy_visu$dosfop_white=$dosfop_white$dosfop_hv_setting"
set dosfop_reg(sort_structures)      "sort_structures$dosfop_white=$dosfop_white$dosfop_sort_str_setting"
set dosfop_reg(language)             "language$dosfop_white=$dosfop_white$dosfop_lang_setting"
set dosfop_reg(new_pages)            "new_pages$dosfop_white=$dosfop_white$dosfop_onoff"
set dosfop_reg(levels)               "levels$dosfop_white=$dosfop_white$dosfop_lev_setting"
set dosfop_reg(drop_empty_lines)     "drop_empty_lines$dosfop_white=$dosfop_white$dosfop_onoff"
set dosfop_reg(single_node)          "single_node$dosfop_white=$dosfop_white$dosfop_onoff"

set dosfop_reg(survey)               "SURVEY$dosfop_white$dosfop_texinfo_content"

set dosfop_macroreg "$dosfop_macroStringReg$dosfop_white=$dosfop_white$dosfop_texinfo_content"

}

