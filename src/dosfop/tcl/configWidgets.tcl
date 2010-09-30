#################################################################################
# simple text-widgets #
#######################
# $Id$

proc nameWidget {introText name} {
  global headlineFont

  frame .name
  label .name_label -text "$introText$name" -font $headlineFont
  pack  .name_label -in .name -side top -pady 2m 
  return ".name" 
}

proc locationWidget {array introText} {
  upvar $array setting

  frame .location -relief flat
  label .location_label -text $introText  
  entry .location_entry -textvariable setting(location) -width 60 
  bind .location_entry <Tab> "completion location"
  pack .location_label .location_entry -in .location -side top -anchor w

  return ".location"
}

proc checkLocationSetting {} {
  global setting
  global dir

  set setting(location) [string trim $setting(location)]

  # check lexical structure of input
  if {[check $setting(location) $dir] != 1} then {
    errorWidget "Syntax error in location name!"
    return 0
  }
  return 1
}


proc outputFileWidget array {
  upvar $array setting

  frame .output_file -relief flat  
  label .output_file_label -text "Intermediate code output file :"
  entry .output_file_entry -textvariable setting(output_file) -width 60 
  bind  .output_file_entry <Tab> "completion output_file"
  pack .output_file_label .output_file_entry -in .output_file -side top -anchor w
  return ".output_file"
}


proc topStructureWidget array {
upvar $array setting
frame .top_structure  -relief flat  
label .top_structure_label -text "Top Structure Name : " 
entry .top_structure_entry -textvariable setting(top_structure) -width 60 -relief raised
pack  .top_structure_label .top_structure_entry -in .top_structure -anchor w -side top 
return ".top_structure"
}

proc projectNameWidget array {
upvar $array setting
frame .project_name -relief flat  
label .project_name_label -text "Name of the Project : " 
entry .project_name_entry -textvariable setting(project_name) -width 60 -relief raised
pack  .project_name_label .project_name_entry -in .project_name -anchor w -side top 
return ".project_name"
}

proc authorsWidget array {
upvar $array setting
frame .authors  -relief flat 
label .authors_label -text "Authors' Names : " 
entry .authors_entry -textvariable setting(authors) -width 60 -relief raised
pack  .authors_label .authors_entry -in .authors -anchor w -side top 
return ".authors"
}

proc dateWidget array {
upvar $array setting
frame .date -relief flat 
label .date_label -text "Date of Creation : " 
entry .date_entry -textvariable setting(date) -width 60 -relief raised
pack  .date_label .date_entry -in .date -anchor w -side top 
return ".date"
}

####################################################################################
# surveys #
###########

proc surveyWidget array {
  upvar $array setting

  frame .survey
  label .survey_label -text "Survey:"
  button .survey_button -text "Configure" -command "displaySurvey $array"

  pack .survey_label -in .survey -side left -padx 2m -pady 2m 
  pack .survey_button -in .survey -side right -padx 2m -pady 2m 

  return ".survey"
}

proc displaySurvey array {
  upvar $array setting

  disable {.survey_button}
 
  toplevel .surveyFrame
  wm title .surveyFrame "Survey Specification"

  frame .surveyFrame.survey_label_frame
  label .surveyFrame.survey_label -text "Survey Editor"

  frame .surveyFrame.survey_text_widget -relief flat
  text  .surveyFrame.survey_text \
                        -yscrollcommand ".surveyFrame.survey_text_scroll set" \
                        -width 80 -height 25
  .surveyFrame.survey_text insert 0.0 $setting(survey)

  scrollbar .surveyFrame.survey_text_scroll -command ".surveyFrame.survey_text yview"

  frame .surveyFrame.buttons
  button .surveyFrame.exitButton -text Exit -command exitSurvey

  pack .surveyFrame.exitButton -in .surveyFrame.buttons -side right -pady 2m -padx 2m
  pack .surveyFrame.survey_text .surveyFrame.survey_text_scroll \
        -in .surveyFrame.survey_text_widget \
        -side left -fill y 
  pack .surveyFrame.survey_label -in .surveyFrame.survey_label_frame -pady 2m -padx 2m
  pack .surveyFrame.survey_label_frame .surveyFrame.survey_text_widget \
       -in .surveyFrame -anchor w -side top -fill x
  pack .surveyFrame.buttons -in .surveyFrame -side top -fill x  
}

proc exitSurvey {} {
  global setting

  set setting(survey) [.surveyFrame.survey_text get 0.0 end]
  destroy .surveyFrame

  enable {.survey_button}
}

###################################################################################
# macros #
##########

proc macroWidget {} {

  frame .macros
  label .macros_label -text "Macros:"
  button .macros_button -text "Configure" -command configMacros

  pack .macros_label -in .macros -side left -padx 2m -pady 2m 
  pack .macros_button -in .macros -side right -padx 2m -pady 2m 

  return ".macros"
}

proc configMacros {} {
  global macro

  disable {.macros_button}

  toplevel .mac 
  wm title .mac "Macro Manipulation"

  label .mac.headline -text "List of defined Macros:" -anchor w
  
  frame .mac.listarea -relief flat
  listbox .mac.list -yscrollcommand ".mac.scroll set" -width 40 -height 10 -selectmode single
  scrollbar .mac.scroll -command ".mac.list yview" 

  updateMacroListbox

  ## buttons
  frame .mac.buttons

  button .mac.delete -text Delete -command macroDelete
  button .mac.edit   -text Edit  -command macroEdit
  ## enable editing with double-click on element
  bind .mac.list <Double-Button-1> macroEdit 
  button .mac.exit   -text Exit -command macroExit

  
  pack .mac.delete .mac.edit -in .mac.buttons -side left -pady 2m -padx 2m
  pack .mac.exit -in .mac.buttons -side right -pady 2m -padx 1m
  pack .mac.headline -side top -fill x -pady 2m -padx 2m
  pack .mac.list -in .mac.listarea -side left
  pack .mac.scroll -in .mac.listarea -side right -fill y
  pack .mac.listarea -side top -fill x
  pack .mac.buttons -side top -fill x
}

proc updateMacroListbox {} {
  global macro

  ## delete old entries
  .mac.list delete 0 end

  if {[info exists macro] != 0} then {
    ## produce newly sorted macro-list 
    foreach element [lsort [array names macro]] {
      .mac.list insert end $element
    }
  }
}

proc macroExit {} {
  destroy .mac

  enable {.macros_button}
}

proc macroDelete {} {
  set selectionList [.mac.list curselection]
  if {[llength $selectionList] == 0} then {
    errorWidget "No Macro Marked for Deletion!"
    return
  }

  set currIndex [lindex $selectionList 0]
  .mac.list delete $currIndex
}

proc macroEdit {} {
  global macro
  global editMacroname

  set selectionList [.mac.list curselection]
  if {[llength $selectionList] == 0} then {
    ## new macro
    set editMacroname ""
    set editMacrosetting ""
  } else {
    ## edit selected macro
    set editMacroname [.mac.list get [lindex $selectionList 0]]
    set editMacrosetting $macro($editMacroname)
  }
  
  frame .mac.editFrame 
  frame .mac.editFrame.name -relief flat
  frame .mac.editFrame.setting -relief flat
 
  label .mac.editFrame.editEntryNameLabel -text "Macroname:" -anchor w
  entry .mac.editFrame.editEntryName    -textvariable editMacroname
  label .mac.editFrame.editEntrySettingLabel -text "Macrosetting:" -anchor w
  
  frame .mac.editFrame.editEntrySettingFrame
  text  .mac.editFrame.editEntrySettingText  -relief raised -height 4 -width 30 \
         -yscrollcommand ".mac.editFrame.editEntrySettingScrollbar set"
  .mac.editFrame.editEntrySettingText insert end $editMacrosetting
  scrollbar  .mac.editFrame.editEntrySettingScrollbar  \
             -command ".mac.editFrame.editEntrySettingText yview"

  pack .mac.editFrame.editEntrySettingScrollbar \
       -in .mac.editFrame.editEntrySettingFrame -side right -fill y
  pack .mac.editFrame.editEntrySettingText \
       -in .mac.editFrame.editEntrySettingFrame -side top -fill x
  

  frame  .mac.editFrame.buttonFrame 
  button .mac.editFrame.apply  -text Apply -command applyEdit
  button .mac.editFrame.cancel -text Cancel -command cancelEdit

  
  pack .mac.editFrame.editEntryNameLabel .mac.editFrame.editEntryName  \
       -in .mac.editFrame.name -side top -anchor w -fill x
  pack .mac.editFrame.editEntrySettingLabel .mac.editFrame.editEntrySettingFrame \
        -in .mac.editFrame.setting  -side top -anchor w -fill x 
  pack .mac.editFrame.name .mac.editFrame.setting -padx 2m -pady 2m -fill x
  pack .mac.editFrame.apply -in .mac.editFrame.buttonFrame -side left -padx 2m -pady 2m
  pack .mac.editFrame.cancel -in .mac.editFrame.buttonFrame -side right -padx 2m -pady 2m
  pack .mac.editFrame -side top -fill x
  pack .mac.editFrame.buttonFrame  -side top -fill x

  pack forget .mac.buttons

  ## enable re-editing via double-click if edit-window is open
  bind .mac.list <Double-Button-1> {
    ## get name of selected macro
    set editMacroname [.mac.list get [lindex [.mac.list curselection] 0]]

    ## delete old text frame contents
    .mac.editFrame.editEntrySettingText delete 0.0 end

    ## insert macro-setting of selected macro in text widget
    .mac.editFrame.editEntrySettingText insert end $macro($editMacroname)
  }

}  

proc applyEdit {} {
  global macro
  global editMacroname
  global macroStringReg

  set editMacrosetting [.mac.editFrame.editEntrySettingText get 0.0 end]

  # check lexical structure of macro
  if {[check $editMacroname $macroStringReg] != 1} then {
    errorWidget "Syntax error in Macro Name!"
    return
  }
  
  set macro($editMacroname) $editMacrosetting
  updateMacroListbox
  cancelEdit
}

proc cancelEdit {} {
  destroy .mac.editFrame
  pack .mac.buttons -side top -fill x
  
  ## re-enable macro-editing via double-click
  bind .mac.list <Double-Button-1> macroEdit 
}

#####################################################################################
# frame for functionality index #
#################################

proc functionalityIndexWidget array {
  upvar $array setting
  global dataFont
  
  frame .functionality_index -relief flat 
  label .functionality_index_label -text "Functionality Index : " -width 23 -anchor w
  label .functionality_index_setting -textvariable setting(functionality_index) \
                                     -width 20 -anchor w -relief sunken -font $dataFont
  menubutton .functionality_index_config  \
	     -text "Configure" -menu .functionality_index_config.menu \
	     -relief raised
  menu .functionality_index_config.menu
  .functionality_index_config.menu add radiobutton \
					 -label "On with full Identifier-Annotation" \
					 -variable setting(functionality_index)  \
					 -value on
  .functionality_index_config.menu add separator
  .functionality_index_config.menu add radiobutton \
					 -label "On with medium Annotation Verbosity" \
					 -variable setting(functionality_index) \
					 -value on_medium_verb 
  .functionality_index_config.menu add radiobutton \
					 -label "On without Identifier-Annotation" \
					 -variable setting(functionality_index) \
					 -value on_no_verb
  .functionality_index_config.menu add separator
  .functionality_index_config.menu add radiobutton \
					 -label "No do not generate Functionality Index" \
					 -variable setting(functionality_index) \
					 -value off
  
  pack .functionality_index_label .functionality_index_setting \
       -in .functionality_index -side left 
  
  pack .functionality_index_config \
       -in .functionality_index -side right 
  return ".functionality_index"
}


#####################################################################################
# frame for application index #
###############################

proc applicationIndexWidget array {
  upvar $array setting
  global dataFont
  
  frame .application_index  -relief flat
  label .application_index_label -text "Application Index : " -width 23 -anchor w
  label .application_index_setting -textvariable setting(application_index) \
                                   -width 20  -anchor w -relief sunken -font $dataFont
  menubutton .application_index_config  \
	     -text "Configure" -menu .application_index_config.menu \
	     -relief raised
  menu .application_index_config.menu
  .application_index_config.menu add radiobutton \
		  -label "On with full Annotation Verbosity"\
		  -variable setting(application_index)  \
		  -value on
  .application_index_config.menu add separator
  .application_index_config.menu add radiobutton \
		  -label "On with medium Annotation Verbosity" \
		  -variable setting(application_index) \
		  -value on_medium_verb 
  .application_index_config.menu add radiobutton \
		  -label "On without Annotation" \
		  -variable setting(application_index) \
		  -value on_no_verb
  .application_index_config.menu add radiobutton \
		  -label "On but no Library-Indexing" \
		  -variable setting(application_index) \
		  -value on_no_libs
  .application_index_config.menu add radiobutton \
		  -label "On with medium Annotation Verbosity but no Library-Indexing" \
		  -variable setting(application_index) \
		  -value on_no_libs_medium_verb
  .application_index_config.menu add radiobutton \
		  -label "On without Annotation and no Library-Indexing" \
		  -variable setting(application_index) \
		  -value on_no_libs_no_verb
  .application_index_config.menu add separator
  .application_index_config.menu add radiobutton \
		  -label "No Application Index" \
		  -variable setting(application_index) \
		  -value off
  
  pack .application_index_label .application_index_setting \
       -in .application_index -side left
  pack .application_index_config \
       -in .application_index -side right
  return ".application_index"
}


#####################################################################################
# frame for concept index: #
############################

proc conceptIndexWidget array {
  upvar $array setting
  global dataFont
  
  frame .concept_index -relief flat
  label .concept_index_label -text "Concept Index : " -width 23 -anchor w
  label .concept_index_setting -textvariable setting(concept_index) \
                               -width 20 -anchor w -relief sunken -font $dataFont
  menubutton .concept_index_config  \
	     -text "Configure" -menu .concept_index_config.menu \
	     -relief raised
  menu .concept_index_config.menu
  .concept_index_config.menu add radiobutton -label On \
					     -variable setting(concept_index)  \
					     -value on
  .concept_index_config.menu add separator 
  .concept_index_config.menu add radiobutton -label "No Concept Index" \
					     -variable setting(concept_index) \
					     -value off
  
  pack .concept_index_label .concept_index_setting \
       -in .concept_index -side left 
  pack .concept_index_config \
       -in .concept_index -side right
  return ".concept_index"
}

#####################################################################################
# frame for structure index: #
############################

proc structureIndexWidget array {
  upvar $array setting
  global dataFont
  
  frame .structure_index -relief flat
  label .structure_index_label -text "Structure Index : " -width 23 -anchor w
  label .structure_index_setting -textvariable setting(structure_index) \
                               -width 20 -anchor w -relief sunken -font $dataFont
  menubutton .structure_index_config  \
	     -text "Configure" -menu .structure_index_config.menu \
	     -relief raised
  menu .structure_index_config.menu
  .structure_index_config.menu add radiobutton -label On \
					     -variable setting(structure_index)  \
					     -value on
  .structure_index_config.menu add separator 
  .structure_index_config.menu add radiobutton -label "No Structure Index" \
					     -variable setting(structure_index) \
					     -value off
  
  pack .structure_index_label .structure_index_setting \
       -in .structure_index -side left 
  pack .structure_index_config \
       -in .structure_index -side right
  return ".structure_index"
}

#####################################################################################
# frame for subsystems include: #
#################################

proc subsystemsIncludeWidget array {
  upvar $array setting 
  global dataFont

  frame .subsystems_include -relief flat
  label .subsystems_include_label -text "Subsystem Inclusion : " -width 23 -anchor w
  label .subsystems_include_setting -textvariable setting(subsystems_include) \
                                    -width 20 -anchor w -relief sunken -font $dataFont
  menubutton .subsystems_include_config  \
             -text "Configure" -menu .subsystems_include_config.menu \
             -relief raised
  menu .subsystems_include_config.menu
  .subsystems_include_config.menu add radiobutton -label On \
                                                  -variable setting(subsystems_include) \
                                                  -value on
  .subsystems_include_config.menu add separator
  .subsystems_include_config.menu add radiobutton -label "No Subsystem Inclusion" \
                                                  -variable setting(subsystems_include) \
                                                  -value off

  pack .subsystems_include_label .subsystems_include_setting \
       -in .subsystems_include -side left 

  pack .subsystems_include_config \
       -in .subsystems_include -side right 
  return ".subsystems_include"
}


#####################################################################################
# frame for library include: #
##############################

proc libraryIncludeWidget array {
  upvar array setting
  global dataFont
  
  frame .library_include -relief flat
  label .library_include_label -text "Library Inclusion : " -width 23 -anchor w
  label .library_include_setting -textvariable setting(library_include) \
                                 -width 20 -anchor w -relief sunken -font $dataFont
  menubutton .library_include_config  \
	     -text "Configure" -menu .library_include_config.menu \
	     -relief raised
  menu .library_include_config.menu
  .library_include_config.menu add radiobutton -label On \
					       -variable setting(library_include)  \
					       -value on
  .library_include_config.menu add separator
  .library_include_config.menu add radiobutton -label "No Library Inclusion" \
					       -variable setting(library_include) \
					       -value off
  
  pack .library_include_label .library_include_setting \
       -in .library_include -side left 
  
  pack .library_include_config -in .library_include -side right 
  return ".library_include"
}


#####################################################################################
# frame for properties include: #
#################################

proc propertiesIncludeWidget {array withContextDef} {
  upvar array setting
  global dataFont
  
  frame .properties_include -relief flat
  label .properties_include_label -text "Property Inclusion : " -width 23 -anchor w
  label .properties_include_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting properties_include w
  uplevel trace variable setting(properties_include) w settingTrace

  menubutton .properties_include_config  \
	     -text "Configure" -menu .properties_include_config.menu \
	     -relief raised
  menu .properties_include_config.menu
  .properties_include_config.menu add radiobutton \
                                            -label On \
					    -variable setting(properties_include)  \
					    -value on
  .properties_include_config.menu add separator
  .properties_include_config.menu add radiobutton \
                                            -label "Do not Include Properties" \
					    -variable setting(properties_include) \
					    -value off
  if {$withContextDef} then {
    .properties_include_config.menu add separator
    .properties_include_config.menu add separator
    .properties_include_config.menu add radiobutton \
                                            -label "context-defined" \
	  				    -variable setting(properties_include) \
					    -value " "
  }
  pack .properties_include_label .properties_include_setting \
       -in .properties_include -side left 
  
  pack .properties_include_config \
       -in .properties_include -side right
  return ".properties_include"
}

##################################################################################
# frame for only interfaces: #
##############################

proc onlyInterfacesWidget {array withContextDef} {
  upvar array setting
  global dataFont
  
  frame .only_interfaces -relief flat
  label .only_interfaces_label -text "Include only Interfaces : " -width 23 -anchor w
  label .only_interfaces_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting only_interfaces w
  uplevel trace variable setting(only_interfaces) w settingTrace

  menubutton .only_interfaces_config  \
	     -text "Configure" -menu .only_interfaces_config.menu \
	     -relief raised
  menu .only_interfaces_config.menu
  .only_interfaces_config.menu add radiobutton \
                                            -label On \
					    -variable setting(only_interfaces)  \
					    -value on
  .only_interfaces_config.menu add separator
  .only_interfaces_config.menu add radiobutton \
                                            -label "Include all Structures" \
					    -variable setting(only_interfaces) \
					    -value off
  if {$withContextDef} then {
    .only_interfaces_config.menu add separator
    .only_interfaces_config.menu add separator
    .only_interfaces_config.menu add radiobutton \
					    -label "context-defined" \
					    -variable setting(only_interfaces) \
					    -value " "
  }
  pack .only_interfaces_label .only_interfaces_setting \
       -in .only_interfaces -side left 
  
  pack .only_interfaces_config \
       -in .only_interfaces -side right 
  return ".only_interfaces"
}


#####################################################################################
# frame for hierarchy visu: #
#############################

proc hierarchyVisuWidget {array withContextDef} {
  upvar $array setting
  global dataFont

  frame .hierarchy_visu -relief flat
  label .hierarchy_visu_label -text "Hierarchy Visualization : " -width 23 -anchor w
  label .hierarchy_visu_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting hierarchy_visu w
  uplevel trace variable setting(hierarchy_visu) w settingTrace

  menubutton .hierarchy_visu_config  \
	     -text "Configure" -menu .hierarchy_visu_config.menu \
	     -relief raised
  menu .hierarchy_visu_config.menu
  .hierarchy_visu_config.menu add radiobutton -label On \
					    -variable setting(hierarchy_visu)  \
					    -value on
  .hierarchy_visu_config.menu add separator
  .hierarchy_visu_config.menu add radiobutton -label "On, but without Subsystems" \
					    -variable setting(hierarchy_visu)  \
					    -value on_no_sub
  .hierarchy_visu_config.menu add radiobutton -label "On, but without Structures"\
					    -variable setting(hierarchy_visu)  \
					    -value on_no_struct
  .hierarchy_visu_config.menu add separator
  .hierarchy_visu_config.menu add radiobutton -label "No Hierarchy Visualization" \
					    -variable setting(hierarchy_visu) \
					    -value off
  if {$withContextDef} then {
    .hierarchy_visu_config.menu add separator
    .hierarchy_visu_config.menu add separator
    .hierarchy_visu_config.menu add radiobutton -label "context-defined" \
	  				    -variable setting(hierarchy_visu) \
		  			    -value " "
  }
  
  pack .hierarchy_visu_label .hierarchy_visu_setting \
       -in .hierarchy_visu -side left 
  
  pack .hierarchy_visu_config \
       -in .hierarchy_visu -side right
  return ".hierarchy_visu"
}


#####################################################################################
# frame for sort structures visu: #
#############################

proc sortStructuresWidget {array withContextDef} {
  upvar $array setting
  global dataFont

  frame .sort_structures -relief flat
  label .sort_structures_label -text "Sort Structures : " -width 23 -anchor w
  label .sort_structures_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting sort_structures w
  uplevel trace variable setting(sort_structures) w settingTrace

  menubutton .sort_structures_config  \
	     -text "Configure" -menu .sort_structures_config.menu \
	     -relief raised
  menu .sort_structures_config.menu
  .sort_structures_config.menu add radiobutton -label "top-down" \
					 -variable setting(sort_structures)  \
					 -value top_down
  .sort_structures_config.menu add radiobutton \
	                          -label "bottom-up" \
				  -variable setting(sort_structures)  \
				  -value bottom_up
  .sort_structures_config.menu add separator
  .sort_structures_config.menu add radiobutton -label "alphabetically"\
					-variable setting(sort_structures)  \
					-value alphabetically
  .sort_structures_config.menu add radiobutton -label "user defined" \
	                                -variable setting(sort_structures) \
					-value user_defined \
					-state disabled
  .sort_structures_config.menu add separator
  .sort_structures_config.menu add radiobutton \
	                  -label "no sorting of structures" \
			  -variable setting(sort_structures) \
					    -value off
  if {$withContextDef} then {
    .sort_structures_config.menu add separator
    .sort_structures_config.menu add separator
    .sort_structures_config.menu add radiobutton -label "context-defined" \
  				    -variable setting(sort_structures) \
	  			    -value " "
  }
  
  pack .sort_structures_label .sort_structures_setting \
       -in .sort_structures -side left
  
  pack .sort_structures_config \
       -in .sort_structures -side right
  return ".sort_structures"
}


#####################################################################################
# frame for import references: #
################################

proc importReferencesWidget {array withContextDef} {
  upvar $array setting
  global dataFont

  frame .import_references -relief flat
  label .import_references_label -text "Import Referencing : " -width 23 -anchor w
  label .import_references_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting import_references w
  uplevel trace variable setting(import_references) w settingTrace

  menubutton .import_references_config  \
	     -text "Configure" -menu .import_references_config.menu \
	     -relief raised
  menu .import_references_config.menu
  .import_references_config.menu add radiobutton \
                                        -label On \
					-variable setting(import_references)  \
					-value on
  .import_references_config.menu add separator
  .import_references_config.menu add radiobutton \
                                        -label "On, but Libraries are not considered" \
					-variable setting(import_references)  \
					-value on_no_libs
  .import_references_config.menu add separator
  .import_references_config.menu add radiobutton \
                                        -label "No Import Reference Generation" \
					-variable setting(import_references) \
					-value off
  if {$withContextDef} then {
    .import_references_config.menu add separator
    .import_references_config.menu add separator
    .import_references_config.menu add radiobutton \
                                        -label "context-defined" \
					-variable setting(import_references) \
					-value " "
  }
  
  pack .import_references_label .import_references_setting \
       -in .import_references -side left
  
  pack .import_references_config \
       -in .import_references -side right
  return ".import_references"
}


#####################################################################################
# frame for used function tables: #
###################################

proc usedFunctionTablesWidget {array withContextDef} {
  upvar $array setting
  global dataFont

  frame .used_function_tables -relief flat
  label .used_function_tables_label -text "Used Function Tables : " -width 23 -anchor w
  label .used_function_tables_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting used_function_tables w
  uplevel trace variable setting(used_function_tables) w settingTrace

  menubutton .used_function_tables_config  \
	     -text "Configure" -menu .used_function_tables_config.menu \
	     -relief raised
  menu .used_function_tables_config.menu
  .used_function_tables_config.menu add radiobutton \
                   -label "On with full Instantiation Verbosity"\
		   -variable setting(used_function_tables)  \
		   -value on
  .used_function_tables_config.menu add separator 
  .used_function_tables_config.menu add radiobutton \
                   -label "On with medium Instantiation Verbosity" \
		   -variable setting(used_function_tables)  \
		   -value on_medium_verb
  .used_function_tables_config.menu add radiobutton \
                   -label "On with low Instantiation Verbosity" \
		   -variable setting(used_function_tables)  \
		   -value on_low_verb
  .used_function_tables_config.menu add radiobutton \
                   -label "On with no Instantiation"\
		   -variable setting(used_function_tables)  \
		   -value on_no_verb
  .used_function_tables_config.menu add separator 
  .used_function_tables_config.menu add radiobutton \
                   -label "On with full Instantiation Verbosity, but without Libraries" \
		   -variable setting(used_function_tables)  \
		   -value on_no_libs
  .used_function_tables_config.menu add radiobutton \
                   -label "On with medium Instantiation Verbosity, but without Libraries" \
		   -variable setting(used_function_tables)  \
		   -value on_no_libs_medium_verb
  .used_function_tables_config.menu add radiobutton \
                   -label "On with low Instantiation Verbosity, but without Libraries" \
		   -variable setting(used_function_tables)  \
		   -value on_no_libs_low_verb
  .used_function_tables_config.menu add radiobutton \
                   -label "On with no Instantiation and without Libraries" \
		   -variable setting(used_function_tables)  \
		   -value on_no_libs_no_verb
  .used_function_tables_config.menu add separator 
  .used_function_tables_config.menu add radiobutton \
                   -label "No Used Function Table Generation" \
		   -variable setting(used_function_tables) \
		   -value off
  if {$withContextDef} then {
    .used_function_tables_config.menu add separator 
    .used_function_tables_config.menu add separator 
    .used_function_tables_config.menu add radiobutton \
                   -label "context-defined" \
		   -variable setting(used_function_tables) \
		   -value " "
  }
  
  pack .used_function_tables_label .used_function_tables_setting \
       -in .used_function_tables -side left 
  
  pack .used_function_tables_config \
       -in .used_function_tables -side right 
  return ".used_function_tables"
}


###################################################################################
# frame for language: #
#######################

proc languageWidget array {
  upvar $array setting
  global dataFont
  
  frame .language -relief flat
  label .language_label -text "Basic Language : " -width 23 -anchor w
  label .language_setting -textvariable setting(language) \
                          -width 20 -anchor w -relief sunken -font $dataFont
  menubutton .language_config  \
	     -text "Configure" -menu .language_config.menu \
	     -relief raised
  menu .language_config.menu
  .language_config.menu add radiobutton -label English \
					-variable setting(language)  \
					-value english
  .language_config.menu add radiobutton -label German \
					-variable setting(language) \
					-value german
  
  pack .language_label .language_setting \
       -in .language -side left 
  
  pack .language_config \
       -in .language -side right 
  return ".language"
}


#################################################################################
# frame for new pages: #
########################

proc newPagesWidget {array withContextDef} {
  upvar $array setting
  global dataFont
  
  frame .new_pages -relief flat
  label .new_pages_label -text "Start New Pages : " -width 23 -anchor w
  label .new_pages_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting new_pages w
  uplevel trace variable setting(new_pages) w settingTrace

  menubutton .new_pages_config  \
	     -text "Configure" -menu .new_pages_config.menu \
	     -relief raised
  menu .new_pages_config.menu
  .new_pages_config.menu add radiobutton -label "Start New Page where Appropriate" \
					 -variable setting(new_pages)  \
					 -value on
  .new_pages_config.menu add separator
  .new_pages_config.menu add radiobutton -label "Do not insert New Pages" \
					 -variable setting(new_pages) \
					 -value off
  if {$withContextDef} then {
    .new_pages_config.menu add separator
    .new_pages_config.menu add separator
    .new_pages_config.menu add radiobutton -label "context-defined" \
					   -variable setting(new_pages) \
					   -value " "
  }
  
  pack .new_pages_label .new_pages_setting \
       -in .new_pages -side left 
  
  pack .new_pages_config \
       -in .new_pages -side right 
  return ".new_pages"
}


#####################################################################################
# frame for drop empty lines: #
###############################

proc dropEmptyLinesWidget {array withContextDef} {
  upvar $array setting
  global dataFont

  frame .drop_empty_lines -relief flat
  label .drop_empty_lines_label -text "Drop Empty Lines : " -width 23 -anchor w
  label .drop_empty_lines_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting drop_empty_lines w
  uplevel trace variable setting(drop_empty_lines) w settingTrace

  menubutton .drop_empty_lines_config  \
	     -text "Configure" -menu .drop_empty_lines_config.menu \
	     -relief raised
  menu .drop_empty_lines_config.menu
  .drop_empty_lines_config.menu add radiobutton -label "Eliminate useless Layout" \
					        -variable setting(drop_empty_lines)  \
					        -value on
  .drop_empty_lines_config.menu add separator
  .drop_empty_lines_config.menu add radiobutton -label "Keep Layout" \
					        -variable setting(drop_empty_lines) \
					        -value off
  if {$withContextDef} then {
    .drop_empty_lines_config.menu add separator
    .drop_empty_lines_config.menu add separator
    .drop_empty_lines_config.menu add radiobutton \
                                                -label "context-defined" \
					        -variable setting(drop_empty_lines) \
					        -value " "
  }
  
  pack .drop_empty_lines_label .drop_empty_lines_setting \
       -in .drop_empty_lines -side left 
  
  pack .drop_empty_lines_config \
       -in .drop_empty_lines -side right
  return ".drop_empty_lines"
}

##############################################################################
# frame for single node: #
###############################

proc singleNodeWidget {array withContextDef} {
  upvar $array setting
  global dataFont

  frame .single_node -relief flat
  label .single_node_label -text "Structure in single node: " -width 23 -anchor w
  label .single_node_setting -width 20 -anchor w -relief sunken -font $dataFont

  settingTrace setting single_node w
  uplevel trace variable setting(single_node) w settingTrace

  menubutton .single_node_config  \
	     -text "Configure" -menu .single_node_config.menu \
	     -relief raised
  menu .single_node_config.menu
  .single_node_config.menu add radiobutton -label "On" \
					        -variable setting(single_node)  \
					        -value on
  .single_node_config.menu add separator
  .single_node_config.menu add radiobutton -label "Off" \
					        -variable setting(single_node) \
					        -value off
  if {$withContextDef} then {
    .single_node_config.menu add separator
    .single_node_config.menu add separator
    .single_node_config.menu add radiobutton \
                                                -label "context-defined" \
					        -variable setting(single_node) \
					        -value " "
  }
  
  pack .single_node_label .single_node_setting \
       -in .single_node -side left
  
  pack .single_node_config \
       -in .single_node -side right 
  return ".single_node"
}


#####################################################################################
# frame for levels: #
#####################

proc levelsWidget {array withContextDef} {
  upvar $array setting
  
  frame .levels 
  label .levels_label -text "Level Mechanism : " -width 20 -anchor w
  menubutton .levels_config  \
	     -text "Configure" -menu .levels_config.menu \
	     -relief raised
  menu .levels_config.menu
  .levels_config.menu add command -label "Modify Levels" \
				  -command "modifyLevels $withContextDef"
   
  .levels_config.menu add separator
  .levels_config.menu add radiobutton -label "No Level Handling" \
					    -variable setting(levels) \
					    -value off
  if {$withContextDef} then {
    .levels_config.menu add separator
    .levels_config.menu add separator
    .levels_config.menu add radiobutton -label "context-defined" \
					-variable setting(levels) \
					-value " "
  }
  pack .levels_label \
       -in .levels -side left -padx 2m -pady 2m 
  
  pack .levels_config \
       -in .levels -side right -padx 2m -pady 2m
  return ".levels"
}

proc modifyLevels withContextDef {
  global addedLevel

  disable {.levels_config}

  toplevel .modify_levels
  wm title .modify_levels "Level Modification"

  frame .modify_levels.displayFrame -relief flat

  # display frame for local levels
  #
  frame .modify_levels.displayFrameLocal
  if $withContextDef then { 
    label .modify_levels.localLabel -text "Locally defined:" -anchor w
    pack .modify_levels.localLabel \
         -in .modify_levels.displayFrameLocal -side top -fill x -pady 2m -padx 2m
  }
  listbox .modify_levels.levellistLocal \
          -yscrollcommand ".modify_levels.levelscrollLocal set"
  scrollbar .modify_levels.levelscrollLocal \
          -command ".modify_levels.levellistLocal yview"
    
  pack .modify_levels.levellistLocal .modify_levels.levelscrollLocal \
       -in .modify_levels.displayFrameLocal -side left -fill y

  if $withContextDef then {
    # diplay frame for context-defined labels
    # not displayed immediately!
    #
    frame .modify_levels.displayFrameContext 
    label .modify_levels.contextLabel -text "Context defined:" -anchor w
    listbox .modify_levels.levellistContext \
	    -yscrollcommand ".modify_levels.levelscrollContext set" \
            -relief flat -bg azure3
    scrollbar .modify_levels.levelscrollContext \
	      -command ".modify_levels.levellistContext yview"
      
    pack .modify_levels.contextLabel \
	 -in .modify_levels.displayFrameContext -side top -fill x -pady 2m -padx 2m
    pack .modify_levels.levellistContext .modify_levels.levelscrollContext \
	 -in .modify_levels.displayFrameContext -side left -fill y
  }

  frame .modify_levels.buttons
  button .modify_levels.exit -text Exit -command "exitModifyLevels $withContextDef"
  button .modify_levels.delete -text Delete -command deleteLevel
  button .modify_levels.add  -text Add  -command addLevel

  pack .modify_levels.add  .modify_levels.delete \
       -in .modify_levels.buttons -side left -padx 2m -pady 2m
  pack .modify_levels.exit -in .modify_levels.buttons -side right -padx 2m -pady 2m

  pack .modify_levels.buttons -side bottom -fill x -padx 2m -pady 2m
  pack .modify_levels.displayFrameLocal \
       -in  .modify_levels.displayFrame -side bottom -padx 2m -pady 2m
  pack .modify_levels.displayFrame -side top -fill x 

  makeLevellists $withContextDef
}

proc deleteLevel {} {

  set selectionList [.modify_levels.levellistLocal curselection]
  if {[llength $selectionList] == 0} then {
    errorWidget "No Level Marked for Deletion!"
    return
  }

  set currIndex [lindex $selectionList 0]
  .modify_levels.levellistLocal delete $currIndex
}
  
proc addLevel {} {
  global addedLevel

  pack forget .modify_levels.buttons 

  frame .modify_levels.enter
  label .modify_levels.label -text "New Label Name : "
  entry .modify_levels.entry -textvariable addedLevel

  pack .modify_levels.label .modify_levels.entry -in .modify_levels.enter \
       -side top -anchor w
  pack .modify_levels.enter -side top -fill x

  frame .modify_levels.addButtons
  button .modify_levels.exitAdd -text "Exit Add" -command exitAddLevel
  button .modify_levels.doAdd   -text "Do Add"  -command doAddLevel

  pack .modify_levels.doAdd -in .modify_levels.addButtons -side left -padx 2m -pady 2m
  pack .modify_levels.exitAdd -in .modify_levels.addButtons -side right -padx 2m -pady 2m

  pack .modify_levels.addButtons -side top -fill x
}

proc doAddLevel {} {
  global setting
  global addedLevel
  global levelStringReg

  # check lexical structure of level
  if {[check $addedLevel $levelStringReg] != 1} then {
    errorWidget "Syntax error in label name!"
    return
  }

  if {[string compare $setting(levels) off]} then {
    regsub -all "\{|\}" $setting(levels) "" newStr
    set newStr "\{$newStr,$addedLevel\}"        
    set setting(levels) $newStr
  } else {
    set setting(levels) "\{$addedLevel\}"
  }
  .modify_levels.levellistLocal insert end $addedLevel
}

proc exitAddLevel {} {
  destroy .modify_levels.enter
  destroy .modify_levels.addButtons

  pack .modify_levels.buttons -side bottom -fill x

}


proc exitModifyLevels withContextDef {

  makeSetting $withContextDef

  destroy .modify_levels

  enable {.levels_config}
}

proc makeLevellists withContextDef {
  global setting

  # make listbox entries for present local levels
  #
  if {[expr ([string compare $setting(levels) off] !=0) && \
            ([string compare $setting(levels) " "] !=0)]} {
      regsub -all "\{|\}" $setting(levels) "" newStr
      foreach elem [lsort [split $newStr "," ]] {
        .modify_levels.levellistLocal insert end $elem
    }
  }

  if $withContextDef then {  
    # make listbox entries for context levels
    #
    if {[string compare $setting(levels) " "] == 0} {
	regsub -all "\{|\}" $setting(levels,context) "" newStr
	foreach elem [lsort [split $newStr "," ]] {
	  .modify_levels.levellistContext insert end $elem
      }
    pack .modify_levels.displayFrameContext \
	 -in .modify_levels.displayFrame -side top -padx 2m -pady 2m
    }
}
}


proc makeSetting withContextDef {
  global setting

  set listSize [.modify_levels.levellistLocal size]

  if {$listSize == 0} then {
    if $withContextDef then {
      set setting(levels) " " 
      # make undefined
    } else {
      # at top-level there has to be at least one level defined or the 
      # level-handling has to be off
      set setting(levels) off
    }
  } else {

    set temp ""

    for {set i 0} {$i < $listSize} {incr i} {
      if {$i == ([.modify_levels.levellistLocal size] - 1) } then {    
        set temp "$temp\{[.modify_levels.levellistLocal get $i]\}"
      } else {
        set temp "$temp\{[.modify_levels.levellistLocal get $i]\},"
      }
    }
    set setting(levels) $temp
  }

}



proc settingTrace {name element op} {
  global setting

  set actSetting [set ${name}($element)]

  if {[string compare $actSetting " "] == 0} then {
      .${element}_setting configure -text [set ${name}($element,context)] \
                                    -bg azure3
  } else {
      .${element}_setting configure -text $actSetting -bg lightSteelBlue2
  }

}







