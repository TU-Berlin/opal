# $Id$
##############################
# output.tcl
##############################
# needs the following modules:
# -
####################################################
# the output-strings for new default-file generation
####################################################
# the xxx substring is a placeholder for the corresponding setting(...) value

set top_option_ordering {USER_GLOBAL \
                         location \
                         SURVEY \
                         survey \
                         MACROS \
                         macros \
                         OPTIONAL \
                         top_structure output_file project_name authors date \
                         functionality_index application_index concept_index \
			 structure_index \
                         subsystems_include library_include properties_include \
                         only_interfaces \
                         import_references used_function_tables \
                         hierarchy_visu sort_structures language new_pages levels drop_empty_lines single_node}

set subsystem_option_ordering \
                        {name \
                         location \
                         SURVEY \
                         survey \
                         OPTIONAL \
                         properties_include only_interfaces \
                         import_references used_function_tables \
                         hierarchy_visu sort_structures new_pages levels drop_empty_lines}

set structure_option_ordering \
                        {STRUCTURE\
                         name\
                         OPTIONAL\
                         properties_include only_interfaces\
                         import_references used_function_tables\
                         new_pages levels drop_empty_lines}

set structures_option_ordering \
                        {OPTIONAL\
                         properties_include only_interfaces\
                         import_references used_function_tables\
                         new_pages levels drop_empty_lines}

set setting(USER_GLOBAL) "USER_GLOBAL"
# not modifyable
set out(USER_GLOBAL) xxx

set setting(SUBSYSTEM) "SUBSYSTEM"
# not modifyable
set out(SUBSYSTEM) xxx

set setting(STRUCTURE) "STRUCTURE"
# not modifyable
set out(STRUCTURE) xxx

set out(name)       "  name = \{xxx\}"
set out(location)   "  directory = xxx"

set setting(SURVEY) "SURVEY"           
# not modifyable
set out(SURVEY) xxx
set out(survey) "|xxx|"

set setting(MACROS) "MACROS"
set out(MACROS) xxx

set out(macros) xxx

set setting(OPTIONAL) "OPTIONAL"       
# not modifyable
set out(OPTIONAL) xxx

set out(top_structure) "  top_structure = {xxx}"
set out(output_file)   "  output_file   = xxx"
set out(project_name)  "  project_name  = {xxx}"
set out(authors)       "  authors       = {xxx}"
set out(date)          "  date          = {xxx}"

set out(functionality_index) "  functionality_index  = xxx"
set out(application_index)   "  application_index    = xxx"
set out(concept_index)       "  concept_index        = xxx" 
set out(structure_index)     "  structure_index      = xxx"

set out(subsystems_include) "  subsystems_include   = xxx" 
set out(library_include)    "  library_include      = xxx" 
set out(properties_include) "  properties_include   = xxx" 
set out(only_interfaces)    "  only_interfaces      = xxx" 

set out(import_references)    "  import_references    = xxx"
set out(used_function_tables) "  used_function_tables = xxx"
set out(hierarchy_visu)       "  hierarchy_visu       = xxx"
set out(sort_structures)      "  sort_structures      = xxx"
set out(language)             "  language             = xxx"
set out(new_pages)            "  new_pages            = xxx"
set out(levels)               "  levels               = xxx"
set out(drop_empty_lines)     "  drop_empty_lines     = xxx"
set out(single_node)          "  single_node     = xxx"

















