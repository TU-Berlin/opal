#############################################################################
# global fonts #
################
# $Id$

set headlineFont "-adobe-times-bold-r-*-*-24-*-*-*-*-*-*-*"
set labelFont    "-adobe-times-bold-r-*-*-12-*-*-*-*-*-*-*"
set dataFont     "-adobe-helvetica-medium-r-*-*-*-*-*-*-*-*-*-*"


#############################################################################
# extension of Entry-binding for cr #
#####################################

bind Entry <Any-KeyPress> {
    if {"%A" == "\x0d"} {return}
    if {"%A" != ""} {
        %W insert insert %A
#        tk_entrySeeCaret %W
    }
}


#############################################################################
# button-state modification #
#############################

proc enable buttonList {foreach i $buttonList {$i configure -state normal}}

proc disable buttonList {foreach i $buttonList {$i configure -state disabled}}


##############################################################################
# standard menu for all applications #
######################################

# E : path names for widgets that are help-supported
# A : .mbar as handle for pack commands for complete menu
#     .mbar.file.menu for addition of components to the file menu

proc makeStandardMenu helpSupportedWidgets {

  frame .mbar

  menubutton .mbar.file -text File -menu .mbar.file.menu -underline 0
  menubutton .mbar.help -text Help -menu .mbar.help.menu -underline 0
  
  menu .mbar.file.menu

  menu .mbar.help.menu
  .mbar.help.menu add command -label Global \
             	              -command {globalHelp $helpSupportedWidgets} \
                              -accelerator "  Ctrl+h"
    bind all <Control-h> {globalHelp $helpSupportedWidgets}

  .mbar.help.menu add command -label "Context Sensitive" \
	                      -command {contextSensitiveHelp $helpSupportedWidgets} \
                              -accelerator "  Ctrl+t"
    bind all <Control-t> {contextSensitiveHelp $helpSupportedWidgets}
  
  pack .mbar.file -side left
  pack .mbar.help -side right

  tk_menuBar .mbar .mbar.file .mbar.help
  focus .mbar

  return .mbar.file.menu
}

############################################################################
# State Messages #
##################

proc stateMsgInit {} {
  label .stateMsg  -padx 2m -pady 2m
  pack .stateMsg
  update
}

proc stateMsg msg {
  .stateMsg configure -text $msg
  update
}

proc stateMsgExit {} { 
  pack forget .stateMsg
  update
}



############################################################################
# invoke editor #
#################

proc invokeEditor fileBasename {

  toplevel .choice

  button .choice.sign -text Signature -command "doInvokeEditor $fileBasename.sign"
  button .choice.impl -text Implementation -command "doInvokeEditor $fileBasename.impl"
  button .choice.extp -text "External Properties" -command "doInvokeEditor $fileBasename.extp"
  button .choice.intp -text "Internal Properties" -command "doInvokeEditor $fileBasename.intp"

  pack .choice.sign .choice.impl .choice.extp .choice.intp -side top -padx 2m -pady 2m

}

proc doInvokeEditor filename {

  global env

  destroy .choice

  if {[info exists env(EDITOR)]} then {
    exec $env(EDITOR) $filename &
  } else {
    exec xterm -e vi $filename &
  }
}

############################################################################
# acoustic signal #
###################

proc beep {} {
  puts -nonewline "\a"
}







