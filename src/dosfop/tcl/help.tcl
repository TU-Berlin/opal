# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl/help.tcl,v 1.1.1.1 1998-06-16 16:00:31 wg Exp $
############################
# creating the help window #
############################

proc helpWindow helpSupportedWidgets {

  if {[winfo exists .help]} then {
    .help.text delete 0.0 end
  } else {
    toplevel .help
  
    frame .help.textFrame 
    text .help.text -width 60 -height 20 -yscrollcommand ".help.scrollbar set"
    scrollbar .help.scrollbar -command ".help.text yview"
  
    frame .help.buttonFrame
    button .help.iconify -text Iconify -command "wm iconify .help"
    button .help.dismiss -text Dismiss -command "dismissHelp \{$helpSupportedWidgets\}"
  
    pack .help.dismiss -in .help.buttonFrame -side right -padx 2m -pady 2m
    pack .help.iconify -in .help.buttonFrame -side left -padx 2m -pady 2m
    pack .help.text .help.scrollbar -in .help.textFrame -side left -fill y
  
    pack .help.textFrame .help.buttonFrame -side top -fill x
  }
}

proc dismissHelp widgets {

  deleteBinding $widgets

  destroy .help
}

###################
# initiating help #
###################
# these procedures are to be called by applications that want to provide
# a help facility

proc globalHelp widgetList {
  global global

  # in case global help is requested after context-sensitive help:
  deleteBinding $widgetList

  helpWindow $widgetList

  wm title .help "Global Help Window for [wm title .]"

 .help.text insert end $global([winfo name .])

  adjustHelpWindowSize [llength [split $global([winfo name .]) \n]]1
}


proc contextSensitiveHelp widgetList {

  helpWindow $widgetList

  makeBinding $widgetList

  wm title .help "Context Sensitive Help Window for [wm title .]"

  .help.text insert end {Move mouse-cursor over item to get help!}

  adjustHelpWindowSize 1
}

##############################################
# handle bindings for help supported widgets #
##############################################

proc makeBinding widgetList {
  global context

  foreach elem $widgetList {

    if {[winfo exists $elem] == 0} continue

    set class [winfo class $elem]

#        tk_butEnter %W 

    if {$class == "Button"} then {
      bind $elem <Any-Enter> {
	if {[winfo exists .help.text]} then {
          .help.text delete 0.0 end 
	  .help.text insert end $context(%W) 
        } 
        adjustHelpWindowSize [llength [split $context(%W) \n]]
      }
    }

    if {$class == "Menubutton"} then {
      bind $elem <Any-Enter> {
        set tk_priv(inMenuButton) %W
        if {[lindex [%W config -state] 4] != "disabled"} {
          if {!$tk_strictMotif} {
            %W config -state active
          }
        }
        if {[winfo exists .help.text]} then {
          .help.text delete 0.0 end 
	  .help.text insert end $context(%W) 
        }
        adjustHelpWindowSize [llength [split $context(%W) \n]]
      } 
    }

    if {($class == "Entry") || ($class == "Label") || \
        ($class == "Listbox") || ($class == "Text")} then {
      bind $elem <Any-Enter> {
        if {[winfo exists .help.text]} then {
          .help.text delete 0.0 end 
	  .help.text insert end $context(%W) 
        }
        adjustHelpWindowSize [llength [split $context(%W) \n]]
      }
    }
      
  }
}

proc adjustHelpWindowSize height {

  set maxheight 20

  # define upper bound of height
  if {$height > $maxheight} then {set height $maxheight}

  .help.text configure -height $height
}


proc deleteBinding widgetList {
  foreach elem $widgetList {

    if {[winfo exists $elem] == 0} continue

    set class [winfo class $elem]

# tk_butEnter %W
    if { $class == "Button" } then {
        bind $elem <Any-Enter> { } 
    }

#        set tk_priv(inMenuButton) %W
    if { $class == "Menubutton" } then {
      bind $elem <Any-Enter> {
        if {[lindex [%W config -state] 4] != "disabled"} {
          if {!$tk_strictMotif} {
            %W config -state active
          }
        }
      } 
  }

    if {($class == "Entry") || ($class == "Label") || \
        ($class == "Listbox") || ($class == "Text")} then {
      bind $elem <Any-Enter> { }
    }
  }
}

##############
# help texts #
##############

set bindingsInEntries \
{
The following key- and mouse bindings are valid in this entry
widget:
(1)    Clicking mouse button 1 in an entry  positions  the 
       insertion  cursor  just before the character under- 
       neath the mouse cursor and sets the input focus  to 
       this widget.                                        

(2)    Dragging with mouse button 1 strokes out  a  selec- 
       tion between the insertion cursor and the character 
       under the mouse.                                    

(3)    The  ends of the selection can be adjusted by drag- 
       ging with mouse button 1 while  the  shift  key  is 
       down;   this  will  adjust the end of the selection 
       that was nearest to the mouse cursor when button  1 
       was pressed.                                        

(4)    The view in the entry can be adjusted  by  dragging 
       with mouse button 2.                                

(5)    If the input focus is in an entry widget and  char- 
       acters  are  typed  on the keyboard, the characters 
       are inserted just before the insertion cursor.      

(6)    Control-h  and  the Backspace and Delete keys erase 
       the character just before the insertion cursor.     

(7)    Control-w erases the word just before the insertion 
       cursor.                                             
(8)    Control-u clears the entry to an empty string.      

(9)    Control-v inserts the current selection just before 
       the insertion cursor.                               

(10)   Control-d  deletes  the  selected  characters;   an 
       error  occurs  if the selection is not in this wid- 
       get.                                                
}

set bindingsInTexts \
{
The following key- and mouse bindings are valid in this text
widget:

(1)    Pressing mouse button 1 in an  text  positions  the
       insertion  cursor  just before the character under-
       neath the mouse cursor and sets the input focus  to
       this widget.

(2)    Dragging  with  mouse button 1 strokes out a selec-
       tion between the insertion cursor and the character
       under the mouse.

(3)    If  you  double-press  mouse button 1 then the word
       under the mouse cursor will be selected, the inser-
       tion  cursor will be positioned at the beginning of
       the word, and dragging the mouse will stroke out  a
       selection whole words at a time.

(4)    If  you  triple-press  mouse button 1 then the line
       under the mouse cursor will be selected, the inser-
       tion  cursor will be positioned at the beginning of
       the line, and dragging the mouse will stroke out  a
       selection whole line at a time.

(5)    The  ends of the selection can be adjusted by drag-
       ging with mouse button 1 while  the  shift  key  is
       down;   this  will  adjust the end of the selection
       that was nearest to the mouse cursor when button  1
       was  pressed.  If the selection was made in word or
       line mode then it will be  adjusted  in  this  same
       mode.

(6)    The  view  in  the text can be adjusted by dragging
       with mouse button 2.

(7)    If the input focus is in a text widget and  charac-
       ters  are typed on the keyboard, the characters are
       inserted just before the insertion cursor.

(8)    Control+h and the Backspace and Delete  keys  erase
       the character just before the insertion cursor.

(9)    Control+v inserts the current selection just before
       the insertion cursor.

(10)   Control+d  deletes  the  selected  characters;   an
       error  occurs  if the selection is not in this wid-
       get."
}

set admissableTexinfoCommands \
{
Environments:
=============
@asis
@enumerate
@flushleft
@flushright
@group
@ifinfo
@iftex
@ignore
@item
@itemize
@itemx
@quotation
@table

Special Symbols:
================
@@
@{
@}
@bullet{}
@copyright{}
@dots{}
@minus
@sp <n>
@TeX{}
@today{}

Font Modification:
==================
@b{...}
@code{...}
@emph{...}
@footnote{...}
@i{...}
@r{...}
@sc{...}
@t{...}
@w{...}

Formatting:
===========
@*
@.
@:
@center{...}
@need <n>
@noindent
@page

Miscellaneous:
==============
@cindex{...}
}
# for DOSFOP main window:
#------------------------
 
set global(dosfop) \
{DOSFOP is a toolkit for the delevelopment and
administration of documentation for large OPAL projects. It 
provides functionalities in mainly three areas:
(1) Configuration of documentation elements
(2) Translation of OPAL-project-documentations 
(3) Examining the results of the translation process

Initially the user of the DOSFOP-System has to denote the name
of the top-level structure of the project to be documented. 
This structure is not necessarily the top-structure needed 
for the transalation process of the OPAL Compilation System 
(OCS). The given top-structure name just serves as
a reference point for calculation of the direct and 
transitive import relations. The complete project is gathered 
by accumulating all structures that are imported directly or 
indirectly by the top-structure.

The name of the top-structure has to be specified in the 
"Global Configuration" tool that can be invoked via the 
corresponding button in the DOSFOP window. The extent of 
inclusion of structures in the finally produced documentation
can be determined in the "Global Configuration" tool, either.

The "Subsystem Configuration" enables the user to handle
subsystems in connection with the generated documentation. The
position of subsystems and its structures are to be specified
here in case the OCS-subsystem mechanism is applied for the 
implementation of the project to be documented.

The consistency of the specified configuration is checked 
separately in the "Check Configuration Consistency" tool.

The "Start DOSFOP" button transforms the documented project 
into an intermediate representation (if no errors occur). 
Subsequently users can decide to produce a printable documentation 
as well as an info-hypertext after successful completion of the 
translation process.

The final products (dvi-file and/or info-hypertext) can be 
examined -if they have been produced previously- in the last
stage via the rightmost two buttons. Disablesd buttons 
indicate that the dvi-file or info-hypertex are not available 
for previewing.}

#---

set context(.globalConfig) \
{The "Global Configuration" tool provides a database with the
following contents:

(1) The MANDATORY definition of the name of the top-structure
    of the project. The name "Main" is provided by the 
    DOSFOP default mechanism. But certainly this name will not 
    be appropriate for any top-structure.
(2) Textual information on the project. Textual information
    presented on the top sheet of the printable documentation
    as well as in the top-node of the info-hypertext can be 
    specified here. Moreover a global survey on the project
    functionalities can be formulated by the documenting user.
(3) Global Macro Definitions. Macros decrease the amount of 
    Texinfo markup commands that are specified in the informal
    documentaries.
(4) Global option settings that serve as a pool for default
    option-settings for lower-level structures, structure-groups
    and subsystems. Options in general serve as a means for
    manipulating the appearance of the generated documentation.}

#---

set context(.projectBrowser)\
{The "Project Browser" tool enables the user to 
define a documentation specific structure on the project. 
Subsystems can be hierarchically ordered not depending on the
structure defined for the OPAL Compilation System w.r.t. the
UNIX-filesystem. The tool administrates its own documentation-
oriented structure on the OPAL source modules.

The globally specified shape of the documentation to be 
generated for the project can be adjusted locally by
configuration tools for different levels of abstraction. These
specific configurations can be defined for 

 (1) single structures
 (2) complete subsystems
 (3) structure groups of a subsystem or top-level structures

}

#---

set context(.checkConsistency)\
{Consistency of a configuration w.r.t. a specific OPAL-project
is a precondition for a successful translation of a documented
OPAL-project into a printable documentation as well as an info-
hypertext. Consistency in the context of the DOSFOP documenta-
tion system means that all information needed for the construc-
tion of the complete project documentation can be extracted 
from the UNIX-filesystem. This means for example:

- For every structure it can be decided if it belongs to the
  Standard OPAL-library (Bibliotheca Opalica) or is a user-
  implemented structure.
- Parts of structures that are to be included into the 
  project documentation are accessible for DOSFOP, i.e. the
  location of a structure specified during the configuration
  process is valid.
- Context-information from InterOpal-databases is available.

These consistency conditions are checked w.r.t. a given
configuration and errors are reported in a protocol-window.
The current state of configuration check is displayed on the
top of the window.

Note that a consistent configuration does NOT necessarily mean
that the DOSFOP translation process will be successful. Errors
like e.g. wrong macro references, syntactic errors in the
documentaries, references to unavailable postscript-pictures
or errors in the application Texinfo-markup language can also 
make the DOSFOP translation process fail!

}
#---

set context(.dosfop) \
"The DOSFOP-translation process is initiated with the current
configuration. 

On error the translation terminates and has to
be started again.

On success, the user is asked to produce a printable 
documentation and/or an info-hypertext. Errors that occur here
indicate wrong usage of the Texinfo markup language. A list of
admitted Texinfo-commands is given below:
$admissableTexinfoCommands"



#---

set context(.xdvi)\
{Start of a previewer for the printable documentation. 

If the button is disabled, one has to initiate a 
DOSFOP-translation process first, to produce a 
previewable dvi-file!

Xdvi recognizes the following keystrokes when typed in its
window.  Each may optionally be preceded by a (positive or
negative) number, whose interpretation will depend on  the
particular  keystroke.  Also, the "Home", "Prior", "Next",
and arrow cursor keys are synonyms for `^', `b', `f', `l',
`r', `u', and `d' keys, respectively.

q      Quits the program.  Control-C and control-D will do
       this, too.

n      Moves to the next page (or to the nth next page  if
       a number  is given).  Synonyms are `f', Space, Re-
       turn, and Line Feed.

p      Moves to the previous page (or back n pages).  Syn-
       onyms are `b', control-H, and Delete.

g, j   Moves  to the page with the given number. Initial-
       ly, the first page is assumed to be page number  1,
       but this can be changed with the `P' keystroke, be-
       low.  If no page number is given, then it goes  to
       the last page.

P      ``This  is  page  number  n.''  This can be used to
       make the `g' keystroke refer to actual page numbers
       instead of absolute page numbers.

Control-L
       Redisplays the current page.

^      Move to the ``home'' position of the page.  This is
       normally the upper left-hand corner  of  the  page,
       depending on the margins as described in the -mar-
       gins option, above.

u      Moves up two thirds of a window-full.

d      Moves down two thirds of a window-full.

l      Moves left two thirds of a window-full.

r      Moves right two thirds of a window-full.

c      Moves the page so that the point currently  beneath
       the  cursor  is  moved to the middle of the window.
       It also (gasp!) warps the cursor to the same place.

M      Sets  the margins so that the point currently under
       the cursor is the upper  left-hand  corner  of  the
       text  in  the  page.  Note that this command itself
       does not move the image at all.  For details on how
       the margins are used, see the -margins option.

s      Changes  the shrink factor to the given number.  If
       no number is given, the smallest factor that  makes
       the  entire  page fit  in the window will be used.
       (Margins are ignored in this computation.)

S      Sets the density factor to be used  when  shrinking
       bitmaps.  This  should  be  a number between 0 and
       100; higher numbers produce lighter characters.

R      Forces the dvi file to be reread. This allows  you
       to  preview  many versions  of the same file while
       running xdvi only once.

k      Normally when xdvi switches pages, it moves to  the
       home position as well.  The `k' keystroke toggles a
       `keep-position' flag which, when set, will keep the
       same position when moving between pages.  Also `0k'
       and `1k' clear and  set  this  flag,  respectively.
       See also the -keep option.

x      Toggles  expert  mode  (in which the buttons do not
       appear).  Also `0x' and `1x' clear and  reset  this
       mode, respectively.}

#---

set context(.info)\
{An info-hypertext reader containing the hypertext for 
the current project is displayed.

If the button is disabled, one has to initiate a 
DOSFOP-translation process first, to produce a 
previewable dvi-file!

When in info the following commands are available:
==================================================

h      Invoke the Info tutorial.

?      Get a short summary of info commands.

h      Select the info node from the main directory;  this
       is much more complete than just using ?.

Ctrl-g Abort whatever you are doing.

Ctrl-l Redraw the screen.

Selecting other nodes:
----------------------
n      Move to the "next" node of this node.

p      Move to the "previous" node of this node.

u      Move to this node's "up" node.

m      Pick  a menu item specified by name. Picking a menu
       item causes another node to be selected. You do not
       need to type a complete nodename; if you type a few
       letters and then a space or tab info will will  try
       to fill in the rest of the nodename. If you ask for
       further completion without typing any more  charac-
       ters  you'll  be given a list of possibilities; you
       can also get the list with ?.  If you  type  a  few
       characters  and then hit return info will try to do
       a completion, and if it is ambigous use  the  first
       possibility.

f      Follow  a  cross  reference.  You are asked for the
       name of the reference, using command completion  as
       for m.

l      Move to the last node you were at.

Moving within a node:
---------------------
Space  Scroll forward a page.

DEL    Scroll backward a page.

b      Go to the beginning of this node.

Advanced commands:
------------------
q      Quit info.

1      Pick first item in node's menu.

2 -- 5 Pick second ... fifth item in node's menu.

g      Move  to node specified by name. 

s      Search through  this  info  file  for  a  specified
       string,  and  select  the  node  in  which the next
       occurrence is found.
}

# --
set context(.html) \
{Starts netscape for viewing the generated HTML document.

Some WWW browsers have a bug which makes the links in the 
indices almost unusable. The browsers netscape, lynx, arena 
do handle the indices properly. See texi2html(1) for 
more details.
}

# for global configuration window
#-------------------------------- 

set global(globalConfig) {Global help for Global Configuration}

# for subsystem configuration window
#----------------------------------- 

set global(subsystemConfig) {Global help for Subsystem Configuration}

# for structure configuration window
#----------------------------------- 

set global(structureConfig) {Global help for Structure Configuration}


# for structure global configuration window
#------------------------------------------ 

set global(structuresConfigGlobal) {Global help for Structures Configuration}

# for browse window
#------------------ 

set global(browse) $context(.projectBrowser)

set context(.locationText) \
{Help on Location Indicator}
set context(.subsystemListboxHeadline) \
{Help on Subsystem Name List Headline}
set context(.subsystemListbox) \
{Help on Subsystem Name List}
set context(.subsystemDelete) \
{First mark then delete marked subsystem-name.}
set context(.subsystemInsert) \
{Insert a sub-subsystem into the current subsystem. Keep in mind: The 
order of subsystems is significant!}
set context(.subsystemRename) \
{First mark then rename the marked subsystem.}
set context(.subsystemConfig) \
{First mark then configure options for the whole marked subsystem.}

#---

set context(.subsystemMove) \
{The order of subsystem names in the listbox above is
significant w.r.t. the generated documentation. The "Move"
functionality enables the user to REORDER the sequence 
of subsystem names.

First mark the subsystem name to be reordered (left mouse
button), then press this button. A hand-cursor will appear 
if one enters the subsystem listbox. The fingertip of the 
hand denotes the new position of the marked subsystem (name).
Press the left mouse-button to confirm the new position.}

#---

set context(.structsListboxHeadline) \
{Structures of the current subsystem.}
set context(.structsListbox) \
{Structures of the current subsystem.
Invoke editor by Double-Click on Mouse-Button 1 when Mouse-Cursor
is located above a structure's name.}
set context(.structsDelete) \
{First mark then delete marked structure.}
set context(.structsInsert) \
{Insert a structure into the current subsystem.
One can use the Filebrowser (see File-Menu) as supportting-tool. }
set context(.structsRename) \
{First mark then rename marked structure.}
set context(.structsConfigGlobal) \
{Initiate configuration which is valid for ALL structures of the 
current subsystem.}
set context(.structsConfig) \
{Initiate configuration for the structure that is currently marked.}

# for filebrowser window
#-----------------------

set global(filebrowser) {Global Help on Filebrowser}

set context(.currdir) \
{Double-click on this name to transfer it to the subsystem-insertion 
widget (if it is opened).}
set context(.list) \
{Double-click on directory-name to browse through directory-tree.
Double-click on file-name to transfer it to the structure-insertion
widget (if it is opened).}

# for configuration widgets
#---------------------------

set context(.name) {Just the headline of the current window...} 

#---

set context(.location_entry) \
"Specification of the absolute path where the top-structure
of the project is located.

Note: You have to specify a complete pathname!

One can use TAB-completion -known from C-shell file
specification- in order to reduce typing effort.

$bindingsInEntries
"

#---

set context(.output_file_entry) \
"The file specified here denotes the location of the
intermediate code produced during the DOSFOP translation.
Initially, this name is chosen by default during installation
of the graphical user interface.

It is not needed to modify this file reference as it is 
just used transparently for internal purposes. If the 
intermediate representation should be furtherly process
by other tools, a modification might be necessary.

Note: You have to specify a complete pathname!

One can use TAB-completion -known from C-shell file
specification- in order to reduce typing effort.

$bindingsInEntries
"
#---

set context(.top_structure_entry) \
"Name of the structure that serves as reference point
for the calculation of import relations. All structures 
that are imported via direct or transitive import-
relations by the structure specified here build the
complete OPAL project.

The name of the top-structure has to specified WITHOUT
extensions like \".sign\", \".impl\", \".extp\" ot \".intp\". 

$bindingsInEntries"

#---

set context(.project_name_entry) \
"Name of the project that appears as the title of the 
printable documentation.

$bindingsInEntries
"
#---

set context(.authors_entry) \
"Specification of the names(s) of one (or more) author(s)
of the project's documentation. It appears on the titlepage
of the printable documentation.

$bindingsInEntries
"

#---

set context(.date_entry) \
"Specification of the creation date of the documentation. The
specified string appears on the titlepage
of the printable documentation.

$bindingsInEntries
"
#---

set context(.survey_button) \
"Specification of an overview on the functionalities of the 
project or a sub-part of the project. A reduced set of
Texinfo commands can be applied. No macro application allowed
here! 

List of admissable Texinfo commands in a survey:
$admissableTexinfoCommands

$bindingsInTexts
"


#---

set context(.macros_button) \
{Definiton of Texinfo-Macros that can be applied globally
when formulating informal documentaries.}

#---

set context(.functionality_index_setting) \
  {Help on functionality_index  (Refer to printed documentation)}
set context(.functionality_index_config) \
  {Help on functionality_index_config (Refer to printed documentation) }
set context(.application_index_setting) \
  {Help on application_index (Refer to printed documentation) }
set context(.application_index_config) \
  {Help on application_index_config (Refer to printed documentation) }
set context(.concept_index_setting) \
  {Help on concept_index (Refer to printed documentation) }
set context(.concept_index_config) \
  {Help on concept_index_config (Refer to printed documentation) }
set context(.structure_index_setting) \
  {Help on structure_index (Refer to printed documentation) }
set context(.structure_index_config) \
  {Help on structure_index_config (Refer to printed documentation) }
set context(.subsystems_include_setting) \
  {Help on subsystems_include (Refer to printed documentation) }
set context(.subsystems_include_config) \
  {Help on subsystems_include_config (Refer to printed documentation) }
set context(.library_include_setting) \
  {Help on library_include (Refer to printed documentation) }
set context(.library_include_config) \
  {Help on library_include_config (Refer to printed documentation) }
set context(.properties_include_setting) \
  {Help on properties_include (Refer to printed documentation) }
set context(.properties_include_config) \
  {Help on properties_include_config (Refer to printed documentation) }
set context(.only_interfaces_setting) \
  {Help on only_interfaces (Refer to printed documentation) }
set context(.only_interfaces_config) \
  {Help on only_interfaces_config (Refer to printed documentation) }
set context(.hierarchy_visu_setting) \
  {Help on hierarchy_visu (Refer to printed documentation) }
set context(.hierarchy_visu_config) \
  {Help on hierarchy_visu_config (Refer to printed documentation) }
set context(.sort_structures_setting) \
  {Help on sort_structures_visu (Refer to printed documentation) }
set context(.sort_structures_config) \
  {Help on sort_structures_config (Refer to printed documentation) }
set context(.import_references_setting) \
  {Help on import_references (Refer to printed documentation) }
set context(.import_references_config) \
  {Help on import_references_config (Refer to printed documentation) }
set context(.used_function_tables_setting) \
  {Help on used_function_tables (Refer to printed documentation) }
set context(.used_function_tables_config) \
  {Help on used_function_tables_config (Refer to printed documentation) }
set context(.language_setting) \
  {Help on language (Refer to printed documentation) }
set context(.language_config) \
  {Help on language_config (Refer to printed documentation) }
set context(.new_pages_setting) \
  {Help on new_pages (Refer to printed documentation) }
set context(.new_pages_config) \
  {Help on new_pages_config (Refer to printed documentation) }
set context(.drop_empty_lines_setting) \
  {Help on drop_empty_lines (Refer to printed documentation) }
set context(.drop_empty_lines_config) \
  {Help on drop_empty_lines_config (Refer to printed documentation) }
set context(.levels) \
  {Help on levels (Refer to printed documentation) }
set context(.levels_config) \
  {Help on levels_config (Refer to printed documentation) }




# other widgets
#--------------

set context(.mbar.help) \
{There are two different types of help available
in the context of each major application (configurators, 
browsers...):

(1) Global Help:
    This option displays a global overview on the basic 
    functionalties of the tool, where the "Global Help"
    has been requested.

(2) Context-Sensitive Help:
    Depending on the location of the mouse-pointer in the 
    considered application, a specific help-text is displayed,
    explaining the purpose of a particular widget in detail.
    Moving the mouse pointer over different widget induces a
    adaption of the content of the help-window. Press a mouse
    button if you do not want to change the help text
    currently displayed in the help window.}

set context(.mbar.file) \
{Help on file menubutton}








