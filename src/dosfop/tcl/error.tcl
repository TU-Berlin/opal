# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl/error.tcl,v 1.1.1.1 1998-06-16 16:00:30 wg Exp $
proc errorWidget text {

  toplevel .errorFrame
  wm title .errorFrame "Error"

  message .errorFrame.msg -width 6c -text $text
  button  .errorFrame.exit -text "Dismiss" -command "destroy .errorFrame"

  pack .errorFrame.msg .errorFrame.exit -in .errorFrame -fill x

  grab set .errorFrame.exit
}
