# $Id$
proc errorWidget text {

  toplevel .errorFrame
  wm title .errorFrame "Error"

  message .errorFrame.msg -width 6c -text $text
  button  .errorFrame.exit -text "Dismiss" -command "destroy .errorFrame"

  pack .errorFrame.msg .errorFrame.exit -in .errorFrame -fill x

  grab set .errorFrame.exit
}
