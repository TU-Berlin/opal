# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl/stack.tcl,v 1.1.1.1 1998-06-16 16:00:31 wg Exp $
proc emptyStack {} {
   return {}
}

proc isEmpty stack {
  return [expr [llength $stack] == 0]
}

proc push {stack elem} {
  return [linsert $stack 0 $elem]
}

proc top stack {
  return [lindex $stack 0]
}

proc pop stack {
  return [lrange $stack 1 end]
}

proc singleton stack {
  return [expr [llength $stack] == 1]
}
