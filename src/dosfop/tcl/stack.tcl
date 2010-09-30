# $Id$
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
