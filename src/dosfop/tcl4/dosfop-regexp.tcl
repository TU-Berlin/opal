### some procedure based on regular expressions

# $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/dosfop/tcl4/dosfop-regexp.tcl,v 1.1.1.1 1998-06-16 16:00:44 wg Exp $

uplevel #0 append dosfopVersions {dosfop-regexp\ 1.01\n}


# return match for regexp in string
proc dosfop-getMatch {actRegexp str} {
  set match " "
  regexp $actRegexp $str _ match
  return $match
}

# return match for regexp in file
proc dosfop-fileMatch {actRegexp filename } {
    dosfop-getMatch $actRegexp [dosfop-readFile $filename]
}

# return whether string tocheck matches regularExpr
proc dosfop-check {toCheck regularExpr} {
  set matched [regexp $regularExpr $toCheck matchvar]
  if $matched then {
    return [expr [string compare $toCheck $matchvar] == 0]
  } else {
    return 0
  }
}


