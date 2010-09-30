### some procedure based on regular expressions

# $Id$

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


