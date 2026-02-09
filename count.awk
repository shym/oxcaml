function check_prefix(base, n, pfx) {
  if (substr(base,length(pfx)+1) == n) { return "PFX" }
  return ""
}
function check_suffix(base, n, sfx) {
  if (substr(base,1,length(base)-length(sfx)-1) == n) { return "SFX" }
  return ""
}

$2 == "=>" {
  items[$1] += 1
  nb_items = $1
}

$1 == "~name:" {
  name[$3] += 1
  base = substr($2, 1, match($2, /_[[:digit:]]*$/)-1)
  if (base == "") {
    base = substr($2, 1, match($2, /_[[:digit:]]*_partial$/)-1) "_timestamp_partial"
    if (base == "_timestamp_partial") { base = "" }
  }
  if ($3 == "≠") {
    if ("partial_" $4 == base) { name["≠ partial " nb_items] += 1 }
    if ($4 "_timestamp_partial" == base) { name["≠ ts_partial " nb_items] += 1 }
    if ($4 "_dps" == base) { name["≠ dps " nb_items] += 1 }
    if ($4 "_inner" == base) { name["≠ inner " nb_items] += 1 }
    if ($4 == "PARTIAL") { name["≠ PARTIAL " nb_items] += 1 }
    if ($4 == "ANONYMOUS") { name["≠ ANONYMOUS " nb_items] += 1 }
  }
  if ($3 == "0≠") {
    basenodbg[base] += 1
  }
}

END {
  all = 0
  printf("| # items | occurrences |\n")
  printf("| ------: | ----------: |\n")
  for(i in items) {
    all += items[i]
    printf("| % 7d |   % 9d |\n", i, items[i])
  }
  printf("|     all |   % 9d |\n\n", all)

  printf("| category             | occurrences |\n")
  printf("| :------------------- | ----------: |\n")
  for(n in name) {
    printf("| %20s | % 11d |\n", n, name[n])
  }

  printf("\n")

  printf("| function name (no debug info)            | occurrences |\n")
  printf("| :--------------------------------------- | ----------: |\n")
  for(sym in basenodbg) {
    printf("| %40s | % 11d |\n", sym, basenodbg[sym])
  }
}
