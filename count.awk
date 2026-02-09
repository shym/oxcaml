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
    if ("partial_" $4 == base) { name["partial " nb_items] += 1 }
    if ($4 "_timestamp_partial" == base) { name["ts_partial " nb_items] += 1 }
    if ($4 "_dps" == base) { name["dps " nb_items] += 1 }
    if ($4 "_inner" == base) { name["inner " nb_items] += 1 }
  }
  if ($3 == "0≠") {
    basenodbg[base] += 1
  }
}

END {
  all = 0
  for(i in items) {
    all += items[i]
    printf("%d:   % 7d\n", i, items[i])
  }
  printf("all: % 7d\n\n", all)

  for(n in name) {
    printf("%20s: % 7d\n", n, name[n])
  }

  printf("\n")

  for(sym in basenodbg) {
    printf("%40s: % 7d\n", sym, basenodbg[sym])
  }
}
