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
    if ($4 == base) { name["≠ ="] += 1 }
    else { if ("partial_" $4 == base) { name["≠ partial"] += 1 }
    else { if ($4 "_timestamp_partial" == base) { name["≠ ts_partial"] += 1 }
    else { if ($4 "_dps" == base) { name["≠ dps"] += 1 }
    else { if ($4 "_inner" == base) { name["≠ inner"] += 1 }
    else {
      switch (base) {
        case /_timestamp_partial$/:
          name["≠≠ `" $5 "` `<timestamp>_partial` suffix "] += 1
          break
        case /^fn$/:
          name["≠≠ `" $5 "` `fn`"] += 1
          break
        case /^partial_/:
          name["≠≠ `" $5 "` `partial` prefix"] += 1
          break
        default:
          name["≠≠ `" $5 "`"] += 1
      }
    }}}}}
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

  printf("| category                                 | occurrences |\n")
  printf("| :--------------------------------------- | ----------: |\n")
  asorti(name, idxs)
  for(i in idxs) {
    printf("| %40s | % 11d |\n", idxs[i], name[idxs[i]])
  }

  printf("\n")

  printf("| function name (no debug info)            | occurrences |\n")
  printf("| :--------------------------------------- | ----------: |\n")
  asorti(basenodbg, idxs)
  for(i in idxs) {
    printf("| %40s | % 11d |\n", idxs[i], basenodbg[idxs[i]])
  }
}
