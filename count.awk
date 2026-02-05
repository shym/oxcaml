function check_prefix(base, n, pfx) {
  if (substr(base,length(pfx)+1) == n) { return "PFX" }
  return ""
}
function check_suffix(base, n, sfx) {
  if (substr(base,1,length(base)-length(sfx)-1) == n) { return "SFX" }
  return ""
}

$1 == "~name:" {
  name[$3] += 1
  base = substr($2, 1, match($2, /_[[:digit:]]*$/)-1)
  if (base == "") {
    base = substr($2, 1, match($2, /_[[:digit:]]*_partial$/)-1) "_timestamp_partial"
  }
  if (base == "") {
    name["notimestamp " $3] += 1
    base = $2
  }
  if (base == $4) { name["EQ " $3] += 1 }
  else {
    switch (base) {
    case "fn":
      name["fn " $3] += 1
      break
    case "equal":
      name["equal " $3] += 1
      break
    case "compare":
      name["compare " $3] += 1
      break
    case "hash":
      name["hash " $3] += 1
      break
    case /_dps$/:
      name[check_suffix(base, $4, "dps") "dps " $3] += 1
      break
    case /_inner$/:
      name[check_suffix(base, $4, "inner") "inner " $3] += 1
      break
    case /_timestamp_partial$/:
      name[check_suffix(base, $4, "timestamp_partial") "ts_partial " $3] += 1
      break
    case /_partial$/:
      name[check_suffix(base, $4, "partial") "_partial " $3] += 1
      break
    case /_init$/:
      name[check_suffix(base, $4, "init") "init " $3] += 1
      print "INIT: " $0
      break
    case /^partial_/:
      name[check_prefix(base, $4, "partial") "partial " $3] += 1
      break
    default:
      name["else " $3] += 1
    }
  }
}

$2 == "=>" {
  items[$1] += 1
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
}
