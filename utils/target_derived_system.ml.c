type derived_system =
  | Linux
#define linux() "linux" -> Linux
  | MinGW_32
#define mingw() "mingw" -> MinGW_32
  | MinGW_64
#define mingw64() "mingw64" -> MinGW_64
  | Win32
#define win32() "win32" -> Win32
  | Win64
#define win64() "win64" -> Win64
  | Cygwin
#define cygwin() "cygwin" -> Cygwin
  | MacOS_like
#define macosx() "macosx" -> MacOS_like
  | FreeBSD
#define freebsd() "freebsd" -> FreeBSD
  | NetBSD
#define netbsd() "netbsd" -> NetBSD
  | OpenBSD
#define openbsd() "openbsd" -> OpenBSD
  | Solaris
#define solaris() "solaris" -> Solaris
  | Dragonfly
#define dragonfly() "dragonfly" -> Dragonfly
  | GNU
#define gnu() "gnu" -> GNU
  | BeOS
#define beos() "beos" -> BeOS
  | Unknown
#define unknown() "unknown" -> Unknown

#define MATCH_TARGET(PAT,NATIVE,ARCH,MODEL,SYSTEM) \
  | #ARCH, #MODEL, SYSTEM()

/* The 32-bit variants are ignored as the only occurrence in the current source
 * is a duplicate of another case */
#define MATCH_TARGET_AND_64(PAT,ARCH64,MODEL64,ARCH32,MODEL32,SYSTEM) \
  | #ARCH64, #MODEL64, SYSTEM()

#define DEFAULT(NATIVE,ARCH,MODEL,SYSTEM) \
  | #ARCH, #MODEL, SYSTEM()

let derived_system () : derived_system =
  match Config.architecture, Config.model, Config.system with
#include "target_system_cases.tbl"
  | _, _, _ ->
    Misc.fatal_errorf
      "Cannot determine system type (model %s, system %s): ensure \
       `target_system.ml' matches `configure'"
      Config.model Config.system
