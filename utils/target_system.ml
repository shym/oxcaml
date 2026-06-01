[@@@ocaml.warning "+a-4-30-40-41-42"]

type architecture =
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z
  | Riscv

let architecture () : architecture =
  match Config.architecture with
  | "i386" -> IA32
  | "amd64" -> X86_64
  | "arm" -> ARM
  | "arm64" -> AArch64
  | "power" -> POWER
  | "s390x" -> Z
  | "riscv" -> Riscv
  | arch -> Misc.fatal_errorf "Unknown architecture `%s'" arch

let is_arm () =
  match architecture () with
  | ARM | AArch64 -> true
  | _ -> false

let is_64_bit () =
  match architecture () with
  | X86_64
  | AArch64
  | POWER
  | Z
  | Riscv -> true
  | IA32
  | ARM -> false

let is_32_bit () = not (is_64_bit ())

type derived_system =
  | Linux
  | MinGW_32
  | MinGW_64
  | Win32
  | Win64
  | Cygwin
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Solaris
  | Dragonfly
  | GNU
  | BeOS
  | Unknown

let derived_system () : derived_system =
  (* /!\ This should be kept in sync with [configure.ac], mostly using
     [utils/target_system.sh] *)
  match architecture (), Config.model, Config.system with
  (* Part generated from [configure.ac] by [target_system.sh] *)
  (* BEGIN target_system.sh *)
  | AArch64, _, "freebsd" -> FreeBSD
  | AArch64, _, "linux" -> Linux
  | AArch64, _, "macosx" -> MacOS_like
  | AArch64, _, "netbsd" -> NetBSD
  | AArch64, _, "openbsd" -> OpenBSD
  | ARM, "armv5", "linux" -> Linux
  | ARM, "armv5te", "linux" -> Linux
  | ARM, "armv6", "freebsd" -> FreeBSD
  | ARM, "armv6", "linux" -> Linux
  | ARM, "armv6", "netbsd" -> NetBSD
  | ARM, "armv6t2", "linux" -> Linux
  | ARM, "armv7", "linux" -> Linux
  | ARM, "armv7", "netbsd" -> NetBSD
  | ARM, "armv8", "linux" -> Linux
  | ARM, _, "linux" -> Linux
  | ARM, _, "openbsd" -> OpenBSD
  | IA32, _, "beos" -> BeOS
  | IA32, _, "cygwin" -> Cygwin
  | IA32, _, "freebsd" -> FreeBSD
  | IA32, _, "gnu" -> GNU
  | IA32, _, "linux" -> Linux
  | IA32, _, "mingw" -> MinGW_32
  | IA32, _, "netbsd" -> NetBSD
  | IA32, _, "openbsd" -> OpenBSD
  | IA32, _, "win32" -> Win32
  | POWER, "ppc64le", "linux" -> Linux
  | POWER, "ppc64", "linux" -> Linux
  | Riscv, "riscv64", "linux" -> Linux
  | X86_64, _, "beos" -> BeOS
  | X86_64, _, "cygwin" -> Cygwin
  | X86_64, _, "dragonfly" -> Dragonfly
  | X86_64, _, "freebsd" -> FreeBSD
  | X86_64, _, "gnu" -> GNU
  | X86_64, _, "linux" -> Linux
  | X86_64, _, "macosx" -> MacOS_like
  | X86_64, _, "mingw64" -> MinGW_64
  | X86_64, _, "netbsd" -> NetBSD
  | X86_64, _, "openbsd" -> OpenBSD
  | X86_64, _, "solaris" -> Solaris
  | X86_64, _, "win64" -> Win64
  | Z, "z10", "linux" -> Linux
  (* END target_system.sh *)
  | _, _, "unknown" -> Unknown
  | _, _, _ ->
    Misc.fatal_errorf
      "Cannot determine system type (model %s, system %s): ensure \
       `target_system.ml' matches `configure'"
      Config.model Config.system

let is_windows () =
  match derived_system () with
  | Linux | MacOS_like | FreeBSD | NetBSD | OpenBSD | Solaris | Dragonfly | GNU
  | BeOS | Unknown ->
    false
  | MinGW_32 | MinGW_64 | Win32 | Win64 | Cygwin -> true

type assembler =
  | GAS_like
  | MacOS
  | MASM

let assembler () =
  match derived_system () with
  | Win32 | Win64 -> MASM
  | MacOS_like -> MacOS
  | MinGW_32 | MinGW_64 | Cygwin | Linux | FreeBSD | NetBSD | OpenBSD
  | Solaris | GNU | Dragonfly | BeOS | Unknown ->
    GAS_like

module Machine_width = struct
  type t =
    | Thirty_two  (* Traditional 32-bit OCaml with GC tag bit *)
    | Thirty_two_no_gc_tag_bit  (* JavaScript mode with full 32-bit integers *)
    | Sixty_four  (* Traditional 64-bit OCaml with GC tag bit *)

  let print ppf = function
    | Thirty_two -> Format.fprintf ppf "Thirty_two"
    | Thirty_two_no_gc_tag_bit -> Format.fprintf ppf "Thirty_two_no_gc_tag_bit"
    | Sixty_four -> Format.fprintf ppf "Sixty_four"

  let equal t1 t2 =
    match t1, t2 with
    | Thirty_two, Thirty_two
    | Thirty_two_no_gc_tag_bit, Thirty_two_no_gc_tag_bit
    | Sixty_four, Sixty_four -> true
    | _ -> false

  let is_32_bit = function
    | Thirty_two | Thirty_two_no_gc_tag_bit -> true
    | Sixty_four -> false

  let is_64_bit = function
    | Thirty_two | Thirty_two_no_gc_tag_bit -> false
    | Sixty_four -> true

  let size_in_bytes = function
    | Thirty_two | Thirty_two_no_gc_tag_bit -> 4
    | Sixty_four -> 8
end

type windows_system =
  | Cygwin
  | MinGW
  | Native

type system =
  | Linux
  | Windows of windows_system
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Solaris
  | Dragonfly
  | GNU
  | BeOS
  | Unknown

let system () : system =
  match derived_system () with
  | Linux -> Linux
  | MinGW_32 | MinGW_64 -> Windows MinGW
  | Win32 | Win64 -> Windows Native
  | Cygwin -> Windows Cygwin
  | MacOS_like -> MacOS_like
  | FreeBSD -> FreeBSD
  | NetBSD -> NetBSD
  | OpenBSD -> OpenBSD
  | Solaris -> Solaris
  | Dragonfly -> Dragonfly
  | GNU -> GNU
  | BeOS -> BeOS
  | Unknown -> Unknown

let windows () =
  match system () with
  | Windows _ -> true
  | _ -> false

let is_macos () =
  match assembler () with
  | MASM | GAS_like -> false
  | MacOS -> true

let is_gas () =
  match assembler () with
  | MASM | MacOS -> false
  | GAS_like -> true
