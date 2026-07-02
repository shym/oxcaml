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

include Target_derived_system

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

let is_windows () =
  match derived_system () with
  | Linux | MacOS_like | FreeBSD | NetBSD | OpenBSD | Generic_BSD | Solaris
  | Dragonfly | GNU | BeOS | Unknown ->
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
  | Generic_BSD | Solaris | GNU | Dragonfly | BeOS | Unknown ->
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
  | Generic_BSD
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
  | Generic_BSD -> Generic_BSD
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
