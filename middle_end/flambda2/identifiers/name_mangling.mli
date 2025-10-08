type path_item =
  | Module of string
  | AnonymousFunction of int * int * string option
  | NamedFunction of string
  | PartialFunction

(* let n = [Module "Foo"; Module "Bar"; NamedFunction "baz"] *)
type path = path_item list

val path_of_debug_info : Debuginfo.t -> path

(** Transform a [path] into a mangled name suitable for creating a LinkageName.t *)
val mangle_path : path -> string

val mangle_comp_unit : Compilation_unit.t -> string

val mangle_ident : Compilation_unit.t -> path -> string
