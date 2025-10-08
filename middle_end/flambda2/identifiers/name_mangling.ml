type path_item =
  | Module of string
  | AnonymousFunction of int * int * string option
  | NamedFunction of string
  | PartialFunction

(*
  let n = [Module "Foo"; Module "Bar"; NamedFunction "baz"]
 *)
type path = path_item list

let debug_info_scope_mangling_item_to_path_item (item : Debuginfo.Scoped_location.mangling_item) : path_item =
  match item with
  | Module name -> Module name
  | AnonymousFunction loc ->
    AnonymousFunction (loc.loc_start.pos_lnum, loc.loc_start.pos_cnum, Some loc.loc_start.pos_fname)
  | NamedFunction name -> NamedFunction name
  | PartialFunction -> PartialFunction

let rec path_of_debug_info_scopes (scopes: Debuginfo.Scoped_location.scopes) =
  match scopes with
  | Empty -> []
  | Cons { item = _; name = _; str = _; str_fun = _; assume_zero_alloc = _; prev; mangling_item = None} ->
      path_of_debug_info_scopes prev
  | Cons { item = _; name = _; str = _; str_fun = _; assume_zero_alloc = _; prev; mangling_item = Some mangling_item} ->
      debug_info_scope_mangling_item_to_path_item mangling_item :: (path_of_debug_info_scopes prev)

let path_of_debug_info dbg : path =
  match Debuginfo.to_items dbg with
  | [] -> Misc.fatal_error "path_of_debug_info: no debug info for function"
  | [item] -> path_of_debug_info_scopes item.dinfo_scopes
  | _ :: _ :: _ -> Misc.fatal_error "path_of_debug_info: multiple debug info items for function"


let mangle_path _ = assert false

let mangle_comp_unit _ = assert false

let mangle_ident _ = assert false