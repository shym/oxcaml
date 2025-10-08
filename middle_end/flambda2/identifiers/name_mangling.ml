type path_item =
  | Module of string
  | AnonymousFunction of int * int * string option
  | NamedFunction of string
  | PartialFunction
  | AnonymousModule of int * int * string option

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


let is_out_char = function
   | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
   | _ -> false

 let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 let hex = "0123456789abcdef"

 (** Encode a length as base-26 number using [[A-Z]] *)
 let rec encode_len buf len =
   let r = len mod 26 and q = len / 26 in
   if q > 0 then encode_len buf q;
   Buffer.add_char buf upper.[r]

 let encode_char buf c =
   let c = Char.code c in
   let h = (c lsr 4) land 0xf and l = c land 0xf in
   Buffer.add_char buf (hex.[h]);
   Buffer.add_char buf (hex.[l])

 type encode_state = Raw | Enc

 let encode (sym : string) =
   let raw = Buffer.create (String.length sym)
   and enc = Buffer.create (2 * String.length sym)
   and ins_pos = ref 0 in
   let rec aux i = function
     | _ when i >= String.length sym ->
        Printf.sprintf "%s_%s" (Buffer.contents enc) (Buffer.contents raw)
     | Raw ->
        if is_out_char sym.[i] then (
          Buffer.add_char raw sym.[i];
          incr ins_pos;
          aux (i + 1) Raw)
        else (
          encode_len enc !ins_pos;
          encode_char enc sym.[i];
          aux (i + 1) Enc)
     | Enc ->
        if is_out_char sym.[i] then (
          Buffer.add_char raw sym.[i];
          ins_pos := 1;
          aux (i + 1) Raw)
        else (
          encode_char enc sym.[i];
          aux (i + 1) Enc)
   in
   aux 0 Raw

 let mangle_chunk = function
   | Module sym ->
      let pref, rsym =
        if String.for_all is_out_char sym then ("", sym) else ("u", encode sym)
      in
      Printf.sprintf "%s%d%s" pref (String.length rsym) rsym
   | NamedFunction sym ->
      let pref, rsym =
        if String.for_all is_out_char sym then ("", sym) else ("u", encode sym)
      in
      Printf.sprintf "%s%d%s" pref (String.length rsym) rsym
   | AnonymousFunction (line, col, file_opt) ->
      let file_name = Option.value ~default:"" file_opt in
      let ts = Printf.sprintf "%s_%d_%d" file_name line col  in
      Printf.sprintf "%d%s" (String.length ts) ts
   | AnonymousModule (line, col, file_opt) ->
      let file_name = Option.value ~default:"" file_opt in
      let ts = Printf.sprintf "%s_%d_%d" file_name line col  in
      Printf.sprintf "%d%s" (String.length ts) ts
    | PartialFunction -> (* CR sspies: Implement. *) assert false

let mangle_path (path : path) : string =
  let b = Buffer.create 10 in
  List.iter (fun s -> Buffer.add_string b (mangle_chunk s)) path;
  Buffer.contents b

let mangle_path_with_prefix (path : path) : string =
  let b = Buffer.create 10 in
  Buffer.add_string b "_O";
  List.iter (fun s -> Buffer.add_string b (mangle_chunk s)) path;
  Buffer.contents b

let mangle_comp_unit (cu : Compilation_unit.t) : string =
  let for_pack_prefix, name, flattened_instance_args = Compilation_unit.flatten cu in
  let name = Compilation_unit.Name.to_string name in
  if not (Compilation_unit.Prefix.is_empty for_pack_prefix)
  then begin
      assert (match flattened_instance_args with [] -> true | _ -> false);
      let pack_names =
        Compilation_unit.Prefix.to_list for_pack_prefix
        |> List.map (fun x -> Module (Compilation_unit.Name.to_string x))
      in
      mangle_path_with_prefix (Module name :: (pack_names @ [Module name]))
    end else begin
      (* TODO For Parameterised libraries??? *)
      let instance_separator = "____" in
      let instance_separator_depth_char = '_' in
      let arg_segments =
        List.map
          (fun (depth, _param, value) ->
            let extra_separators =
              String.make depth instance_separator_depth_char
            in
            let value = value |> Compilation_unit.Name.to_string in
            Module (String.concat "" [instance_separator; extra_separators; value]))
          flattened_instance_args
      in
      mangle_path ((Module name) :: arg_segments)
    end

let mangle_ident (cu : Compilation_unit.t) (path : path) =
  let b = Buffer.create 10 in
  Buffer.add_string b (mangle_comp_unit cu);
  Buffer.add_string b  (mangle_path path);
  Buffer.contents b

