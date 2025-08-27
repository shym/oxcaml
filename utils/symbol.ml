(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-30-40-41-42"]

module CU = Compilation_unit

type t = {
  compilation_unit : Compilation_unit.t;
  linkage_name : Linkage_name.t;
  hash : int;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = compare t1.hash t2.hash in
      if c <> 0 then c
      else
        (* Linkage names are unique across a whole project, so just comparing
           those is sufficient. *)
        Linkage_name.compare t1.linkage_name t2.linkage_name

  let equal t1 t2 = compare t1 t2 = 0
  let output chan t = Linkage_name.output chan t.linkage_name
  let hash { hash; } = hash

  (* CR mshinwell: maybe print all fields *)
  let print ppf t = Linkage_name.print ppf t.linkage_name
end)

module type Name_mangling_scheme = sig
  val linkage_name_for_compilation_unit : Compilation_unit.t -> Linkage_name.t
  val this_is_ocamlc : bool ref
  val force_runtime4_symbols : bool ref
  val member_separator : unit -> string
end

module V0 : Name_mangling_scheme = struct
  let caml_symbol_prefix = "caml"

  (* CR ocaml 5 all-runtime5: Remove this_is_ocamlc and force_runtime4_symbols
     once fully on runtime5 *)
  let this_is_ocamlc = ref false
  let force_runtime4_symbols = ref true


  let upstream_runtime5_symbol_separator =
    match Config.ccomp_type with
    | "msvc" -> '$' (* MASM does not allow for dots in symbol names *)
    | _ -> '.'

  let separator () =
    if !this_is_ocamlc then
      Misc.fatal_error "Didn't expect utils/symbol.ml to be used in ocamlc";
    if Config.runtime5 && not !force_runtime4_symbols then
      Printf.sprintf "%c" upstream_runtime5_symbol_separator
    else
      "__"

  let member_separator = separator

  (* Constants used within this module *)
  let _pack_separator = separator
  let _instance_separator = "____"
  let _instance_separator_depth_char = '_'

  let linkage_name_for_compilation_unit comp_unit =
    (* CR-someday lmaurer: If at all possible, just use square brackets instead of
       this unholy underscore encoding. For now I'm following the original
       practice of avoiding non-identifier characters. *)
    let for_pack_prefix, name, flattened_instance_args = CU.flatten comp_unit in
    let name = CU.Name.to_string name in
    let suffix =
      if not (CU.Prefix.is_empty for_pack_prefix)
      then begin
        assert (flattened_instance_args = []);
        let pack_names =
          CU.Prefix.to_list for_pack_prefix |> List.map CU.Name.to_string
        in
        String.concat (_pack_separator ()) (pack_names @ [name])
      end else begin
        let arg_segments =
          List.map
            (fun (depth, _param, value) ->
               let extra_separators =
                 String.make depth _instance_separator_depth_char
               in
               let value = value |> CU.Name.to_string in
               String.concat "" [_instance_separator; extra_separators; value])
            flattened_instance_args
        in
        String.concat "" arg_segments
      end
    in
    caml_symbol_prefix ^ name ^ suffix
    |> Linkage_name.of_string
end

module V1 : Name_mangling_scheme = struct
  (* V1 scheme - placeholder for new implementation *)
  let this_is_ocamlc = ref false
  let force_runtime4_symbols = ref true


  (* V1 could have different separator logic *)
  let separator () =
    if !this_is_ocamlc then
      Misc.fatal_error "Didn't expect utils/symbol.ml to be used in ocamlc";
    if Config.runtime5 && not !force_runtime4_symbols then
      "."  (* Different from V0 - always use dot for V1 *)
    else
      "__"

  let member_separator = separator

  let linkage_name_for_compilation_unit comp_unit =
    (* Dummy implementation: just use V0 for now *)
    V0.linkage_name_for_compilation_unit comp_unit
end

let current_scheme : (module Name_mangling_scheme) =
  match Config.name_mangling_version with
  | "V0" -> (module V0)
  | "V1" -> (module V1)
  | version -> Misc.fatal_errorf "Unknown name mangling version: %s" version

(* Compatibility accessors - use current scheme's references *)
let this_is_ocamlc () =
  let (module Scheme) = current_scheme in
  Scheme.this_is_ocamlc := true

let force_runtime4_symbols () =
  let (module Scheme) = current_scheme in
  Scheme.force_runtime4_symbols := true

(* Compatibility accessors for existing code *)
let caml_symbol_prefix = "caml"

let member_separator () =
  let (module Scheme) = current_scheme in
  Scheme.member_separator ()

let linkage_name t = t.linkage_name

let linkage_name_for_ocamlobjinfo t =
  (* For legacy compatibility, even though displaying "Foo.Bar" is nicer
     than "Foo__Bar" *)
  let linkage_name = linkage_name t |> Linkage_name.to_string in
  assert (Misc.Stdlib.String.begins_with linkage_name
            ~prefix:caml_symbol_prefix);
  let prefix_len = String.length caml_symbol_prefix in
  String.sub linkage_name prefix_len (String.length linkage_name - prefix_len)

let compilation_unit t = t.compilation_unit

(* CR-someday lmaurer: Would be nicer to have some of this logic in
   [Linkage_name]; among other things, we could then define
   [Linkage_name.for_current_unit] *)

let linkage_name_for_compilation_unit comp_unit =
  let (module Scheme) = current_scheme in
  Scheme.linkage_name_for_compilation_unit comp_unit

let for_predef_ident id =
  assert (Ident.is_predef id);
  let linkage_name = "caml_exn_" ^ Ident.name id |> Linkage_name.of_string in
  let compilation_unit = CU.predef_exn in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name;
  }

let unsafe_create compilation_unit linkage_name =
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name; }

let for_name compilation_unit name =
  let prefix =
    linkage_name_for_compilation_unit compilation_unit |> Linkage_name.to_string
  in
  let linkage_name =
    prefix ^ (member_separator ()) ^ name |> Linkage_name.of_string
  in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name; }

let for_local_ident id =
  assert (not (Ident.is_global_or_predef id));
  let compilation_unit = CU.get_current_exn () in
  for_name compilation_unit (Ident.unique_name id)

let for_compilation_unit compilation_unit =
  let linkage_name = linkage_name_for_compilation_unit compilation_unit in
  { compilation_unit;
    linkage_name;
    hash = Hashtbl.hash linkage_name;
  }

let for_current_unit () =
  for_compilation_unit (CU.get_current_exn ())

let const_label = ref 0

let for_new_const_in_current_unit () =
  incr const_label;
  for_name (Compilation_unit.get_current_exn ()) (Int.to_string !const_label)

let is_predef_exn t =
  CU.equal t.compilation_unit CU.predef_exn
