(******************************************************************************)
(*                                                                            *)
(*                                 OCaml                                      *)
(*                                                                            *)
(*                           Samuel Hym, Tarides                              *)
(*                                                                            *)
(*   Copyright 2025 Tarides                                                   *)
(*                                                                            *)
(* Permission is hereby granted, free of charge, to any person obtaining a    *)
(* copy of this software and associated documentation files (the "Software"), *)
(* to deal in the Software without restriction, including without limitation  *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *)
(* and/or sell copies of the Software, and to permit persons to whom the      *)
(* Software is furnished to do so, subject to the following conditions:       *)
(*                                                                            *)
(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software.                        *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *)
(* DEALINGS IN THE SOFTWARE.                                                  *)
(*                                                                            *)
(******************************************************************************)

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

let encode sym =
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

let mangle_chunk sym =
  let pref, rsym =
    if String.for_all is_out_char sym then ("", sym) else ("u", encode sym)
  in
  Printf.sprintf "%s%d%s" pref (String.length rsym) rsym

let mangle ty sym uid =
  match ty with
  | `Named ->
      let b = Buffer.create 10 in
      Buffer.add_string b "_ON";
      List.iter (fun s -> Buffer.add_string b (mangle_chunk s)) sym;
      Buffer.add_char b '_';
      Buffer.add_string b uid;
      Buffer.contents b
  | `Anonymous -> "_OA"
