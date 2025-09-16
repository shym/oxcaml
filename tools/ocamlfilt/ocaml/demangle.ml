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

let decode_len str pos =
  let rec aux n =
    match str.[!pos] with
    | 'A' .. 'Z' as l ->
        incr pos;
        aux ((n * 26) + (Char.code l - Char.code 'A'))
    | _ -> n
  in
  match str.[!pos] with
  | '_' -> None
  | 'A' .. 'Z' -> Some (aux 0)
  | _ -> invalid_arg "No length to decode"

let decode_char h1 h2 =
  let value = function
    | '0' .. '9' as c -> Char.code c - Char.code '0'
    | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
    | _ -> invalid_arg "Cannot decode hexdigit out of [0-9a-f]"
  in
  Char.chr ((value h1 lsl 4) lor value h2)

let decode_chars buf str pos =
  let rec loop () =
    match str.[!pos] with
    | '0' .. '9' | 'a' .. 'f' ->
        Buffer.add_char buf (decode_char str.[!pos] str.[!pos + 1]);
        pos := !pos + 2;
        loop ()
    | _ -> ()
  in
  loop ()

let decode sym =
  let res = Buffer.create (String.length sym) in
  let enc_pos = ref 0
  and raw_pos =
    ref
      (try String.index sym '_' + 1
       with Not_found ->
         invalid_arg
           (Printf.sprintf "\"%s\" is not a valid component of a mangled name"
              sym))
  in
  let rec loop () =
    match decode_len sym enc_pos with
    | Some l ->
        if l > 0 then (
          Buffer.add_substring res sym !raw_pos l;
          raw_pos := !raw_pos + l);
        decode_chars res sym enc_pos;
        loop ()
    | None ->
        if !raw_pos < String.length sym then
          Buffer.add_substring res sym !raw_pos (String.length sym - !raw_pos)
  in
  loop ();
  Buffer.contents res

let is_digit = function '0' .. '9' -> true | _ -> false

let decode_decimal_int str pos =
  let rec len pos' =
    if pos' < String.length str && is_digit str.[pos'] then len (pos' + 1)
    else pos' - pos
  in
  match len pos with
  | 0 -> None
  | len ->
      Option.map
        (fun n -> (n, len))
        (int_of_string_opt (String.sub str pos len))

let demangle_exn sym =
  let err () =
    invalid_arg (Printf.sprintf "not a valid mangled symbol: \"%s\"" sym)
  in
  let rec loop acc = function
    | i when i >= String.length sym -> (acc, "")
    | i -> (
        match sym.[i] with
        | 'u' -> (
            match decode_decimal_int sym (i + 1) with
            | Some (n, len) ->
                if n <= 0 then err ();
                let chunk = decode (String.sub sym (i + 1 + len) n) in
                loop (chunk :: acc) (i + 1 + len + n)
            | None -> err ())
        | '_' -> (acc, String.sub sym (i + 1) (String.length sym - i - 1))
        | x when is_digit x -> (
            match decode_decimal_int sym i with
            | Some (n, len) ->
                if n <= 0 then err ();
                let chunk = String.sub sym (i + len) n in
                loop (chunk :: acc) (i + len + n)
            | None -> err ())
        | _ -> err ())
  in
  if sym.[0] <> '_' || sym.[1] <> 'O' then err ();
  let ty = match sym.[2] with 'A' -> `Anonymous | 'N' -> `Named | _ -> err ()
  and sym, uid = loop [] 3 in
  (ty, List.rev sym, uid)

let demangle_as_string sym =
  match demangle_exn sym with
  | _, sym, _ -> String.concat "." sym
  | exception Invalid_argument _ -> ""
