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

(* Named after the similar c++filt command *)

type action = Mangle | Demangle_OCaml | Demangle_C | Demangle_Cxx

let parse argv =
  let open Arg in
  let action = ref Demangle_OCaml and verbose = ref false in
  let set a () = action := a in
  let print s =
    match (!verbose, !action) with
    | false, _ -> print_endline s
    | _, Mangle -> Printf.printf "Mangled: %s\n" s
    | _, Demangle_OCaml -> Printf.printf "OCaml:   %s\n" s
    | _, Demangle_C -> Printf.printf "C:       %s\n" s
    | _, Demangle_Cxx -> Printf.printf "C++:     %s\n" s
  in
  let perform (x : string) =
    (match !action with
    | Mangle ->
        Ocamlfilt_ocaml.Mangle.mangle `Named
          (String.split_on_char '.' x)
          "suffix"
    | Demangle_OCaml -> Ocamlfilt_ocaml.Demangle.demangle_as_string x
    | Demangle_C -> Ocamlfilt_demangle_c.demangle x
    | Demangle_Cxx -> Ocamlfilt_demangle_cxx.demangle x)
    |> print
  in
  let specs =
    [
      ("-v", Set verbose, "Set the verbose mode");
      ("-c", Unit (set Demangle_C), "Use the C demangler");
      ("-cxx", Unit (set Demangle_Cxx), "Use the C++ demangler");
      ("-o", Unit (set Demangle_OCaml), "Use the OCaml demangler (default)");
      ("-m", Unit (set Mangle), "Mangle");
      ("--", Rest perform, "Process all remaining arguments as symbols");
    ]
  and usage =
    "ocamlfilt [-c] [-cxx] [-o] [-m] [-v] [symbol...]: (de)mangle OCaml symbols"
  in
  try parse_argv ~current:(ref 0) argv specs perform usage with
  | Help msg -> Printf.printf "%s" msg
  | Bad msg ->
      Printf.eprintf "%s" msg;
      exit 1

let () = parse Sys.argv
