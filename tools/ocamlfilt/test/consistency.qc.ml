open Base_quickcheck

module NamedSymbol : Test.S with type t = string list * string = struct
  open Generator

  (* Upstream OCaml supports some Unicode letters starting from 5.3 *)
  let uppercase_unicode_letters =
    [
      "À";
      "Á";
      "Â";
      "Ã";
      "Ä";
      "Å";
      "Æ";
      "Ç";
      "È";
      "É";
      "Ê";
      "Ë";
      "Ì";
      "Í";
      "Î";
      "Ï";
      "Ð";
      "Ñ";
      "Ò";
      "Ó";
      "Ô";
      "Õ";
      "Ö";
      "Ø";
      "Ù";
      "Ú";
      "Û";
      "Ü";
      "Ý";
      "Þ";
      "Š";
      "Ž";
      "Œ";
      "Ÿ";
      "ẞ";
    ]

  let lowercase_unicode_letters =
    [
      "à";
      "á";
      "â";
      "ã";
      "ä";
      "å";
      "æ";
      "ç";
      "è";
      "é";
      "ê";
      "ë";
      "ì";
      "í";
      "î";
      "ï";
      "ð";
      "ñ";
      "ò";
      "ó";
      "ô";
      "õ";
      "ö";
      "ø";
      "ù";
      "ú";
      "û";
      "ü";
      "ý";
      "þ";
      "š";
      "ž";
      "œ";
      "ÿ";
      "ß";
    ]

  let general_uppercase =
    weighted_union
      [
        (5., map ~f:(String.make 1) char_uppercase);
        (1., of_list uppercase_unicode_letters);
      ]

  let general_lowercase =
    weighted_union
      [
        (5., map ~f:(String.make 1) char_lowercase);
        (1., of_list lowercase_unicode_letters);
      ]

  let general_char =
    weighted_union
      [
        (26., general_uppercase);
        (26., general_lowercase);
        (10., map ~f:(String.make 1) char_digit);
        (1., return "_");
        (1., return "'");
      ]

  let alphanum_symbol cap =
    let symchar =
      weighted_union
        [ (62., char_alphanum); (1., return '_'); (1., return '\'') ]
    in
    let ascii =
      map
        ~f:(fun c s -> Printf.sprintf "%c%s" c s)
        (if cap then char_uppercase else char_lowercase)
      <*> string_of symchar
    and general =
      small_positive_or_zero_int >>= fun length ->
      map
        ~f:(fun c s -> String.concat "" (c :: s))
        (if cap then general_uppercase else general_lowercase)
      <*> list_with_length general_char ~length
    in
    weighted_union [ (3., ascii); (1., general) ]

  let operator =
    of_list
      [
        '!';
        '#';
        '$';
        '%';
        '&';
        '*';
        '+';
        '-';
        '.';
        '/';
        ':';
        '<';
        '=';
        '>';
        '?';
        '@';
        '^';
        '|';
        '~';
      ]
    |> string_non_empty_of

  let module_name = alphanum_symbol true

  let value_name =
    weighted_union
      [
        (8., alphanum_symbol false);
        (3., operator);
        (1., map ~f:(fun o -> "let" ^ o) operator);
      ]

  let named_symbol =
    both
      ( small_positive_or_zero_int >>= fun length ->
        map ~f:(fun p v -> p @ [ v ]) (list_with_length module_name ~length)
        <*> value_name )
      (map ~f:string_of_int small_positive_or_zero_int)

  let named_symbol =
    weighted_union
      [
        (99., named_symbol);
        ( 1.,
          both
            (list (string_of (char_uniform_inclusive '\001' '\255')))
            (return "1") );
      ]

  type t = string list * string

  let sexp_of_t (x, y) =
    Sexplib0.Sexp.(List [ List (List.map (fun z -> Atom z) x); Atom y ])

  let quickcheck_generator = named_symbol
  let quickcheck_shrinker = Shrinker.(both (list string) string)
end

module AnyString : Test.S with type t = string = struct
  type t = string

  let sexp_of_t x = Sexplib0.Sexp.Atom x

  let quickcheck_generator =
    let open Generator in
    let any = string_of (char_uniform_inclusive '\001' '\255') in
    union [ any; map ~f:(fun x -> "_ON" ^ x) any ]

  let quickcheck_shrinker = Shrinker.string
end

let ocaml_cxx_c_demangle_consistency sym =
  let ocaml = Ocamlfilt_ocaml.Demangle.demangle_as_string sym
  and cxx = Ocamlfilt_demangle_cxx.demangle sym
  and c = Ocamlfilt_demangle_c.demangle sym in
  if ocaml <> cxx then
    failwith (Printf.sprintf "%S: %S (OCaml) <> %S (C++)" sym ocaml cxx);
  if ocaml <> c then
    failwith (Printf.sprintf "%S: %S (OCaml) <> %S (C)" sym ocaml c)

let ocaml_cxx_c_mangle_demangle_consistency (name, uid) =
  let sym = Ocamlfilt_ocaml.Mangle.mangle `Named name uid in
  ocaml_cxx_c_demangle_consistency sym

let _ =
  let deterministic = true in
  let open Test in
  let config =
    if deterministic then default_config
    else { default_config with seed = Config.Seed.Nondeterministic }
  in
  run_exn ~config ~f:ocaml_cxx_c_mangle_demangle_consistency
    (module NamedSymbol);
  run_exn ~config ~f:ocaml_cxx_c_demangle_consistency (module AnyString)
