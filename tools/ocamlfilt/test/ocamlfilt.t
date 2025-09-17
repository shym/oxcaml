Some simple uses of ocamlfilt

  $ ocamlfilt.opt -v -m open_in_bin
  Mangled: _ON11open_in_bin_suffix
  $ sym=_ON11open_in_bin_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   open_in_bin
  C:       open_in_bin
  C++:     open_in_bin

  $ ocamlfilt.opt -v -m In_channel.open_bin
  Mangled: _ON10In_channel8open_bin_suffix
  $ sym=_ON10In_channel8open_bin_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   In_channel.open_bin
  C:       In_channel.open_bin
  C++:     In_channel.open_bin

  $ ocamlfilt.opt -v -m 'let*+'
  Mangled: _ONu9D2a2b_let_suffix
  $ sym=_ONu9D2a2b_let_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   let*+
  C:       let*+
  C++:     let*+

  $ ocamlfilt.opt -v -m étoile
  Mangled: _ONu11Ac3a9_toile_suffix
  $ sym=_ONu11Ac3a9_toile_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   étoile
  C:       étoile
  C++:     étoile

  $ ocamlfilt.opt -v -m 'Module.Sub.let+'
  Mangled: _ON6Module3Subu7D2b_let_suffix
  $ sym=_ON6Module3Subu7D2b_let_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   Module.Sub.let+
  C:       Module.Sub.let+
  C++:     Module.Sub.let+

  $ ocamlfilt.opt -v -m '>>='
  Mangled: _ONu8A3e3e3d__suffix
  $ sym=_ONu8A3e3e3d__suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   >>=
  C:       >>=
  C++:     >>=

  $ ocamlfilt.opt -v -m "name'"
  Mangled: _ONu8E27_name_suffix
  $ sym=_ONu8E27_name_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   name'
  C:       name'
  C++:     name'

  $ ocamlfilt.opt -v -m "name'1"
  Mangled: _ONu9E27_name1_suffix
  $ sym=_ONu9E27_name1_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   name'1
  C:       name'1
  C++:     name'1

  $ ocamlfilt.opt -v -m abcdefghijklmnopqrstuvwxyzé
  Mangled: _ONu33BAc3a9_abcdefghijklmnopqrstuvwxyz_suffix
  $ sym=_ONu33BAc3a9_abcdefghijklmnopqrstuvwxyz_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   abcdefghijklmnopqrstuvwxyzé
  C:       abcdefghijklmnopqrstuvwxyzé
  C++:     abcdefghijklmnopqrstuvwxyzé

  $ ocamlfilt.opt -v -m abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé
  Mangled: _ONu65BAc3a9BAc3a9_abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz_suffix
  $ sym=_ONu65BAc3a9BAc3a9_abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé
  C:       abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé
  C++:     abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé

  $ ocamlfilt.opt -v -m "abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé'"
  Mangled: _ONu67BAc3a9BAc3a927_abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz_suffix
  $ sym=_ONu67BAc3a9BAc3a927_abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz_suffix
  $ ocamlfilt.opt -v $sym -c $sym -cxx $sym
  OCaml:   abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé'
  C:       abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé'
  C++:     abcdefghijklmnopqrstuvwxyzéabcdefghijklmnopqrstuvwxyzé'
