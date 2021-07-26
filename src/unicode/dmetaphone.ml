open! Core_kernel

let preserve = function
| "ç"
 |"Ç"
 |"ñ"
 |"Ñ" ->
  true
| _ -> false

let ( =* ) ~glyphs =
  let len = Array.length glyphs in
  (fun pos against -> if pos < 0 || pos >= len then false else String.( = ) (Array.get glyphs pos) against)

let ( <>* ) = String.( <> )

let get_at glyphs =
  let len = Array.length glyphs in
  (fun pos -> if pos < 0 || pos >= len then " " else Array.get glyphs pos)

let is_vowel = function
| "A"
 |"E"
 |"I"
 |"O"
 |"U"
 |"Y" ->
  true
| _ -> false

let slavo_germanic s =
  String.is_substring s ~substring:"W"
  || String.is_substring s ~substring:"K"
  || String.is_substring s ~substring:"CZ"

let obvious_germanic = function
| "V" :: "A" :: "N" :: " " :: _
 |"V" :: "O" :: "N" :: " " :: _
 |"S" :: "C" :: "H" :: _ ->
  true
| _ -> false

let prepare ~n arg =
  let rec loop = function
    | 0, acc
     |_, (([], _) as acc) ->
      acc
    | i, (x :: rest, dropped) -> loop (i - 1, (rest, x :: dropped))
  in
  loop (n, arg)

let double_metaphone ?(max_length = 4) ~standardized:original ~glyphs =
  let debug (here : Source_code_position.t) =
    if false then print_endline (sprintf "[%d]" here.pos_lnum)
  in
  (* original: AMELIE *)
  (* glyphs: [| "A"; "M"; "E"; "L"; "I"; "E" |] *)
  (* It's not safe to index into the original string because ç and ñ are multibyte *)
  let glyphs_ll = Array.to_list glyphs in
  let glyphs_rev = lazy (Array.fold glyphs ~init:[] ~f:(fun acc x -> x :: acc)) in
  let get_at = get_at glyphs in
  let ( =* ) = ( =* ) ~glyphs in
  let is_obvious_germanic = lazy (obvious_germanic glyphs_ll) in
  let primary = Buffer.create (max_length + 2) in
  let secondary = Buffer.create (max_length + 2) in
  let add p s =
    Buffer.add_char primary p;
    Buffer.add_char secondary s
  in
  let add_both c = add c c in

  let rec loop i ((pair, _, _, is_slavo_germanic) as pattern) =
    let advance =
      match pattern with
      | ("A" :: _, []), _, _, _
       |("E" :: _, []), _, _, _
       |("I" :: _, []), _, _, _
       |("O" :: _, []), _, _, _
       |("U" :: _, []), _, _, _
       |("Y" :: _, []), _, _, _ ->
        add_both 'A';
        debug [%here];
        1
      | ("A" :: _, _), _, _, _
       |("E" :: _, _), _, _, _
       |("I" :: _, _), _, _, _
       |("O" :: _, _), _, _, _
       |("U" :: _, _), _, _, _
       |("Y" :: _, _), _, _, _ ->
        debug [%here];
        1
      | ("B" :: "B" :: _, _), _, _, _ ->
        add_both 'P';
        debug [%here];
        2
      | ("B" :: _, _), _, _, _ ->
        (* L.290 "-mb", e.g "dumb", already skipped over... *)
        add_both 'P';
        debug [%here];
        1
      | ("ç" :: _, _), _, _, _
       |("Ç" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        1
      | ("C" :: "H" :: _, "A" :: prev :: _), _, _, _
        when (not (is_vowel prev))
             && get_at (i + 2) <>* "I"
             && (get_at (i + 2) <>* "E" || ((i - 2 =* "B" || i - 2 =* "M") && i + 3 =* "R")) ->
        (* L. 307 various germanic *)
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "A" :: "E" :: "S" :: "A" :: "R" :: _, []), _, _, _ ->
        (* L.322 special case 'caesar' *)
        add_both 'S';
        debug [%here];
        2
      | ("C" :: "H" :: "A" :: "E" :: _, _ :: _), _, _, _ ->
        (* L.343 find 'michael' *)
        add 'K' 'X';
        debug [%here];
        2
      | ("C" :: "H" :: "O" :: "R" :: "E" :: _, []), _, _, _ ->
        add_both 'X';
        debug [%here];
        2
      | ("C" :: "H" :: "A" :: "R" :: "A" :: "C" :: _, []), _, _, _
       |("C" :: "H" :: "A" :: "R" :: "I" :: "S" :: _, []), _, _, _
       |("C" :: "H" :: "O" :: "R" :: _, []), _, _, _
       |("C" :: "H" :: "Y" :: "M" :: _, []), _, _, _
       |("C" :: "H" :: "E" :: "M" :: _, []), _, _, _
       |("C" :: "H" :: "I" :: "A" :: _, _), _, _, _ ->
        (* L.332 italian 'chianti' *)
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "H" :: "E" :: "S" :: _, "R" :: "O" :: _), _, _, _
       |("C" :: "H" :: "I" :: "T" :: _, "R" :: "A" :: _), _, _, _
       |("C" :: "H" :: "I" :: "D" :: _, "R" :: "O" :: _), _, _, _
       |("C" :: "H" :: "T" :: _, _), _, _, _
       |("C" :: "H" :: "S" :: _, _), _, _, _ ->
        (* L.367 germanic, greek, or otherwise 'ch' for 'kh' sound *)
        (* L.371 'architect but not 'arch', 'orchestra', 'orchid' *)
        (* L.381 e.g., 'wachtler', 'wechsler', but not 'tichner' *)
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "H" :: "L" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "L" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "L" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "L" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "L" :: _, []), _, _, _
       |("C" :: "H" :: "R" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "R" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "R" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "R" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "R" :: _, []), _, _, _
       |("C" :: "H" :: "N" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "N" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "N" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "N" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "N" :: _, []), _, _, _
       |("C" :: "H" :: "M" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "M" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "M" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "M" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "M" :: _, []), _, _, _
       |("C" :: "H" :: "B" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "B" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "B" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "B" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "B" :: _, []), _, _, _
       |("C" :: "H" :: "H" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "H" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "H" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "H" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "H" :: _, []), _, _, _
       |("C" :: "H" :: "F" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "F" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "F" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "F" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "F" :: _, []), _, _, _
       |("C" :: "H" :: "V" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "V" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "V" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "V" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "V" :: _, []), _, _, _
       |("C" :: "H" :: "W" :: _, "A" :: _), _, _, _
       |("C" :: "H" :: "W" :: _, "O" :: _), _, _, _
       |("C" :: "H" :: "W" :: _, "U" :: _), _, _, _
       |("C" :: "H" :: "W" :: _, "E" :: _), _, _, _
       |("C" :: "H" :: "W" :: _, []), _, _, _
       |("C" :: "H" :: " " :: _, "A" :: _), _, _, _
       |("C" :: "H" :: " " :: _, "O" :: _), _, _, _
       |("C" :: "H" :: " " :: _, "U" :: _), _, _, _
       |("C" :: "H" :: " " :: _, "E" :: _), _, _, _
       |("C" :: "H" :: " " :: _, []), _, _, _
       |([ "C"; "H" ], "A" :: _), _, _, _
       |([ "C"; "H" ], "O" :: _), _, _, _
       |([ "C"; "H" ], "U" :: _), _, _, _
       |([ "C"; "H" ], "E" :: _), _, _, _
       |([ "C"; "H" ], []), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "H" :: _, _), "V" :: "A" :: "N" :: " " :: _, _, _
       |("C" :: "H" :: _, _), "V" :: "O" :: "N" :: " " :: _, _, _
       |("C" :: "H" :: _, _), "S" :: "C" :: "H" :: _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "H" :: _, _ :: _), "M" :: "C" :: _, _, _ ->
        (* L.396 e.g., "McHugh" *)
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "H" :: _, []), _, _, _ ->
        add_both 'X';
        debug [%here];
        2
      | ("C" :: "H" :: _, _ :: _), _, _, _ ->
        add 'X' 'K';
        debug [%here];
        2
      | ("C" :: "Z" :: _, "I" :: "W" :: _), _, _, _ ->
        add_both 'K';
        debug [%here];
        1
      | ("C" :: "Z" :: _, _), _, _, _ ->
        (* L.415  e.g, 'czerny' *)
        add 'S' 'X';
        debug [%here];
        2
      | ("C" :: "C" :: "I" :: "A" :: _, _), _, _, _ ->
        (* L.425 e.g., 'focaccia' *)
        add_both 'X';
        debug [%here];
        3
      | ("C" :: "C" :: "E" :: _, [ "M" ]), _, _, _
       |("C" :: "C" :: "I" :: _, [ "M" ]), _, _, _ ->
        add_both 'K';
        debug [%here];
        1
      | ("C" :: "C" :: _, [ "M" ]), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "C" :: "H" :: "U" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "C" :: "I" :: _, [ "A" ]), _, _, _
       |("C" :: "C" :: "E" :: _, [ "A" ]), _, _, _
       |("C" :: "C" :: "H" :: _, [ "A" ]), _, _, _
       |("C" :: "C" :: "E" :: "E" :: _, "U" :: _), _, _, _
       |("C" :: "C" :: "E" :: "S" :: _, "U" :: _), _, _, _ ->
        (* L.438 'llbeglyphsocchio' but not 'bacchus' *)
        (* L.442 'accident', 'accede' 'succeed' *)
        add_both 'K';
        add_both 'S';
        debug [%here];
        3
      | ("C" :: "C" :: "I" :: _, _), _, _, _
       |("C" :: "C" :: "E" :: _, _), _, _, _
       |("C" :: "C" :: "H" :: _, _), _, _, _ ->
        (* L.451 'bacci', 'bertucci', other italian *)
        add_both 'X';
        debug [%here];
        3
      | ("C" :: "C" :: _, _), _, _, _ ->
        (* L.462 Pierce's rule *)
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "K" :: _, _), _, _, _
       |("C" :: "G" :: _, _), _, _, _
       |("C" :: "Q" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("C" :: "I" :: "O" :: _, _), _, _, _
       |("C" :: "I" :: "E" :: _, _), _, _, _
       |("C" :: "I" :: "A" :: _, _), _, _, _ ->
        (* L.480 italian vs. english *)
        add 'S' 'X';
        debug [%here];
        2
      | ("C" :: "I" :: _, _), _, _, _
       |("C" :: "E" :: _, _), _, _, _
       |("C" :: "Y" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        2
      | ("C" :: " " :: "C" :: _, _), _, _, _
       |("C" :: " " :: "Q" :: _, _), _, _, _
       |("C" :: " " :: "G" :: _, _), _, _, _ ->
        (* L.500 nam, _e sent in 'mac caffrey', 'mac gregor' *)
        add_both 'K';
        debug [%here];
        3
      | ("C" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        1
      | ("D" :: "G" :: "I" :: _, _), _, _, _
       |("D" :: "G" :: "E" :: _, _), _, _, _
       |("D" :: "G" :: "Y" :: _, _), _, _, _ ->
        (* L.517 'edge' *)
        add_both 'J';
        debug [%here];
        3
      | ("D" :: "G" :: _, _), _, _, _ ->
        (* L.525 'edgar' *)
        add_both 'T';
        add_both 'K';
        debug [%here];
        2
      | ("D" :: "T" :: _, _), _, _, _
       |("D" :: "D" :: _, _), _, _, _ ->
        add_both 'T';
        debug [%here];
        2
      | ("D" :: _, _), _, _, _ ->
        add_both 'T';
        debug [%here];
        1
      | ("F" :: "F" :: _, _), _, _, _ ->
        add_both 'F';
        debug [%here];
        2
      | ("F" :: _, _), _, _, _ ->
        add_both 'F';
        debug [%here];
        1
      | ("G" :: "H" :: "I" :: _, []), _, _, _ ->
        add_both 'J';
        debug [%here];
        2
      | ("G" :: "H" :: _, []), _, _, _ ->
        (* L.569 'ghislane', ghiradelli *)
        add_both 'K';
        debug [%here];
        2
      | ("G" :: "H" :: _, "I" :: _), _, _, _ ->
        debug [%here];
        2
      | ("G" :: "H" :: _, "A" :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "A" :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "A" :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "A" :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "A" :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "A" :: _ :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "A" :: _ :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "A" :: _ :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: _ :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: _ :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: _ :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: _ :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: _ :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: _ :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: _ :: "H" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: _ :: "D" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: _ :: _ :: "B" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: _ :: _ :: "H" :: _), _, _, _ ->
        (* L.588 Parker's rule (with some further refinements) - e.g., 'hugh' *)
        debug [%here];
        2
      | ("G" :: "H" :: _, "U" :: _ :: "C" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: "G" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: "L" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: "R" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _ :: "T" :: _), _, _, _ ->
        (* L.610  e.g., 'laugh', 'McLaughlin', 'cough', 'gough', * 'rough', 'tough' *)
        add_both 'F';
        debug [%here];
        2
      | ("G" :: "H" :: _, "A" :: _), _, _, _
       |("G" :: "H" :: _, "E" :: _), _, _, _
       |("G" :: "H" :: _, "O" :: _), _, _, _
       |("G" :: "H" :: _, "U" :: _), _, _, _
       |("G" :: "H" :: _, "Y" :: _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("G" :: "H" :: _, _ :: _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("G" :: "N" :: _, [ "A" ]), _, _, false
       |("G" :: "N" :: _, [ "E" ]), _, _, false
       |("G" :: "N" :: _, [ "I" ]), _, _, false
       |("G" :: "N" :: _, [ "O" ]), _, _, false
       |("G" :: "N" :: _, [ "U" ]), _, _, false
       |("G" :: "N" :: _, [ "Y" ]), _, _, false ->
        Buffer.add_char primary 'K';
        add_both 'N';
        debug [%here];
        2
      | ("G" :: "N" :: "E" :: "Y" :: _, _), _, _, _ ->
        add_both 'K';
        add_both 'N';
        debug [%here];
        2
      | ("G" :: "N" :: _, _), _, _, true ->
        add_both 'K';
        add_both 'N';
        debug [%here];
        2
      | ("G" :: "N" :: _, _), _, _, _ ->
        (* L.644 not e.g. 'cagney' *)
        Buffer.add_char secondary 'K';
        add_both 'N';
        debug [%here];
        2
      | ("G" :: "L" :: "I" :: _, _), _, _, false ->
        (* L.661 'tagliaro' *)
        Buffer.add_char primary 'K';
        add_both 'L';
        debug [%here];
        2
      | ("G" :: "Y" :: _, []), _, _, _
       |("G" :: "E" :: "S" :: _, []), _, _, _
       |("G" :: "E" :: "P" :: _, []), _, _, _
       |("G" :: "E" :: "B" :: _, []), _, _, _
       |("G" :: "E" :: "L" :: _, []), _, _, _
       |("G" :: "E" :: "Y" :: _, []), _, _, _
       |("G" :: "I" :: "B" :: _, []), _, _, _
       |("G" :: "I" :: "L" :: _, []), _, _, _
       |("G" :: "I" :: "N" :: _, []), _, _, _
       |("G" :: "I" :: "E" :: _, []), _, _, _
       |("G" :: "E" :: "I" :: _, []), _, _, _
       |("G" :: "E" :: "R" :: _, []), _, _, _ ->
        (* L.671 -ges-,-gep-,-gel-, -gie- at beginning *)
        add 'K' 'J';
        debug [%here];
        2
      | ("G" :: "E" :: "R" :: _, _), "D" :: "A" :: "N" :: "G" :: "E" :: "R" :: _, _, _
       |("G" :: "E" :: "R" :: _, _), "R" :: "A" :: "N" :: "G" :: "E" :: "R" :: _, _, _
       |("G" :: "E" :: "R" :: _, _), "M" :: "A" :: "N" :: "G" :: "E" :: "R" :: _, _, _
       |("G" :: "E" :: "R" :: _, "E" :: _), _, _, _
       |("G" :: "E" :: "R" :: _, "I" :: _), _, _, _ ->
        add (if force is_obvious_germanic then 'K' else 'J') 'K';
        debug [%here];
        2
      | ("G" :: "E" :: "R" :: _, _), _, _, _ ->
        (* L.684 -ger-,  -gy- *)
        add 'K' 'J';
        debug [%here];
        2
      | ("G" :: "Y" :: _, _), "D" :: "A" :: "N" :: "G" :: "E" :: "R" :: _, _, _
       |("G" :: "Y" :: _, _), "R" :: "A" :: "N" :: "G" :: "E" :: "R" :: _, _, _
       |("G" :: "Y" :: _, _), "M" :: "A" :: "N" :: "G" :: "E" :: "R" :: _, _, _
       |("G" :: "Y" :: _, "E" :: _), _, _, _
       |("G" :: "Y" :: _, "I" :: _), _, _, _
       |("G" :: "Y" :: _, "R" :: _), _, _, _
       |("G" :: "Y" :: _, "O" :: _), _, _, _ ->
        add (if force is_obvious_germanic then 'K' else 'J') 'K';
        debug [%here];
        2
      | ("G" :: "Y" :: _, _), _, _, _ ->
        (* L.684 -ger-,  -gy- *)
        add 'K' 'J';
        debug [%here];
        2
      | ("G" :: "E" :: "T" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ([ "G"; "I"; "E"; "R" ], _), _, _, _
       |("G" :: "I" :: "E" :: "R" :: " " :: _, _), _, _, _ ->
        (* L.716 always soft if french ending *)
        add_both 'J';
        debug [%here];
        2
      | ("G" :: "E" :: _, _), _, _, _
       |("G" :: "I" :: _, _), _, _, _
       |("G" :: "G" :: "I" :: _, "A" :: _), _, _, _
       |("G" :: "G" :: "I" :: _, "O" :: _), _, _, _ ->
        (* L.700 italian e.g, 'biaggi' *)
        add (if force is_obvious_germanic then 'K' else 'J') 'K';
        debug [%here];
        2
      | ("G" :: "G" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("G" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        1
      | ("H" :: "A" :: _, []), _, _, _
       |("H" :: "E" :: _, []), _, _, _
       |("H" :: "I" :: _, []), _, _, _
       |("H" :: "O" :: _, []), _, _, _
       |("H" :: "U" :: _, []), _, _, _
       |("H" :: "Y" :: _, []), _, _, _
       |("H" :: "A" :: _, "A" :: _), _, _, _
       |("H" :: "E" :: _, "A" :: _), _, _, _
       |("H" :: "I" :: _, "A" :: _), _, _, _
       |("H" :: "O" :: _, "A" :: _), _, _, _
       |("H" :: "U" :: _, "A" :: _), _, _, _
       |("H" :: "Y" :: _, "A" :: _), _, _, _
       |("H" :: "A" :: _, "E" :: _), _, _, _
       |("H" :: "E" :: _, "E" :: _), _, _, _
       |("H" :: "I" :: _, "E" :: _), _, _, _
       |("H" :: "O" :: _, "E" :: _), _, _, _
       |("H" :: "U" :: _, "E" :: _), _, _, _
       |("H" :: "Y" :: _, "E" :: _), _, _, _
       |("H" :: "A" :: _, "I" :: _), _, _, _
       |("H" :: "E" :: _, "I" :: _), _, _, _
       |("H" :: "I" :: _, "I" :: _), _, _, _
       |("H" :: "O" :: _, "I" :: _), _, _, _
       |("H" :: "U" :: _, "I" :: _), _, _, _
       |("H" :: "Y" :: _, "I" :: _), _, _, _
       |("H" :: "A" :: _, "O" :: _), _, _, _
       |("H" :: "E" :: _, "O" :: _), _, _, _
       |("H" :: "I" :: _, "O" :: _), _, _, _
       |("H" :: "O" :: _, "O" :: _), _, _, _
       |("H" :: "U" :: _, "O" :: _), _, _, _
       |("H" :: "Y" :: _, "O" :: _), _, _, _
       |("H" :: "A" :: _, "U" :: _), _, _, _
       |("H" :: "E" :: _, "U" :: _), _, _, _
       |("H" :: "I" :: _, "U" :: _), _, _, _
       |("H" :: "O" :: _, "U" :: _), _, _, _
       |("H" :: "U" :: _, "U" :: _), _, _, _
       |("H" :: "Y" :: _, "U" :: _), _, _, _
       |("H" :: "A" :: _, "Y" :: _), _, _, _
       |("H" :: "E" :: _, "Y" :: _), _, _, _
       |("H" :: "I" :: _, "Y" :: _), _, _, _
       |("H" :: "O" :: _, "Y" :: _), _, _, _
       |("H" :: "U" :: _, "Y" :: _), _, _, _
       |("H" :: "Y" :: _, "Y" :: _), _, _, _ ->
        (* L.742 only keep if first & before vowel or btw. 2 vowels *)
        add_both 'H';
        debug [%here];
        2
      | ("H" :: _, _), _, _, _ ->
        (* L.751 also takes care of 'HH' *)
        debug [%here];
        1
      | ("J" :: _, _), "S" :: "A" :: "N" :: " " :: _, _, _ ->
        (* L.756 obvious spanish, 'jose', 'san jacinto' *)
        add_both 'H';
        debug [%here];
        1
      | ([ "J"; "O"; "S"; "E" ], []), _, _, _
       |("J" :: "O" :: "S" :: "E" :: " " :: _, []), _, _, _ ->
        (* L.756 obvious spanish, 'jose', 'san jacinto' *)
        add_both 'H';
        debug [%here];
        1
      | ("J" :: "O" :: "S" :: "E" :: _, _), _, _, _ ->
        add 'J' 'H';
        debug [%here];
        1
      | ("J" :: "J" :: _, []), _, _, _ ->
        add 'J' 'A';
        debug [%here];
        2
      | ("J" :: _, []), _, _, _ ->
        (* L.779 Yankelovich/Jankelowicz *)
        add 'J' 'A';
        debug [%here];
        1
      | ([ "J" ], _), _, _, _ ->
        Buffer.add_char primary 'J';
        debug [%here];
        1
      | ("J" :: "A" :: _, "A" :: _), _, _, false
       |("J" :: "A" :: _, "E" :: _), _, _, false
       |("J" :: "A" :: _, "I" :: _), _, _, false
       |("J" :: "A" :: _, "O" :: _), _, _, false
       |("J" :: "A" :: _, "U" :: _), _, _, false
       |("J" :: "A" :: _, "Y" :: _), _, _, false
       |("J" :: "O" :: _, "A" :: _), _, _, false
       |("J" :: "O" :: _, "E" :: _), _, _, false
       |("J" :: "O" :: _, "I" :: _), _, _, false
       |("J" :: "O" :: _, "O" :: _), _, _, false
       |("J" :: "O" :: _, "U" :: _), _, _, false
       |("J" :: "O" :: _, "Y" :: _), _, _, false ->
        (* L.784 spanish pron. of e.g. 'bajador' *)
        add 'J' 'H';
        debug [%here];
        1
      | ("J" :: "J" :: _, "S" :: _), _, _, _
       |("J" :: "J" :: _, "K" :: _), _, _, _
       |("J" :: "J" :: _, "L" :: _), _, _, _ ->
        debug [%here];
        2
      | ("J" :: "L" :: _, _), _, _, _
       |("J" :: "T" :: _, _), _, _, _
       |("J" :: "K" :: _, _), _, _, _
       |("J" :: "S" :: _, _), _, _, _
       |("J" :: "N" :: _, _), _, _, _
       |("J" :: "M" :: _, _), _, _, _
       |("J" :: "B" :: _, _), _, _, _
       |("J" :: "Z" :: _, _), _, _, _
       |("J" :: _, "S" :: _), _, _, _
       |("J" :: _, "K" :: _), _, _, _
       |("J" :: _, "L" :: _), _, _, _ ->
        debug [%here];
        1
      | ("J" :: "J" :: _, _), _, _, _ ->
        add_both 'J';
        debug [%here];
        2
      | ("J" :: _, _), _, _, _ ->
        add_both 'J';
        debug [%here];
        1
      | ("K" :: "K" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("K" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        1
      | ([ "L"; "L"; "O" ], "I" :: _), _, _, _
       |([ "L"; "L"; "A" ], "I" :: _), _, _, _
       |([ "L"; "L"; "E" ], "A" :: _), _, _, _ ->
        (* L.832 spanish e.g. 'llcabriglyphso', 'llgaglyphsegos' *)
        Buffer.add_char primary 'L';
        debug [%here];
        2
      | ("L" :: "L" :: "E" :: _, "A" :: _), _, "S" :: "A" :: _, _
       |("L" :: "L" :: "E" :: _, "A" :: _), _, "S" :: "O" :: _, _
       |("L" :: "L" :: "E" :: _, "A" :: _), _, "A" :: _, _
       |("L" :: "L" :: "E" :: _, "A" :: _), _, "O" :: _, _ ->
        (* L.832 spanish e.g. 'llcabriglyphso', 'llgaglyphsegos' *)
        Buffer.add_char primary 'L';
        debug [%here];
        2
      | ("L" :: "L" :: _, _), _, _, _ ->
        add_both 'L';
        debug [%here];
        2
      | ("L" :: _, _), _, _, _ ->
        add_both 'L';
        debug [%here];
        1
      | ([ "M"; "B" ], "U" :: _), _, _, _
       |("M" :: "B" :: "E" :: "R" :: _, "U" :: _), _, _, _
       |("M" :: "M" :: _, _), _, _, _ ->
        add_both 'M';
        debug [%here];
        2
      | ("M" :: _, _), _, _, _ ->
        add_both 'M';
        debug [%here];
        1
      | ("N" :: "N" :: _, _), _, _, _ ->
        add_both 'N';
        debug [%here];
        2
      | ("N" :: _, _), _, _, _ ->
        add_both 'N';
        debug [%here];
        1
      | ("ñ" :: _, _), _, _, _
       |("Ñ" :: _, _), _, _, _ ->
        add_both 'N';
        debug [%here];
        1
      | ("P" :: "H" :: _, _), _, _, _ ->
        add_both 'F';
        debug [%here];
        2
      | ("P" :: "P" :: _, _), _, _, _
       |("P" :: "B" :: _, _), _, _, _ ->
        (* L.891 also account for "llcampbeglyphs", "raspberry" *)
        add_both 'P';
        debug [%here];
        2
      | ("P" :: _, _), _, _, _ ->
        add_both 'P';
        debug [%here];
        1
      | ("Q" :: "Q" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        2
      | ("Q" :: _, _), _, _, _ ->
        add_both 'K';
        debug [%here];
        1
      | ([ "R" ], "E" :: "I" :: "E" :: "M" :: _), _, _, _
       |([ "R" ], "E" :: "I" :: "A" :: "M" :: _), _, _, _ ->
        add_both 'R';
        debug [%here];
        1
      | ([ "R" ], "E" :: "I" :: _), _, _, false ->
        (* L.910 french e.g. 'rogier', but exclude 'hochmeier' *)
        Buffer.add_char secondary 'R';
        debug [%here];
        1
      | ("R" :: "R" :: _, _), _, _, _ ->
        add_both 'R';
        debug [%here];
        2
      | ("R" :: _, _), _, _, _ ->
        add_both 'R';
        debug [%here];
        1
      | ("S" :: "L" :: _, "I" :: _), _, _, _
       |("S" :: "L" :: _, "Y" :: _), _, _, _ ->
        (* L.932 special cases 'island', 'isle', 'carlisle', 'carlysle' *)
        debug [%here];
        1
      | ("S" :: "U" :: "G" :: "A" :: "R" :: _, []), _, _, _ ->
        (* L.939 special case 'sugar-' *)
        add 'X' 'S';
        debug [%here];
        1
      | ("S" :: "H" :: "E" :: "I" :: "M" :: _, _), _, _, _
       |("S" :: "H" :: "O" :: "E" :: "K" :: _, _), _, _, _
       |("S" :: "H" :: "O" :: "L" :: "M" :: _, _), _, _, _
       |("S" :: "H" :: "O" :: "L" :: "Z" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        2
      | ("S" :: "H" :: _, _), _, _, _ ->
        (* L.951 germanic *)
        add_both 'X';
        debug [%here];
        2
      | ("S" :: "I" :: "O" :: _, _), _, _, false
       |("S" :: "I" :: "A" :: _, _), _, _, false ->
        (* L.968 italian & armenian *)
        add 'S' 'X';
        debug [%here];
        3
      | ("S" :: "I" :: "O" :: _, _), _, _, true
       |("S" :: "I" :: "A" :: _, _), _, _, true ->
        (* L.968 italian & armenian *)
        add_both 'S';
        debug [%here];
        3
      | ("S" :: "M" :: _, []), _, _, _
       |("S" :: "N" :: _, []), _, _, _
       |("S" :: "L" :: _, []), _, _, _
       |("S" :: "W" :: _, []), _, _, _ ->
        (* L.987 german & anglicisations, e.g. 'smith' match 'schmidt',
           'snider' match 'schneider' also, -sz- in slavic language
           although in hungarian it is pronounced 's' *)
        add 'S' 'X';
        debug [%here];
        1
      | ("S" :: "Z" :: _, _), _, _, _ ->
        add 'S' 'X';
        debug [%here];
        2
      | ("S" :: "C" :: "H" :: "E" :: "R" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "E" :: "N" :: _, _), _, _, _ ->
        (* L.1015 'schermerhorn', 'schenker' *)
        Buffer.add_char primary 'X';
        Buffer.add_string secondary "SK";
        debug [%here];
        3
      | ("S" :: "C" :: "H" :: "O" :: "O" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "U" :: "Y" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "E" :: "D" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "E" :: "M" :: _, _), _, _, _ ->
        (* L.1007 Schlesinger's rule *)
        (* L.1010 dutch origin, e.g. 'school', 'schooner' *)
        add_both 'S';
        add_both 'K';
        debug [%here];
        3
      | ("S" :: "C" :: "H" :: _, _ :: _), _, _, _
       |("S" :: "C" :: "H" :: "A" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "E" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "I" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "O" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "U" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "Y" :: _, _), _, _, _
       |("S" :: "C" :: "H" :: "W" :: _, _), _, _, _ ->
        add_both 'X';
        debug [%here];
        3
      | ("S" :: "C" :: "H" :: _, _), _, _, _ ->
        add 'X' 'S';
        debug [%here];
        3
      | ("S" :: "C" :: "I" :: _, _), _, _, _
       |("S" :: "C" :: "E" :: _, _), _, _, _
       |("S" :: "C" :: "Y" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        3
      | ("S" :: "C" :: _, _), _, _, _ ->
        add_both 'S';
        add_both 'K';
        debug [%here];
        3
      | ([ "S" ], "I" :: "A" :: _), _, _, _
       |([ "S" ], "I" :: "O" :: _), _, _, _ ->
        (* L.1063 french e.g. 'resnais', 'artois' *)
        Buffer.add_char secondary 'S';
        debug [%here];
        1
      | ("S" :: "S" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        2
      | ("S" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        1
      | ("T" :: "I" :: "O" :: "N" :: _, _), _, _, _
       |("T" :: "I" :: "A" :: _, _), _, _, _
       |("T" :: "C" :: "H" :: _, _), _, _, _ ->
        add_both 'X';
        debug [%here];
        3
      | ("T" :: "H" :: "O" :: "M" :: _, _), _, _, _
       |("T" :: "H" :: "A" :: "M" :: _, _), _, _, _ ->
        (* L.1102 special case 'thomas', 'thames' or germanic *)
        add_both 'T';
        debug [%here];
        2
      | ("T" :: "H" :: _, _), _, _, _
       |("T" :: "T" :: "H" :: _, _), _, _, _ ->
        add (if force is_obvious_germanic then 'T' else '0') 'T';
        debug [%here];
        2
      | ("T" :: "T" :: _, _), _, _, _
       |("T" :: "D" :: _, _), _, _, _ ->
        add_both 'T';
        debug [%here];
        2
      | ("T" :: _, _), _, _, _ ->
        add_both 'T';
        debug [%here];
        1
      | ("V" :: "V" :: _, _), _, _, _ ->
        add_both 'F';
        debug [%here];
        2
      | ("V" :: _, _), _, _, _ ->
        add_both 'F';
        debug [%here];
        1
      | ("W" :: "R" :: _, _), _, _, _ ->
        (* L.1137 can also be in middle of word *)
        add_both 'R';
        debug [%here];
        2
      | ([ "W" ], "A" :: _), _, _, _
       |([ "W" ], "E" :: _), _, _, _
       |([ "W" ], "I" :: _), _, _, _
       |([ "W" ], "O" :: _), _, _, _
       |([ "W" ], "U" :: _), _, _, _
       |([ "W" ], "Y" :: _), _, _, _ ->
        Buffer.add_char secondary 'F';
        debug [%here];
        1
      | ("W" :: "S" :: "K" :: "I" :: _, "E" :: _), _, _, _
       |("W" :: "S" :: "K" :: "Y" :: _, "E" :: _), _, _, _
       |("W" :: "S" :: "K" :: "I" :: _, "O" :: _), _, _, _
       |("W" :: "S" :: "K" :: "Y" :: _, "O" :: _), _, _, _
       |("W" :: _, _), "S" :: "C" :: "H" :: _, _, _ ->
        (* L.1164 Arnow should match Arnoff *)
        Buffer.add_char secondary 'F';
        debug [%here];
        1
      | ("W" :: "I" :: "C" :: "Z" :: _, _), _, _, _
       |("W" :: "I" :: "T" :: "Z" :: _, _), _, _, _ ->
        (* L.1176 polish e.g. 'filipowicz' *)
        Buffer.add_string primary "TS";
        Buffer.add_string secondary "FX";
        debug [%here];
        4
      | ("W" :: _, _), _, _, _ ->
        (* L.1185 else skip it *)
        debug [%here];
        1
      | ([ "X" ], "U" :: "O" :: _), _, _, _
       |([ "X" ], "U" :: "A" :: _), _, _, _ ->
        debug [%here];
        1
      | ("X" :: "C" :: _, _), _, _, _
       |("X" :: "X" :: _, _), _, _, _ ->
        (* L.1190 french e.g. breaux *)
        Buffer.add_string primary "KS";
        Buffer.add_string secondary "KS";
        debug [%here];
        2
      | ("X" :: _, _), _, _, _ ->
        Buffer.add_string primary "KS";
        Buffer.add_string secondary "KS";
        debug [%here];
        1
      | ("Z" :: "H" :: _, _), _, _, _ ->
        (* L.1209 chinese pinyin e.g. 'zhao' *)
        add_both 'J';
        debug [%here];
        2
      | ("Z" :: "Z" :: "O" :: _, _), _, _, _
       |("Z" :: "Z" :: "I" :: _, _), _, _, _
       |("Z" :: "Z" :: "A" :: _, _), _, _, _ ->
        Buffer.add_char secondary 'T';
        add_both 'S';
        debug [%here];
        2
      | ("Z" :: "Z" :: _, "T" :: _), _, _, _ ->
        add_both 'S';
        debug [%here];
        2
      | ("Z" :: _, "T" :: _), _, _, _ ->
        add_both 'S';
        debug [%here];
        1
      | ("Z" :: "Z" :: _, _ :: _), _, _, true ->
        Buffer.add_char secondary 'T';
        add_both 'S';
        debug [%here];
        2
      | ("Z" :: _, _ :: _), _, _, true ->
        Buffer.add_char secondary 'T';
        add_both 'S';
        debug [%here];
        1
      | ("Z" :: "Z" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        2
      | ("Z" :: _, _), _, _, _ ->
        add_both 'S';
        debug [%here];
        1
      | (_ :: _, _), _, _, _ ->
        debug [%here];
        1
      | ([], _), _, _, _ -> 0
    in
    let next_pos = i + advance in
    match
      (Buffer.length primary < max_length || Buffer.length secondary < max_length)
      && next_pos < Array.length glyphs
    with
    | true ->
      let pair = prepare ~n:advance pair in
      loop next_pos (pair, glyphs_ll, force glyphs_rev, is_slavo_germanic)
    | false -> ()
  in

  match original with
  | "" -> "", ""
  | _ ->
    let start_pos =
      match glyphs_ll with
      | "G" :: "N" :: _
       |"K" :: "N" :: _
       |"P" :: "N" :: _
       |"W" :: "R" :: _
       |"P" :: "S" :: _ ->
        (* L.253 skip these when at start of word *)
        debug [%here];
        1
      | "X" :: _ ->
        (* L.257 Initial 'X' is pronounced 'Z' e.g. Xavier *)
        add_both 'S';
        debug [%here];
        1
      | "W" :: "A" :: _
       |"W" :: "E" :: _
       |"W" :: "I" :: _
       |"W" :: "O" :: _
       |"W" :: "U" :: _
       |"W" :: "Y" :: _ ->
        (* L.1150 Wasserman should match Vasserman *)
        add 'A' 'F';
        0
      | "W" :: "H" :: _ ->
        (* L.1158 need Uomo to match Womo *)
        add_both 'A';
        0
      | _ -> 0
    in
    let pair = prepare ~n:start_pos (glyphs_ll, []) in
    (pair, glyphs_ll, force glyphs_rev, slavo_germanic original) |> loop start_pos;
    let finalize b = Buffer.To_string.sub b ~pos:0 ~len:(min max_length (Buffer.length b)) in
    finalize primary, finalize secondary
