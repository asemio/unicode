open! Core_kernel

let preserve = function
| "ç"
 |"Ç"
 |"ñ"
 |"Ñ" ->
  true
| _ -> false

let ( =* ) = String.( = )

let ( <>* ) = String.( <> )

(** One of these glyphs *)
let ( =*| ) ~get_at pos arr = Array.mem arr (get_at pos) ~equal:[%equal: string]

(** Not one of these glyphs *)
let ( <>*| ) ~get_at pos arr = not (( =*| ) ~get_at pos arr)

(** Matches this chain of glyphs *)
let ( =*> ) ~glyphs =
  let len = Array.length glyphs in
  fun pos against ->
    pos >= 0
    &&
    let len2 = Array.length against in
    if len2 + pos > len
    then false
    else (
      let rec loop = function
        | -1 -> true
        | i when Array.get glyphs (pos + i) =* Array.get against i -> loop (i - 1)
        | _ -> false
      in
      loop (len2 - 1)
    )

let get_at glyphs =
  let len = Array.length glyphs in
  (fun pos -> if pos < 0 || pos >= len then " " else Array.get glyphs pos)

let string_at ~glyphs =
  let len = Array.length glyphs in
  fun pos arr ->
    pos >= 0
    && Array.exists arr ~f:(fun against ->
         let len2 = Array.length against in
         if len2 + pos > len
         then false
         else (
           let rec loop = function
             | -1 -> true
             | i when Array.get glyphs (pos + i) =* Array.get against i -> loop (i - 1)
             | _ -> false
           in
           loop (len2 - 1)
         )
       )

let is_vowel pos = pos =*| [| "A"; "E"; "I"; "O"; "U"; "Y" |]

let slavo_germanic s =
  String.is_substring s ~substring:"W"
  || String.is_substring s ~substring:"K"
  || String.is_substring s ~substring:"CZ"
  || String.is_substring s ~substring:"WITZ"

let obvious_germanic ~string_at =
  string_at 0 [| [| "V"; "A"; "N"; " " |]; [| "V"; "O"; "N"; " " |]; [| "S"; "C"; "H" |] |]

let double_metaphone ?(max_length = 4) ~standardized:original ~glyphs =
  let debug (here : Source_code_position.t) =
    if false then print_endline (sprintf "[%d]" here.pos_lnum)
  in
  (* original: AMELIE *)
  (* glyphs: [| "A"; "M"; "E"; "L"; "I"; "E" |] *)
  (* It's not safe to index into the original string because ç and ñ are multibyte *)
  let ll = Array.to_list glyphs in
  let get_at = get_at glyphs in
  let ( =*| ) = ( =*| ) ~get_at in
  let ( <>*| ) = ( <>*| ) ~get_at in
  let ( =*> ) = ( =*> ) ~glyphs in
  let is_vowel = is_vowel ~get_at in
  let string_at = string_at ~glyphs in
  let is_slavo_germanic = lazy (slavo_germanic original) in
  let obvious_germanic = lazy (obvious_germanic ~string_at) in
  let primary = Buffer.create (max_length + 2) in
  let secondary = Buffer.create (max_length + 2) in
  let current = ref 0 in
  let last = Array.length glyphs - 1 in
  let advance x = current := !current + x in
  let add p s =
    Buffer.add_char primary p;
    Buffer.add_char secondary s
  in
  let add_both c = add c c in

  (match ll with
  | "G" :: "N" :: _
   |"K" :: "N" :: _
   |"P" :: "N" :: _
   |"W" :: "R" :: _
   |"P" :: "S" :: _ ->
    (* L.253 skip these when at start of word *)
    debug [%here];
    advance 1
  | "X" :: _ ->
    (* L.257 Initial 'X' is pronounced 'Z' e.g. Xavier *)
    add_both 'S';
    debug [%here];
    advance 1
  | "W" :: "A" :: _
   |"W" :: "E" :: _
   |"W" :: "I" :: _
   |"W" :: "O" :: _
   |"W" :: "U" :: _
   |"W" :: "Y" :: _ ->
    (* L.1150 Wasserman should match Vasserman *)
    add 'A' 'F'
  | "W" :: "H" :: _ ->
    (* L.1158 need Uomo to match Womo *)
    add_both 'A'
  | _ -> ());

  let rec loop i triple =
    (match triple with
    | "A" :: _, [], _
     |"E" :: _, [], _
     |"I" :: _, [], _
     |"O" :: _, [], _
     |"U" :: _, [], _
     |"Y" :: _, [], _ ->
      add_both 'A';
      debug [%here];
      advance 1
    | "A" :: _, _, _
     |"E" :: _, _, _
     |"I" :: _, _, _
     |"O" :: _, _, _
     |"U" :: _, _, _
     |"Y" :: _, _, _ ->
      debug [%here];
      advance 1
    | "B" :: "B" :: _, _, _ ->
      add_both 'P';
      debug [%here];
      advance 2
    | "B" :: _, _, _ ->
      (* L.290 "-mb", e.g "dumb", already skipped over... *)
      add_both 'P';
      debug [%here];
      advance 1
    | "ç" :: _, _, _
     |"Ç" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 1
    | "C" :: "H" :: _, "A" :: _ :: _, _
      when (not (is_vowel (i - 2)))
           && get_at (i + 2) <>* "I"
           && (get_at (i + 2) <>* "E" || (i - 2 =*| [| "B"; "M" |] && i + 2 =*> [| "E"; "R" |])) ->
      (* L. 307 various germanic *)
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "A" :: "E" :: "S" :: "A" :: "R" :: _, [], _ ->
      (* L.322 special case 'caesar' *)
      add_both 'S';
      debug [%here];
      advance 2
    | "C" :: "H" :: "A" :: "E" :: _, _ :: _, _ ->
      (* L.343 find 'michael' *)
      add 'K' 'X';
      debug [%here];
      advance 2
    | "C" :: "H" :: "O" :: "R" :: "E" :: _, [], _ ->
      add_both 'X';
      debug [%here];
      advance 2
    | "C" :: "H" :: "A" :: "R" :: "A" :: "C" :: _, [], _
     |"C" :: "H" :: "A" :: "R" :: "I" :: "S" :: _, [], _
     |"C" :: "H" :: "O" :: "R" :: _, [], _
     |"C" :: "H" :: "Y" :: "M" :: _, [], _
     |"C" :: "H" :: "E" :: "M" :: _, [], _
     |"C" :: "H" :: "I" :: "A" :: _, _, _ ->
      (* L.332 italian 'chianti' *)
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "H" :: "E" :: "S" :: _, "R" :: "O" :: _, _
     |"C" :: "H" :: "I" :: "T" :: _, "R" :: "A" :: _, _
     |"C" :: "H" :: "I" :: "D" :: _, "R" :: "O" :: _, _
     |"C" :: "H" :: "T" :: _, _, _
     |"C" :: "H" :: "S" :: _, _, _ ->
      (* L.367 germanic, greek, or otherwise 'ch' for 'kh' sound *)
      (* L.371 'architect but not 'arch', 'orchestra', 'orchid' *)
      (* L.381 e.g., 'wachtler', 'wechsler', but not 'tichner' *)
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "H" :: "L" :: _, _, _
     |"C" :: "H" :: "R" :: _, _, _
     |"C" :: "H" :: "N" :: _, _, _
     |"C" :: "H" :: "M" :: _, _, _
     |"C" :: "H" :: "B" :: _, _, _
     |"C" :: "H" :: "H" :: _, _, _
     |"C" :: "H" :: "F" :: _, _, _
     |"C" :: "H" :: "V" :: _, _, _
     |"C" :: "H" :: "W" :: _, _, _
     |"C" :: "H" :: " " :: _, _, _
     |[ "C"; "H" ], _, _
      when i - 1 =*| [| "A"; "O"; "U"; "E" |] || i = 0 ->
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "H" :: _, _, _ when force obvious_germanic ->
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "H" :: _, _ :: _, "M" :: "C" :: _ ->
      (* L.396 e.g., "McHugh" *)
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "H" :: _, [], _ ->
      add_both 'X';
      debug [%here];
      advance 2
    | "C" :: "H" :: _, _ :: _, _ ->
      add 'X' 'K';
      debug [%here];
      advance 2
    | "C" :: "Z" :: _, "I" :: "W" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | "C" :: "Z" :: _, _, _ ->
      (* L.415  e.g, 'czerny' *)
      add 'S' 'X';
      debug [%here];
      advance 2
    | "C" :: "C" :: "I" :: "A" :: _, _, _ ->
      (* L.425 e.g., 'focaccia' *)
      add_both 'X';
      debug [%here];
      advance 3
    | "C" :: "C" :: "E" :: _, [ "M" ], _
     |"C" :: "C" :: "I" :: _, [ "M" ], _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | "C" :: "C" :: _, [ "M" ], _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "C" :: "H" :: "U" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "C" :: "I" :: _, [ "A" ], _
     |"C" :: "C" :: "E" :: _, [ "A" ], _
     |"C" :: "C" :: "H" :: _, [ "A" ], _
     |"C" :: "C" :: "E" :: "E" :: _, "U" :: _, _
     |"C" :: "C" :: "E" :: "S" :: _, "U" :: _, _ ->
      (* L.438 'bellocchio' but not 'bacchus' *)
      (* L.442 'accident', 'accede' 'succeed' *)
      add_both 'K';
      add_both 'S';
      debug [%here];
      advance 3
    | "C" :: "C" :: "I" :: _, _, _
     |"C" :: "C" :: "E" :: _, _, _
     |"C" :: "C" :: "H" :: _, _, _ ->
      (* L.451 'bacci', 'bertucci', other italian *)
      add_both 'X';
      debug [%here];
      advance 3
    | "C" :: "C" :: _, _, _ ->
      (* L.462 Pierce's rule *)
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "K" :: _, _, _
     |"C" :: "G" :: _, _, _
     |"C" :: "Q" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "I" :: "O" :: _, _, _
     |"C" :: "I" :: "E" :: _, _, _
     |"C" :: "I" :: "A" :: _, _, _ ->
      (* L.480 italian vs. english *)
      add 'S' 'X';
      debug [%here];
      advance 2
    | "C" :: "I" :: _, _, _
     |"C" :: "E" :: _, _, _
     |"C" :: "Y" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 2
    | "C" :: " " :: "C" :: _, _, _
     |"C" :: " " :: "Q" :: _, _, _
     |"C" :: " " :: "G" :: _, _, _ ->
      (* L.500 nam, _e sent in 'mac caffrey', 'mac gregor' *)
      add_both 'K';
      debug [%here];
      advance 3
    | "C" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | "D" :: "G" :: "I" :: _, _, _
     |"D" :: "G" :: "E" :: _, _, _
     |"D" :: "G" :: "Y" :: _, _, _ ->
      (* L.517 'edge' *)
      add_both 'J';
      debug [%here];
      advance 3
    | "D" :: "G" :: _, _, _ ->
      (* L.525 'edgar' *)
      add_both 'T';
      add_both 'K';
      debug [%here];
      advance 2
    | "D" :: "T" :: _, _, _
     |"D" :: "D" :: _, _, _ ->
      add_both 'T';
      debug [%here];
      advance 2
    | "D" :: _, _, _ ->
      add_both 'T';
      debug [%here];
      advance 1
    | "F" :: "F" :: _, _, _ ->
      add_both 'F';
      debug [%here];
      advance 2
    | "F" :: _, _, _ ->
      add_both 'F';
      debug [%here];
      advance 1
    | "G" :: "H" :: _, _ :: _, _ when not (is_vowel (i - 1)) ->
      add_both 'K';
      debug [%here];
      advance 2
    | "G" :: "H" :: "I" :: _, [], _ ->
      add_both 'J';
      debug [%here];
      advance 2
    | "G" :: "H" :: _, [], _ ->
      (* L.569 'ghislane', ghiradelli *)
      add_both 'K';
      debug [%here];
      advance 2
    | "G" :: "H" :: _, _ :: "B" :: _, _
     |"G" :: "H" :: _, _ :: "H" :: _, _
     |"G" :: "H" :: _, _ :: "D" :: _, _
     |"G" :: "H" :: _, _ :: _ :: "B" :: _, _
     |"G" :: "H" :: _, _ :: _ :: "H" :: _, _
     |"G" :: "H" :: _, _ :: _ :: "D" :: _, _
     |"G" :: "H" :: _, _ :: _ :: _ :: "B" :: _, _
     |"G" :: "H" :: _, _ :: _ :: _ :: "H" :: _, _ ->
      (* L.588 Parker's rule (with some further refinements) - e.g., 'hugh' *)
      debug [%here];
      advance 2
    | "G" :: "H" :: _, "U" :: _ :: "C" :: _, _
     |"G" :: "H" :: _, "U" :: _ :: "G" :: _, _
     |"G" :: "H" :: _, "U" :: _ :: "L" :: _, _
     |"G" :: "H" :: _, "U" :: _ :: "R" :: _, _
     |"G" :: "H" :: _, "U" :: _ :: "T" :: _, _ ->
      (* L.610  e.g., 'laugh', 'McLaughlin', 'cough', 'gough', * 'rough', 'tough' *)
      add_both 'F';
      debug [%here];
      advance 2
    | "G" :: "H" :: _, "I" :: _, _ ->
      debug [%here];
      advance 2
    | "G" :: "H" :: _, _ :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "G" :: "N" :: _, [ "A" ], _
     |"G" :: "N" :: _, [ "E" ], _
     |"G" :: "N" :: _, [ "I" ], _
     |"G" :: "N" :: _, [ "O" ], _
     |"G" :: "N" :: _, [ "U" ], _
     |"G" :: "N" :: _, [ "Y" ], _
      when not (force is_slavo_germanic) ->
      Buffer.add_char primary 'K';
      add_both 'N';
      debug [%here];
      advance 2
    | "G" :: "N" :: "E" :: "Y" :: _, _, _ ->
      add_both 'K';
      add_both 'N';
      debug [%here];
      advance 2
    | "G" :: "N" :: _, _, _ when force is_slavo_germanic ->
      add_both 'K';
      add_both 'N';
      debug [%here];
      advance 2
    | "G" :: "N" :: _, _, _ ->
      (* L.644 not e.g. 'cagney' *)
      Buffer.add_char secondary 'K';
      add_both 'N';
      debug [%here];
      advance 2
    | "G" :: "L" :: "I" :: _, _, _ when not (force is_slavo_germanic) ->
      (* L.661 'tagliaro' *)
      Buffer.add_char primary 'K';
      add_both 'L';
      debug [%here];
      advance 2
    | "G" :: "Y" :: _, [], _
     |"G" :: "E" :: "S" :: _, [], _
     |"G" :: "E" :: "P" :: _, [], _
     |"G" :: "E" :: "B" :: _, [], _
     |"G" :: "E" :: "L" :: _, [], _
     |"G" :: "E" :: "Y" :: _, [], _
     |"G" :: "I" :: "B" :: _, [], _
     |"G" :: "I" :: "L" :: _, [], _
     |"G" :: "I" :: "N" :: _, [], _
     |"G" :: "I" :: "E" :: _, [], _
     |"G" :: "E" :: "I" :: _, [], _
     |"G" :: "E" :: "R" :: _, [], _ ->
      (* L.671 -ges-,-gep-,-gel-, -gie- at beginning *)
      add 'K' 'J';
      debug [%here];
      advance 2
    | "G" :: "E" :: "R" :: _, _, "D" :: "A" :: "N" :: "G" :: "E" :: "R" :: _
     |"G" :: "E" :: "R" :: _, _, "R" :: "A" :: "N" :: "G" :: "E" :: "R" :: _
     |"G" :: "E" :: "R" :: _, _, "M" :: "A" :: "N" :: "G" :: "E" :: "R" :: _
     |"G" :: "E" :: "R" :: _, "E" :: _, _
     |"G" :: "E" :: "R" :: _, "I" :: _, _ ->
      add (if force obvious_germanic then 'K' else 'J') 'K';
      debug [%here];
      advance 2
    | "G" :: "E" :: "R" :: _, _, _ ->
      (* L.684 -ger-,  -gy- *)
      add 'K' 'J';
      debug [%here];
      advance 2
    | "G" :: "Y" :: _, _, "D" :: "A" :: "N" :: "G" :: "E" :: "R" :: _
     |"G" :: "Y" :: _, _, "R" :: "A" :: "N" :: "G" :: "E" :: "R" :: _
     |"G" :: "Y" :: _, _, "M" :: "A" :: "N" :: "G" :: "E" :: "R" :: _
     |"G" :: "Y" :: _, "E" :: _, _
     |"G" :: "Y" :: _, "I" :: _, _
     |"G" :: "Y" :: _, "R" :: _, _
     |"G" :: "Y" :: _, "O" :: _, _ ->
      add (if force obvious_germanic then 'K' else 'J') 'K';
      debug [%here];
      advance 2
    | "G" :: "Y" :: _, _, _ ->
      (* L.684 -ger-,  -gy- *)
      add 'K' 'J';
      debug [%here];
      advance 2
    | "G" :: "E" :: "T" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | [ "G"; "I"; "E"; "R" ], _, _
     |"G" :: "I" :: "E" :: "R" :: " " :: _, _, _ ->
      (* L.716 always soft if french ending *)
      add_both 'J';
      debug [%here];
      advance 2
    | "G" :: "E" :: _, _, _
     |"G" :: "I" :: _, _, _
     |"G" :: "G" :: "I" :: _, "A" :: _, _
     |"G" :: "G" :: "I" :: _, "O" :: _, _ ->
      (* L.700 italian e.g, 'biaggi' *)
      add (if force obvious_germanic then 'K' else 'J') 'K';
      debug [%here];
      advance 2
    | "G" :: "G" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "G" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | "H" :: _, [], _
     |"H" :: _, "A" :: _, _
     |"H" :: _, "E" :: _, _
     |"H" :: _, "I" :: _, _
     |"H" :: _, "O" :: _, _
     |"H" :: _, "U" :: _, _
     |"H" :: _, "Y" :: _, _
      when is_vowel (i + 1) ->
      (* L.742 only keep if first & before vowel or btw. 2 vowels *)
      add_both 'H';
      debug [%here];
      advance 2
    | "H" :: _, _, _ ->
      (* L.751 also takes care of 'HH' *)
      debug [%here];
      advance 1
    | "J" :: _, _, "S" :: "A" :: "N" :: " " :: _ ->
      (* L.756 obvious spanish, 'jose', 'san jacinto' *)
      add_both 'H';
      debug [%here];
      advance 1
    | [ "J"; "O"; "S"; "E" ], [], _
     |"J" :: "O" :: "S" :: "E" :: " " :: _, [], _ ->
      (* L.756 obvious spanish, 'jose', 'san jacinto' *)
      add_both 'H';
      debug [%here];
      advance 1
    | "J" :: "O" :: "S" :: "E" :: _, _, _ ->
      add 'J' 'H';
      debug [%here];
      advance 1
    | "J" :: "J" :: _, [], _ ->
      add 'J' 'A';
      debug [%here];
      advance 2
    | "J" :: _, [], _ ->
      (* L.779 Yankelovich/Jankelowicz *)
      add 'J' 'A';
      debug [%here];
      advance 1
    | [ "J" ], _, _ ->
      Buffer.add_char primary 'J';
      debug [%here];
      advance 1
    | "J" :: "A" :: _, _, _
     |"J" :: "O" :: _, _, _
      when is_vowel (i - 1) && not (force is_slavo_germanic) ->
      (* L.784 spanish pron. of e.g. 'bajador' *)
      add 'J' 'H';
      debug [%here];
      advance 1
    | "J" :: "J" :: _, "S" :: _, _
     |"J" :: "J" :: _, "K" :: _, _
     |"J" :: "J" :: _, "L" :: _, _ ->
      debug [%here];
      advance 2
    | "J" :: "L" :: _, _, _
     |"J" :: "T" :: _, _, _
     |"J" :: "K" :: _, _, _
     |"J" :: "S" :: _, _, _
     |"J" :: "N" :: _, _, _
     |"J" :: "M" :: _, _, _
     |"J" :: "B" :: _, _, _
     |"J" :: "Z" :: _, _, _
     |"J" :: _, "S" :: _, _
     |"J" :: _, "K" :: _, _
     |"J" :: _, "L" :: _, _ ->
      debug [%here];
      advance 1
    | "J" :: "J" :: _, _, _ ->
      add_both 'J';
      debug [%here];
      advance 2
    | "J" :: _, _, _ ->
      add_both 'J';
      debug [%here];
      advance 1
    | "K" :: "K" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "K" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | [ "L"; "L"; "O" ], "I" :: _, _
     |[ "L"; "L"; "A" ], "I" :: _, _
     |[ "L"; "L"; "E" ], "A" :: _, _ ->
      (* L.832 spanish e.g. 'cabrillo', 'gallegos' *)
      Buffer.add_char primary 'L';
      debug [%here];
      advance 2
    | "L" :: "L" :: "E" :: _, "A" :: _, _
      when (last - 1 =*| [| "A"; "O" |] && get_at last =* "S") || last =*| [| "A"; "O" |] ->
      (* L.832 spanish e.g. 'cabrillo', 'gallegos' *)
      Buffer.add_char primary 'L';
      debug [%here];
      advance 2
    | "L" :: "L" :: _, _, _ ->
      add_both 'L';
      debug [%here];
      advance 2
    | "L" :: _, _, _ ->
      add_both 'L';
      debug [%here];
      advance 1
    | [ "M"; "B" ], "U" :: _, _
     |"M" :: "B" :: "E" :: "R" :: _, "U" :: _, _
     |"M" :: "M" :: _, _, _ ->
      add_both 'M';
      debug [%here];
      advance 2
    | "M" :: _, _, _ ->
      add_both 'M';
      debug [%here];
      advance 1
    | "N" :: "N" :: _, _, _ ->
      add_both 'N';
      debug [%here];
      advance 2
    | "N" :: _, _, _ ->
      add_both 'N';
      debug [%here];
      advance 1
    | "ñ" :: _, _, _
     |"Ñ" :: _, _, _ ->
      add_both 'N';
      debug [%here];
      advance 1
    | "P" :: "H" :: _, _, _ ->
      add_both 'F';
      debug [%here];
      advance 2
    | "P" :: "P" :: _, _, _
     |"P" :: "B" :: _, _, _ ->
      (* L.891 also account for "campbell", "raspberry" *)
      add_both 'P';
      debug [%here];
      advance 2
    | "P" :: _, _, _ ->
      add_both 'P';
      debug [%here];
      advance 1
    | "Q" :: "Q" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "Q" :: _, _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | [ "R" ], "E" :: "I" :: "E" :: "M" :: _, _
     |[ "R" ], "E" :: "I" :: "A" :: "M" :: _, _ ->
      add_both 'R';
      debug [%here];
      advance 1
    | [ "R" ], "E" :: "I" :: _, _ when not (force is_slavo_germanic) ->
      (* L.910 french e.g. 'rogier', but exclude 'hochmeier' *)
      Buffer.add_char secondary 'R';
      debug [%here];
      advance (if get_at (i + 1) =* "R" then 2 else 1)
    | "R" :: "R" :: _, _, _ ->
      add_both 'R';
      debug [%here];
      advance 2
    | "R" :: _, _, _ ->
      add_both 'R';
      debug [%here];
      advance 1
    | "S" :: "L" :: _, "I" :: _, _
     |"S" :: "L" :: _, "Y" :: _, _ ->
      (* L.932 special cases 'island', 'isle', 'carlisle', 'carlysle' *)
      debug [%here];
      advance 1
    | "S" :: "U" :: "G" :: "A" :: "R" :: _, [], _ ->
      (* L.939 special case 'sugar-' *)
      add 'X' 'S';
      debug [%here];
      advance 1
    | "S" :: "H" :: "E" :: "I" :: "M" :: _, _, _
     |"S" :: "H" :: "O" :: "E" :: "K" :: _, _, _
     |"S" :: "H" :: "O" :: "L" :: "M" :: _, _, _
     |"S" :: "H" :: "O" :: "L" :: "Z" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 2
    | "S" :: "H" :: _, _, _ ->
      (* L.951 germanic *)
      add_both 'X';
      debug [%here];
      advance 2
    | "S" :: "I" :: "O" :: _, _, _
     |"S" :: "I" :: "A" :: _, _, _ ->
      (* L.968 italian & armenian *)
      add 'S' (if not (force is_slavo_germanic) then 'X' else 'S');
      debug [%here];
      advance 3
    | "S" :: "M" :: _, [], _
     |"S" :: "N" :: _, [], _
     |"S" :: "L" :: _, [], _
     |"S" :: "W" :: _, [], _ ->
      (* L.987 german & anglicisations, e.g. 'smith' match 'schmidt',
         'snider' match 'schneider' also, -sz- in slavic language
         although in hungarian it is pronounced 's' *)
      add 'S' 'X';
      debug [%here];
      advance 1
    | "S" :: "Z" :: _, _, _ ->
      add 'S' 'X';
      debug [%here];
      advance 2
    | "S" :: "C" :: "H" :: "E" :: "R" :: _, _, _
     |"S" :: "C" :: "H" :: "E" :: "N" :: _, _, _ ->
      (* L.1015 'schermerhorn', 'schenker' *)
      Buffer.add_char primary 'X';
      Buffer.add_string secondary "SK";
      debug [%here];
      advance 3
    | "S" :: "C" :: "H" :: "O" :: "O" :: _, _, _
     |"S" :: "C" :: "H" :: "U" :: "Y" :: _, _, _
     |"S" :: "C" :: "H" :: "E" :: "D" :: _, _, _
     |"S" :: "C" :: "H" :: "E" :: "M" :: _, _, _ ->
      (* L.1007 Schlesinger's rule *)
      (* L.1010 dutch origin, e.g. 'school', 'schooner' *)
      add_both 'S';
      add_both 'K';
      debug [%here];
      advance 3
    | "S" :: "C" :: "H" :: _, _ :: _, _
     |"S" :: "C" :: "H" :: "A" :: _, _, _
     |"S" :: "C" :: "H" :: "E" :: _, _, _
     |"S" :: "C" :: "H" :: "I" :: _, _, _
     |"S" :: "C" :: "H" :: "O" :: _, _, _
     |"S" :: "C" :: "H" :: "U" :: _, _, _
     |"S" :: "C" :: "H" :: "Y" :: _, _, _
     |"S" :: "C" :: "H" :: "W" :: _, _, _ ->
      add_both 'X';
      debug [%here];
      advance 3
    | "S" :: "C" :: "H" :: _, _, _ ->
      add 'X' 'S';
      debug [%here];
      advance 3
    | "S" :: "C" :: "I" :: _, _, _
     |"S" :: "C" :: "E" :: _, _, _
     |"S" :: "C" :: "Y" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 3
    | "S" :: "C" :: _, _, _ ->
      add_both 'S';
      add_both 'K';
      debug [%here];
      advance 3
    | [ "S" ], "I" :: "A" :: _, _
     |[ "S" ], "I" :: "O" :: _, _ ->
      (* L.1063 french e.g. 'resnais', 'artois' *)
      Buffer.add_char secondary 'S';
      debug [%here];
      advance 1
    | "S" :: "S" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 2
    | "S" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 1
    | "T" :: "I" :: "O" :: "N" :: _, _, _
     |"T" :: "I" :: "A" :: _, _, _
     |"T" :: "C" :: "H" :: _, _, _ ->
      add_both 'X';
      debug [%here];
      advance 3
    | "T" :: "H" :: "O" :: "M" :: _, _, _
     |"T" :: "H" :: "A" :: "M" :: _, _, _ ->
      (* L.1102 special case 'thomas', 'thames' or germanic *)
      add_both 'T';
      debug [%here];
      advance 2
    | "T" :: "H" :: _, _, _
     |"T" :: "T" :: "H" :: _, _, _ ->
      add (if force obvious_germanic then 'T' else '0') 'T';
      debug [%here];
      advance 2
    | "T" :: "T" :: _, _, _
     |"T" :: "D" :: _, _, _ ->
      add_both 'T';
      debug [%here];
      advance 2
    | "T" :: _, _, _ ->
      add_both 'T';
      debug [%here];
      advance 1
    | "V" :: "V" :: _, _, _ ->
      add_both 'F';
      debug [%here];
      advance 2
    | "V" :: _, _, _ ->
      add_both 'F';
      debug [%here];
      advance 1
    | "W" :: "R" :: _, _, _ ->
      (* L.1137 can also be in middle of word *)
      add_both 'R';
      debug [%here];
      advance 2
    | [ "W" ], "A" :: _, _
     |[ "W" ], "E" :: _, _
     |[ "W" ], "I" :: _, _
     |[ "W" ], "O" :: _, _
     |[ "W" ], "U" :: _, _
     |[ "W" ], "Y" :: _, _ ->
      Buffer.add_char secondary 'F';
      debug [%here];
      advance 1
    | "W" :: "S" :: "K" :: "I" :: _, "E" :: _, _
     |"W" :: "S" :: "K" :: "Y" :: _, "E" :: _, _
     |"W" :: "S" :: "K" :: "I" :: _, "O" :: _, _
     |"W" :: "S" :: "K" :: "Y" :: _, "O" :: _, _
     |"W" :: _, _, "S" :: "C" :: "H" :: _ ->
      (* L.1164 Arnow should match Arnoff *)
      Buffer.add_char secondary 'F';
      debug [%here];
      advance 1
    | "W" :: "I" :: "C" :: "Z" :: _, _, _
     |"W" :: "I" :: "T" :: "Z" :: _, _, _ ->
      (* L.1176 polish e.g. 'filipowicz' *)
      Buffer.add_string primary "TS";
      Buffer.add_string secondary "FX";
      debug [%here];
      advance 4
    | "W" :: _, _, _ ->
      (* L.1185 else skip it *)
      debug [%here];
      advance 1
    | [ "X" ], "U" :: "O" :: _, _
     |[ "X" ], "U" :: "A" :: _, _ ->
      debug [%here];
      advance 1
    | "X" :: "C" :: _, _, _
     |"X" :: "X" :: _, _, _ ->
      (* L.1190 french e.g. breaux *)
      Buffer.add_string primary "KS";
      Buffer.add_string secondary "KS";
      debug [%here];
      advance 2
    | "X" :: _, _, _ ->
      Buffer.add_string primary "KS";
      Buffer.add_string secondary "KS";
      debug [%here];
      advance 1
    | "Z" :: "H" :: _, _, _ ->
      (* L.1209 chinese pinyin e.g. 'zhao' *)
      add_both 'J';
      debug [%here];
      advance 2
    | "Z" :: "Z" :: "O" :: _, _, _
     |"Z" :: "Z" :: "I" :: _, _, _
     |"Z" :: "Z" :: "A" :: _, _, _ ->
      Buffer.add_char secondary 'T';
      add_both 'S';
      debug [%here];
      advance 2
    | "Z" :: "Z" :: _, "T" :: _, _ ->
      add_both 'S';
      debug [%here];
      advance 2
    | "Z" :: _, "T" :: _, _ ->
      add_both 'S';
      debug [%here];
      advance 1
    | "Z" :: "Z" :: _, _ :: _, _ when force is_slavo_germanic ->
      Buffer.add_char secondary 'T';
      add_both 'S';
      debug [%here];
      advance 2
    | "Z" :: _, _ :: _, _ when force is_slavo_germanic ->
      Buffer.add_char secondary 'T';
      add_both 'S';
      debug [%here];
      advance 1
    | "Z" :: "Z" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 2
    | "Z" :: _, _, _ ->
      add_both 'S';
      debug [%here];
      advance 1
    | _ :: _, _, _ ->
      debug [%here];
      advance 1
    | [], _, _ -> ());
    match
      (Buffer.length primary < max_length || Buffer.length secondary < max_length)
      && !current < Array.length glyphs
    with
    | true ->
      Fn.apply_n_times ~n:(!current - i)
        (function
          | ([], _, _) as acc -> acc
          | x :: rest, dropped, ll -> rest, x :: dropped, ll)
        triple
      |> loop !current
    | false -> ()
  in

  match original with
  | "" -> "", ""
  | _ ->
    let triple =
      Fn.apply_n_times ~n:!current
        (function
          | ([], _, _) as acc -> acc
          | x :: rest, dropped, ll -> rest, x :: dropped, ll)
        (ll, [], ll)
    in
    loop !current triple;
    let finalize b = Buffer.To_string.sub b ~pos:0 ~len:(min max_length (Buffer.length b)) in
    finalize primary, finalize secondary
