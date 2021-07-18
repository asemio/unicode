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

let ( =*| ) ~get_at pos arr = Array.mem arr (get_at pos) ~equal:[%equal: string]

let ( <>*| ) ~get_at pos arr = not (( =*| ) ~get_at pos arr)

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

let string_at glyphs =
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

let is_vowel ~get_at pos =
  match get_at pos with
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
  let string_at = string_at glyphs in
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
  | _ -> ());

  let rec loop i ll =
    (match ll, i with
    | "A" :: _, _
     |"E" :: _, _
     |"I" :: _, _
     |"O" :: _, _
     |"U" :: _, _
     |"Y" :: _, _ ->
      if i = 0 then add_both 'A';
      debug [%here];
      advance 1
    | "B" :: "B" :: _, _ ->
      add_both 'P';
      debug [%here];
      advance 2
    | "B" :: _, _ ->
      (* L.290 "-mb", e.g "dumb", already skipped over... *)
      add_both 'P';
      debug [%here];
      advance 1
    | "ç" :: _, _
     |"Ç" :: _, _ ->
      add_both 'S';
      debug [%here];
      advance 1
    | "C" :: _, _
      when i > 1
           && (not (is_vowel (i - 2)))
           && i - 1 =*> [| "A"; "C"; "H" |]
           && get_at (i + 2) <>* "I"
           && (get_at (i + 2) <>* "E"
              || string_at (i - 2)
                   [| [| "B"; "A"; "C"; "H"; "E"; "R" |]; [| "M"; "A"; "C"; "H"; "E"; "R" |] |]
              ) ->
      (* L. 307 various germanic *)
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "A" :: "E" :: "S" :: "A" :: "R" :: _, 0 ->
      (* L.322 special case 'caesar' *)
      add_both 'S';
      debug [%here];
      advance 2
    | "C" :: "H" :: "I" :: "A" :: _, _ ->
      (* L.332 italian 'chianti' *)
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "H" :: _, _ ->
      begin
        match i > 0 with
        | true when i + 2 =*> [| "A"; "E" |] ->
          (* L.343 find 'michael' *)
          debug [%here];
          add 'K' 'X'
        | false
          when string_at (i + 2)
                 [|
                   [| "A"; "R"; "A"; "C" |];
                   [| "A"; "R"; "I"; "S" |];
                   [| "O"; "R" |];
                   [| "Y"; "M" |];
                   [| "I"; "A" |];
                   [| "E"; "M" |];
                 |]
               && not (0 =*> [| "C"; "H"; "O"; "R"; "E" |]) ->
          (* L.353 greek roots e.g. 'chemistry', 'chorus' *)
          debug [%here];
          add_both 'K'
        | gtz
          when (* L.367 germanic, greek, or otherwise 'ch' for 'kh' sound *)
               force obvious_germanic
               (* L.371 'architect but not 'arch', 'orchestra', 'orchid' *)
               || string_at (i - 2)
                    [|
                      [| "O"; "R"; "C"; "H"; "E"; "S" |];
                      [| "A"; "R"; "C"; "H"; "I"; "T" |];
                      [| "O"; "R"; "C"; "H"; "I"; "D" |];
                    |]
               || i + 2 =*| [| "T"; "S" |]
               || (i - 1 =*| [| "A"; "O"; "U"; "E" |] || not gtz)
                  && (i + 2 =*| [| "L"; "R"; "N"; "M"; "B"; "H"; "F"; "V"; "W" |] || get_at (i + 2) =* " ")
          ->
          (* L.381 e.g., 'wachtler', 'wechsler', but not 'tichner' *)
          debug [%here];
          add_both 'K'
        | true when 0 =*> [| "M"; "C" |] ->
          (* L.396 e.g., "McHugh" *)
          debug [%here];
          add_both 'K'
        | true ->
          debug [%here];
          add 'X' 'K'
        | false ->
          debug [%here];
          add_both 'X'
      end;
      debug [%here];
      advance 2
    | "C" :: "Z" :: _, _ when not (i - 2 =*> [| "W"; "I" |]) ->
      (* L.415  e.g, 'czerny' *)
      add 'S' 'X';
      debug [%here];
      advance 2
    | "C" :: "C" :: "I" :: "A" :: _, _ ->
      (* L.425 e.g., 'focaccia' *)
      add_both 'X';
      debug [%here];
      advance 3
    | "C" :: "C" :: _, _ when not (i = 1 && get_at 0 =* "M") -> (
      (* L.434 double 'C', but not if e.g. 'McClellan' *)
      match () with
      | _ when i + 2 =*| [| "I"; "E"; "H" |] && not (i + 2 =*> [| "H"; "U" |]) ->
        (* L.438 'bellocchio' but not 'bacchus' *)
        if (i = 1 && get_at (i - 1) =* "A")
           || string_at (i - 1) [| [| "U"; "C"; "C"; "E"; "E" |]; [| "U"; "C"; "C"; "E"; "S" |] |]
        then (
          (* L.442 'accident', 'accede' 'succeed' *)
          add_both 'K';
          add_both 'S'
        )
        else (* L.451 'bacci', 'bertucci', other italian *)
          add_both 'X';
        debug [%here];
        advance 3
      | _ ->
        (* L.462 Pierce's rule *)
        add_both 'K';
        debug [%here];
        advance 2
    )
    | "C" :: "K" :: _, _
     |"C" :: "G" :: _, _
     |"C" :: "Q" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "C" :: "I" :: "O" :: _, _
     |"C" :: "I" :: "E" :: _, _
     |"C" :: "I" :: "A" :: _, _ ->
      (* L.480 italian vs. english *)
      add 'S' 'X';
      debug [%here];
      advance 2
    | "C" :: "I" :: _, _
     |"C" :: "E" :: _, _
     |"C" :: "Y" :: _, _ ->
      add_both 'S';
      debug [%here];
      advance 2
    | "C" :: _, _ ->
      (* L.500 name sent in 'mac caffrey', 'mac gregor' *)
      add_both 'K';
      debug [%here];
      (match i + 1 with
      | j when get_at j =* " " && j + 1 =*| [| "C"; "Q"; "G" |] -> 3
      | j when j =*| [| "C"; "K"; "Q" |] && not (string_at j [| [| "C"; "E" |]; [| "C"; "I" |] |]) -> 2
      | _ -> 1)
      |> advance
    | "D" :: "G" :: _, _ ->
      if i + 2 =*| [| "I"; "E"; "Y" |]
      then (
        (* L.517 'edge' *)
        add_both 'J';
        debug [%here];
        advance 3
      )
      else (
        (* L.525 'edgar' *)
        add_both 'T';
        add_both 'K';
        debug [%here];
        advance 2
      )
    | "D" :: "T" :: _, _
     |"D" :: "D" :: _, _ ->
      add_both 'T';
      debug [%here];
      advance 2
    | "D" :: _, _ ->
      add_both 'T';
      debug [%here];
      advance 1
    | "F" :: "F" :: _, _ ->
      add_both 'F';
      debug [%here];
      advance 2
    | "F" :: _, _ ->
      add_both 'F';
      debug [%here];
      advance 1
    | "G" :: "H" :: _, _ -> (
      match i with
      | i when i > 0 && not (is_vowel (i - 1)) ->
        add_both 'K';
        debug [%here];
        advance 2
      | 0 ->
        (* L.569 'ghislane', ghiradelli *)
        add_both (if get_at 2 =* "I" then 'J' else 'K');
        debug [%here];
        advance 2
      | i
        when (i > 1 && i - 2 =*| [| "B"; "H"; "D" |])
             (* L.595 e.g., 'bough' *)
             || (i > 2 && i - 3 =*| [| "B"; "H"; "D" |])
             (* L.599 e.g., 'broughton' *)
             || (i > 3 && i - 4 =*| [| "B"; "H" |]) ->
        (* L.588 Parker's rule (with some further refinements) - e.g., 'hugh' *)
        debug [%here];
        advance 2
      | i when i > 2 && get_at (i - 1) =* "U" && i - 3 =*| [| "C"; "G"; "L"; "R"; "T" |] ->
        (* L.610  e.g., 'laugh', 'McLaughlin', 'cough', 'gough', * 'rough', 'tough' *)
        add_both 'F';
        debug [%here];
        advance 2
      | i when i > 0 && not (get_at (i - 1) =* "I") ->
        add_both 'K';
        debug [%here];
        advance 2
      | _ ->
        debug [%here];
        advance 2
    )
    | "G" :: "N" :: _, _ ->
      (match i with
      | 1 when is_vowel 0 && not (force is_slavo_germanic) ->
        Buffer.add_char primary 'K';
        add_both 'N'
      | i
        when (not (i + 2 =*> [| "E"; "Y" |]))
             (* TODO: review this clause *)
             && get_at (i + 1) <>* "Y"
             && not (force is_slavo_germanic) ->
        (* L.644 not e.g. 'cagney' *)
        Buffer.add_char secondary 'K';
        add_both 'N'
      | _ ->
        add_both 'K';
        add_both 'N');
      debug [%here];
      advance 2
    | "G" :: "L" :: _, _ when get_at (i + 2) =* "I" && not (force is_slavo_germanic) ->
      (* L.661 'tagliaro' *)
      Buffer.add_char primary 'K';
      add_both 'L';
      debug [%here];
      advance 2
    | "G" :: next :: _, 0
      when next =* "Y"
           || string_at (i + 1)
                [|
                  [| "E"; "S" |];
                  [| "E"; "P" |];
                  [| "E"; "B" |];
                  [| "E"; "L" |];
                  [| "E"; "Y" |];
                  [| "I"; "B" |];
                  [| "I"; "L" |];
                  [| "I"; "N" |];
                  [| "I"; "E" |];
                  [| "E"; "I" |];
                  [| "E"; "R" |];
                |] ->
      (* L.671 -ges-,-gep-,-gel-, -gie- at beginning *)
      add 'K' 'J';
      debug [%here];
      advance 2
    | "G" :: "E" :: "R" :: _, _
     |"G" :: "Y" :: _, _
      when (not
              (string_at 0
                 [|
                   [| "D"; "A"; "N"; "G"; "E"; "R" |];
                   [| "R"; "A"; "N"; "G"; "E"; "R" |];
                   [| "M"; "A"; "N"; "G"; "E"; "R" |];
                 |]
              )
           )
           && i - 1 <>*| [| "E"; "I" |]
           && not (string_at (i - 1) [| [| "R"; "G"; "Y" |]; [| "O"; "G"; "Y" |] |]) ->
      (* L.684 -ger-,  -gy- *)
      add 'K' 'J';
      debug [%here];
      advance 2
    | "G" :: _, _
      when i + 1 =*| [| "E"; "I"; "Y" |]
           || string_at (i - 1) [| [| "A"; "G"; "G"; "I" |]; [| "O"; "G"; "G"; "I" |] |] ->
      (* L.700 italian e.g, 'biaggi' *)
      (match () with
      | _ when force obvious_germanic || i + 1 =*> [| "E"; "T" |] ->
        (* L.705 obvious germanic *)
        add_both 'K'
      | _ when i + 1 =*> [| "I"; "E"; "R" |] && get_at (i + 4) =* " " ->
        (* L.716 always soft if french ending *)
        add_both 'J'
      | _ -> add 'J' 'K');
      debug [%here];
      advance 2
    | "G" :: "G" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "G" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | "H" :: _, _ when (i = 0 || is_vowel (i - 1)) && is_vowel (i + 1) ->
      (* L.742 only keep if first & before vowel or btw. 2 vowels *)
      add_both 'H';
      debug [%here];
      advance 2
    | "H" :: _, _ ->
      (* L.751 also takes care of 'HH' *)
      debug [%here];
      advance 1
    | "J" :: _, _ when i + 1 =*> [| "O"; "S"; "E" |] || 0 =*> [| "S"; "A"; "N"; " " |] ->
      (* L.756 obvious spanish, 'jose', 'san jacinto' *)
      let p = if (i = 0 && get_at (i + 4) =* " ") || 0 =*> [| "S"; "A"; "N"; " " |] then 'H' else 'J' in
      add p 'H';
      debug [%here];
      advance 1
    | "J" :: _, _ ->
      (match i with
      | i when i = 0 && not (i + 1 =*> [| "O"; "S"; "E" |]) ->
        (* L.779 Yankelovich/Jankelowicz *)
        add 'J' 'A'
      | i when is_vowel (i - 1) && (not (force is_slavo_germanic)) && i + 1 =*| [| "A"; "O" |] ->
        (* L.784 spanish pron. of e.g. 'bajador' *)
        add 'J' 'H'
      | i when i = last -> Buffer.add_char primary 'J'
      | i when i + 1 <>*| [| "L"; "T"; "K"; "S"; "N"; "M"; "B"; "Z" |] && i - 1 <>*| [| "S"; "K"; "L" |]
        ->
        add_both 'J'
      | _ -> ());
      debug [%here];
      advance (if get_at (i + 1) =* "J" then 2 else 1)
    | "K" :: "K" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "K" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | "L" :: "L" :: _, _
      when i = Array.length glyphs - 3
           && string_at (i - 1)
                [| [| "I"; "L"; "L"; "O" |]; [| "I"; "L"; "L"; "A" |]; [| "A"; "L"; "L"; "E" |] |]
           || (string_at (last - 1) [| [| "A"; "S" |]; [| "O"; "S" |] |] || last =*| [| "A"; "O" |])
              && i - 1 =*> [| "A"; "L"; "L"; "E" |] ->
      (* L.832 spanish e.g. 'cabrillo', 'gallegos' *)
      Buffer.add_char primary 'L';
      debug [%here];
      advance 2
    | "L" :: "L" :: _, _ ->
      add_both 'L';
      debug [%here];
      advance 2
    | "L" :: _, _ ->
      add_both 'L';
      debug [%here];
      advance 1
    | "M" :: _, _ ->
      add_both 'M';
      debug [%here];
      advance
        ( if i - 1 =*> [| "U"; "M"; "B" |]
             (* L.858 'dumb','thumb' *)
             && (i + 1 = last || i + 2 =*> [| "E"; "R" |])
             || get_at (i + 1) =* "M"
        then 2
        else 1
        )
    | "N" :: "N" :: _, _ ->
      add_both 'N';
      debug [%here];
      advance 2
    | "N" :: _, _ ->
      add_both 'N';
      debug [%here];
      advance 1
    | "ñ" :: _, _
     |"Ñ" :: _, _ ->
      add_both 'N';
      debug [%here];
      advance 1
    | "P" :: "H" :: _, _ ->
      add_both 'F';
      debug [%here];
      advance 2
    | "P" :: "P" :: _, _
     |"P" :: "B" :: _, _ ->
      (* L.891 also account for "campbell", "raspberry" *)
      add_both 'P';
      debug [%here];
      advance 2
    | "P" :: _, _ ->
      add_both 'P';
      debug [%here];
      advance 1
    | "Q" :: "Q" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 2
    | "Q" :: _, _ ->
      add_both 'K';
      debug [%here];
      advance 1
    | "R" :: _, _ ->
      if not
           (i = last
           && (not (force is_slavo_germanic))
           && i - 2 =*> [| "I"; "E" |]
           && not (string_at (i - 4) [| [| "M"; "E" |]; [| "M"; "A" |] |])
           )
      then (* L.910 french e.g. 'rogier', but exclude 'hochmeier' *)
        Buffer.add_char primary 'R';
      Buffer.add_char secondary 'R';
      debug [%here];
      advance (if get_at (i + 1) =* "R" then 2 else 1)
    | "S" :: "L" :: _, _ when i - 1 =*| [| "I"; "Y" |] ->
      (* L.932 special cases 'island', 'isle', 'carlisle', 'carlysle' *)
      debug [%here];
      advance 1
    | "S" :: "U" :: "G" :: "A" :: "R" :: _, 0 ->
      (* L.939 special case 'sugar-' *)
      add 'X' 'S';
      debug [%here];
      advance 1
    | "S" :: "H" :: _, _ ->
      (* L.951 germanic *)
      add_both
        ( if string_at (i + 2)
               [| [| "E"; "I"; "M" |]; [| "O"; "E"; "K" |]; [| "O"; "L"; "M" |]; [| "O"; "L"; "Z" |] |]
        then 'S'
        else 'X'
        );
      debug [%here];
      advance 2
    | "S" :: "I" :: "O" :: _, _
     |"S" :: "I" :: "A" :: _, _ ->
      (* L.968 italian & armenian *)
      add 'S' (if not (force is_slavo_germanic) then 'X' else 'S');
      debug [%here];
      advance 3
    | "S" :: "M" :: _, 0
     |"S" :: "N" :: _, 0
     |"S" :: "L" :: _, 0
     |"S" :: "W" :: _, 0
     |"S" :: "Z" :: _, _ ->
      (* L.987 german & anglicisations, e.g. 'smith' match 'schmidt',
         'snider' match 'schneider' also, -sz- in slavic language
         although in hungarian it is pronounced 's' *)
      add 'S' 'X';
      debug [%here];
      advance (if get_at (i + 1) =* "Z" then 2 else 1)
    | "S" :: "C" :: _, _ -> (
      (* L.1007 Schlesinger's rule *)
      match i with
      | i when get_at (i + 2) =* "H" ->
        (* L.1010 dutch origin, e.g. 'school', 'schooner' *)
        (match
           string_at (i + 3)
             [|
               [| "O"; "O" |];
               [| "E"; "R" |];
               [| "E"; "N" |];
               [| "U"; "Y" |];
               [| "E"; "D" |];
               [| "E"; "M" |];
             |]
         with
        | true ->
          if string_at (i + 3) [| [| "E"; "R" |]; [| "E"; "N" |] |]
          then (* L.1015 'schermerhorn', 'schenker' *)
            Buffer.add_char primary 'X'
          else Buffer.add_string primary "SK";
          Buffer.add_string secondary "SK"
        | false ->
          let s = if i = 0 && (not (is_vowel 3)) && get_at 3 <>* "W" then 'S' else 'X' in
          add 'X' s);
        debug [%here];
        advance 3
      | i when i + 2 =*| [| "I"; "E"; "Y" |] ->
        add_both 'S';
        debug [%here];
        advance 3
      | _ ->
        add_both 'S';
        add_both 'K';
        debug [%here];
        advance 3
    )
    | "S" :: _, _ ->
      if i = last && string_at (i - 2) [| [| "A"; "I" |]; [| "O"; "I" |] |]
      then (* L.1063 french e.g. 'resnais', 'artois' *)
        Buffer.add_char secondary 'S'
      else add_both 'S';
      debug [%here];
      advance (if i + 1 =*| [| "S"; "Z" |] then 2 else 1)
    | "T" :: "I" :: "O" :: "N" :: _, _
     |"T" :: "I" :: "A" :: _, _
     |"T" :: "C" :: "H" :: _, _ ->
      add_both 'X';
      debug [%here];
      advance 3
    | "T" :: "H" :: _, _
     |"T" :: "T" :: "H" :: _, _ ->
      let p =
        (* L.1102 special case 'thomas', 'thames' or germanic *)
        if string_at (i + 2) [| [| "O"; "M" |]; [| "A"; "M" |] |] || force obvious_germanic
        then 'T'
        else '0'
      in
      add p 'T';
      debug [%here];
      advance 2
    | "T" :: "T" :: _, _
     |"T" :: "D" :: _, _ ->
      add_both 'T';
      debug [%here];
      advance 2
    | "T" :: _, _ ->
      add_both 'T';
      debug [%here];
      advance 1
    | "V" :: "V" :: _, _ ->
      add_both 'F';
      debug [%here];
      advance 2
    | "V" :: _, _ ->
      add_both 'F';
      debug [%here];
      advance 1
    | "W" :: "R" :: _, _ ->
      (* L.1137 can also be in middle of word *)
      add_both 'R';
      debug [%here];
      advance 2
    | "W" :: _, _ -> (
      (match i = 0 && (is_vowel (i + 1) || get_at (i + 1) =* "H"), is_vowel (i + 1) with
      | true, true ->
        (* L.1150 Wasserman should match Vasserman *)
        add 'A' 'F'
      | true, false ->
        (* L.1158 need Uomo to match Womo *)
        add_both 'A'
      | false, _ -> ());
      match i with
      | i
        when (i = last && is_vowel (i - 1))
             || string_at (i - 1)
                  [|
                    [| "E"; "W"; "S"; "K"; "I" |];
                    [| "E"; "W"; "S"; "K"; "Y" |];
                    [| "O"; "W"; "S"; "K"; "I" |];
                    [| "O"; "W"; "S"; "K"; "Y" |];
                  |]
             || 0 =*> [| "S"; "C"; "H" |] ->
        (* L.1164 Arnow should match Arnoff *)
        Buffer.add_char secondary 'F';
        debug [%here];
        advance 1
      | i when string_at (i + 1) [| [| "I"; "C"; "Z" |]; [| "I"; "T"; "Z" |] |] ->
        (* L.1176 polish e.g. 'filipowicz' *)
        Buffer.add_string primary "TS";
        Buffer.add_string secondary "FX";
        debug [%here];
        advance 4
      | _ ->
        (* L.1185 else skip it *)
        debug [%here];
        advance 1
    )
    | "X" :: _, _ ->
      if not (i = last && string_at (i - 2) [| [| "A"; "U" |]; [| "O"; "U" |] |])
      then (
        (* L.1190 french e.g. breaux *)
        Buffer.add_string primary "KS";
        Buffer.add_string secondary "KS"
      );
      debug [%here];
      advance (if i + 1 =*| [| "C"; "X" |] then 2 else 1)
    | "Z" :: "H" :: _, _ ->
      (* L.1209 chinese pinyin e.g. 'zhao' *)
      add_both 'J';
      debug [%here];
      advance 2
    | "Z" :: _, _ ->
      if string_at (i + 1) [| [| "Z"; "O" |]; [| "Z"; "I" |]; [| "Z"; "A" |] |]
         || (force is_slavo_germanic && i > 0 && not (get_at (i - 1) =* "T"))
      then Buffer.add_char secondary 'T';
      add_both 'S';
      debug [%here];
      advance (if get_at (i + 1) =* "Z" then 2 else 1)
    | _ :: _, _ ->
      debug [%here];
      advance 1
    | [], _ -> ());
    match
      (Buffer.length primary < max_length || Buffer.length secondary < max_length)
      && !current < Array.length glyphs
    with
    | true -> List.drop ll (!current - i) |> loop !current
    | false -> ()
  in

  match original with
  | "" -> "", ""
  | _ ->
    List.drop ll !current |> loop !current;
    let finalize b = Buffer.To_string.sub b ~pos:0 ~len:(min max_length (Buffer.length b)) in
    finalize primary, finalize secondary
