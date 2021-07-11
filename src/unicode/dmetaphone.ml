open! Core_kernel

let ignore = function
| "ç"
 |"Ç"
 |"ñ"
 |"Ñ" ->
  true
| _ -> false

let ( =* ) = String.( = )

let ( <>* ) = String.( <> )

let get_at s pos = if pos < 0 || pos >= Array.length s then "" else Array.get s pos

let string_at s pos arr =
  pos >= 0
  && Array.exists arr ~f:(function
       | " " when pos >= String.length s -> true
       | substring -> String.is_substring_at s ~pos ~substring
       )

let is_vowel arr pos =
  match get_at arr pos with
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

let double_metaphone ?(max_length = 4) ~standardized:original ~glyphs =
  let debug (here : Source_code_position.t) =
    if false then print_endline (sprintf "[%d]" here.pos_lnum)
  in
  (* original: AMÉLIE *)
  (* glyphs: [| "A"; "M"; "É"; "L"; "I"; "E" |] *)
  let get_at = get_at glyphs in
  let is_vowel = is_vowel glyphs in
  let string_at = string_at original in
  let is_slavo_germanic = lazy (slavo_germanic original) in
  let primary = Buffer.create 8 in
  let secondary = Buffer.create 8 in
  let current = ref 0 in
  let last = Array.length glyphs - 1 in
  let advance x = current := !current + x in
  let add p s =
    Buffer.add_char primary p;
    Buffer.add_char secondary s
  in
  let add_both c = add c c in

  (* L.253 skip these when at start of word *)
  if string_at 0 [| "GN"; "KN"; "PN"; "WR"; "PS" |]
  then (
    debug [%here];
    advance 1
  );

  (* L.257 Initial 'X' is pronounced 'Z' e.g. Xavier *)
  if get_at 0 =* "X"
  then (
    add_both 'S';
    debug [%here];
    advance 1
  );

  let rec loop () =
    if (Buffer.length primary < 4 || Buffer.length secondary < 4) && !current < Array.length glyphs
    then begin
      (match get_at !current with
      | "A"
       |"E"
       |"I"
       |"O"
       |"U"
       |"Y" ->
        if !current = 0 then add_both 'A';
        debug [%here];
        advance 1
      | "B" ->
        (* L.290 "-mb", e.g "dumb", already skipped over... *)
        add_both 'P';
        advance (if get_at (!current + 1) =* "B" then 2 else 1) (* TODO: Normalize this file *)
      | "Ç" ->
        add_both 'S';
        debug [%here];
        advance 1
      | "C"
        when !current > 1
             && (not (is_vowel (!current - 2)))
             && string_at (!current - 1) [| "ACH" |]
             && get_at (!current + 2) <>* "I"
             && (get_at (!current + 2) <>* "E" || string_at (!current - 2) [| "BACHER"; "MACHER" |]) ->
        (* L. 307 various germanic *)
        add_both 'K';
        debug [%here];
        advance 2
      | "C" when !current = 0 && string_at 0 [| "CAESAR" |] ->
        (* L.322 special case 'caesar' *)
        add_both 'S';
        debug [%here];
        advance 2
      | "C" when string_at !current [| "CHIA" |] ->
        (* L.332 italian 'chianti' *)
        add_both 'K';
        debug [%here];
        advance 2
      | "C" when string_at !current [| "CH" |] ->
        begin
          match !current > 0 with
          | true when string_at !current [| "CHAE" |] ->
            (* L.343 find 'michael' *)
            debug [%here];
            add 'K' 'X'
          | false
            when string_at (!current + 1) [| "HARAC"; "HARIS"; "HOR"; "HYM"; "HIA"; "HEM" |]
                 && not (string_at 0 [| "CHORE" |]) ->
            (* L.353 greek roots e.g. 'chemistry', 'chorus' *)
            debug [%here];
            add_both 'K'
          | gtz
            when (* L.367 germanic, greek, or otherwise 'ch' for 'kh' sound *)
                 string_at 0 [| "VAN "; "VON "; "SCH" |]
                 (* L.371 'architect but not 'arch', 'orchestra', 'orchid' *)
                 || string_at (!current - 2) [| "ORCHES"; "ARCHIT"; "ORCHID" |]
                 || string_at (!current + 2) [| "T"; "S" |]
                 || (string_at (!current - 1) [| "A"; "O"; "U"; "E" |] || not gtz)
                    && string_at (!current + 2) [| "L"; "R"; "N"; "M"; "B"; "H"; "F"; "V"; "W"; " " |] ->
            (* L.381 e.g., 'wachtler', 'wechsler', but not 'tichner' *)
            debug [%here];
            add_both 'K'
          | true when string_at 0 [| "MC" |] ->
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
      | "C" when string_at !current [| "CZ" |] && not (string_at (!current - 2) [| "WICZ" |]) ->
        (* L.415  e.g, 'czerny' *)
        add 'S' 'X';
        debug [%here];
        advance 2
      | "C" when string_at (!current + 1) [| "CIA" |] ->
        (* L.425 e.g., 'focaccia' *)
        add_both 'X';
        debug [%here];
        advance 3
      | "C" when string_at !current [| "CC" |] && not (!current = 1 && get_at 0 =* "M") -> (
        (* L.434 double 'C', but not if e.g. 'McClellan' *)
        match () with
        | _ when string_at (!current + 2) [| "I"; "E"; "H" |] && not (string_at (!current + 2) [| "HU" |])
          ->
          (* L.438 'bellocchio' but not 'bacchus' *)
          if (!current = 1 && get_at (!current - 1) =* "A")
             || string_at (!current - 1) [| "UCCEE"; "UCCES" |]
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
      | "C" when string_at !current [| "CK"; "CG"; "CQ" |] ->
        add_both 'K';
        debug [%here];
        advance 2
      | "C" when string_at !current [| "CI"; "CE"; "CY" |] ->
        (* L.480 italian vs. english *)
        if string_at !current [| "CIO"; "CIE"; "CIA" |] then add 'S' 'X' else add_both 'S';
        debug [%here];
        advance 2
      | "C" ->
        (* L.500 name sent in 'mac caffrey', 'mac gregor' *)
        add_both 'K';
        debug [%here];
        (match !current + 1 with
        | i when string_at i [| " C"; " Q"; " G" |] -> 3
        | i when string_at i [| "C"; "K"; "Q" |] && not (string_at i [| "CE"; "CI" |]) -> 2
        | _ -> 1)
        |> advance
      | "D" when string_at !current [| "DG" |] ->
        if string_at (!current + 2) [| "I"; "E"; "Y" |]
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
      | "D" when string_at !current [| "DT"; "DD" |] ->
        add_both 'T';
        debug [%here];
        advance 2
      | "D" ->
        add_both 'T';
        debug [%here];
        advance 1
      | "F" ->
        add_both 'F';
        debug [%here];
        advance (if get_at (!current + 1) =* "F" then 2 else 1)
      | "G" when get_at (!current + 1) =* "H" -> (
        match !current with
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
          when (i > 1 && string_at (i - 2) [| "B"; "H"; "D" |])
               (* L.595 e.g., 'bough' *)
               || (i > 2 && string_at (i - 3) [| "B"; "H"; "D" |])
               (* L.599 e.g., 'broughton' *)
               || (i > 3 && string_at (i - 4) [| "B"; "H" |]) ->
          (* L.588 Parker's rule (with some further refinements) - e.g., 'hugh' *)
          debug [%here];
          advance 2
        | i when i > 2 && get_at (i - 1) =* "U" && string_at (i - 3) [| "C"; "G"; "L"; "R"; "T" |] ->
          (* L.610  e.g., 'laugh', 'McLaughlin', 'cough', 'gough', * 'rough', 'tough' *)
          add_both 'F';
          debug [%here];
          advance 2
        | i when i > 0 && get_at (i - 1) <>* "I" ->
          add_both 'K';
          debug [%here];
          advance 2
        | _ ->
          debug [%here];
          advance 2
      )
      | "G" when get_at (!current + 1) =* "N" ->
        (match !current with
        | 1 when is_vowel 0 && not (force is_slavo_germanic) ->
          Buffer.add_char primary 'K';
          add_both 'N'
        | i
          when (not (string_at (i + 2) [| "EY" |]))
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
      | "G" when string_at (!current + 1) [| "LI" |] && not (force is_slavo_germanic) ->
        (* L.661 'tagliaro' *)
        Buffer.add_char primary 'K';
        add_both 'L';
        debug [%here];
        advance 2
      | "G"
        when !current = 0
             && (get_at (!current + 1) =* "Y"
                || string_at (!current + 1)
                     [| "ES"; "EP"; "EB"; "EL"; "EY"; "IB"; "IL"; "IN"; "IE"; "EI"; "ER" |]
                ) ->
        (* L.671 -ges-,-gep-,-gel-, -gie- at beginning *)
        add 'K' 'J';
        debug [%here];
        advance 2
      | "G"
        when (string_at (!current + 1) [| "ER" |] || get_at (!current + 1) =* "Y")
             && (not (string_at 0 [| "DANGER"; "RANGER"; "MANGER" |]))
             && (not (string_at (!current - 1) [| "E"; "I" |]))
             && not (string_at (!current - 1) [| "RGY"; "OGY" |]) ->
        (* L.684 -ger-,  -gy- *)
        add 'K' 'J';
        debug [%here];
        advance 2
      | "G"
        when string_at (!current + 1) [| "E"; "I"; "Y" |] || string_at (!current - 1) [| "AGGI"; "OGGI" |]
        ->
        (* L.700 italian e.g, 'biaggi' *)
        (match () with
        | _ when string_at 0 [| "VAN "; "VON "; "SCH" |] || string_at (!current + 1) [| "ET" |] ->
          (* L.705 obvious germanic *)
          add_both 'K'
        | _ when string_at (!current + 1) [| "IER" |] && !current + 3 = last ->
          (* L.716 always soft if french ending *)
          add_both 'J'
        | _ -> add 'J' 'K');
        debug [%here];
        advance 2
      | "G" ->
        add_both 'K';
        debug [%here];
        advance (if get_at (!current + 1) =* "G" then 2 else 1)
      | "H" when (!current = 0 || is_vowel (!current - 1)) && is_vowel (!current + 1) ->
        (* L.742 only keep if first & before vowel or btw. 2 vowels *)
        add_both 'H';
        debug [%here];
        advance 2
      | "H" ->
        (* L.751 also takes care of 'HH' *)
        debug [%here];
        advance 1
      | "J" when string_at !current [| "JOSE" |] || string_at 0 [| "SAN" |] ->
        (* L.756 obvious spanish, 'jose', 'san jacinto' *)
        let p = if (!current = 0 && !current + 3 = last) || string_at 0 [| "SAN" |] then 'H' else 'J' in
        add p 'H';
        debug [%here];
        advance 1
      | "J" ->
        (match !current with
        | i when i = 0 && not (string_at i [| "JOSE" |]) ->
          (* L.779 Yankelovich/Jankelowicz *)
          add 'J' 'A'
        | i
          when is_vowel (i - 1)
               && (not (force is_slavo_germanic))
               && (get_at (i + 1) =* "A" || get_at (i + 1) =* "O") ->
          (* L.784 spanish pron. of e.g. 'bajador' *)
          add 'J' 'H'
        | i when i = last -> Buffer.add_char primary 'J'
        | i
          when (not (string_at (i + 1) [| "L"; "T"; "K"; "S"; "N"; "M"; "B"; "Z" |]))
               && not (string_at (i - 1) [| "S"; "K"; "L" |]) ->
          add_both 'J'
        | _ -> ());
        debug [%here];
        advance (if get_at (!current + 1) =* "J" then 2 else 1)
      | "K" ->
        add_both 'K';
        debug [%here];
        advance (if get_at (!current + 1) =* "K" then 2 else 1)
      | "L"
        when get_at (!current + 1) =* "L"
             && (!current = Array.length glyphs - 3
                 && string_at (!current - 1) [| "ILLO"; "ILLA"; "ALLE" |]
                || (string_at (last - 1) [| "AS"; "OS" |] || string_at last [| "A"; "O" |])
                   && string_at (!current - 1) [| "ALLE" |]
                ) ->
        (* L.832 spanish e.g. 'cabrillo', 'gallegos' *)
        Buffer.add_char primary 'L';
        debug [%here];
        advance 2
      | "L" ->
        add_both 'L';
        debug [%here];
        advance (if get_at (!current + 1) =* "L" then 2 else 1)
      | "M" ->
        add_both 'M';
        debug [%here];
        advance
          ( if string_at (!current - 1) [| "UMB" |]
               (* L.858 'dumb','thumb' *)
               && (!current + 1 = last || string_at (!current + 2) [| "ER" |])
               || get_at (!current + 1) =* "M"
          then 2
          else 1
          )
      | "N" ->
        add_both 'N';
        debug [%here];
        advance (if get_at (!current + 1) =* "N" then 2 else 1)
      | "Ñ" ->
        add_both 'N';
        debug [%here];
        advance 1
      | "P" when get_at (!current + 1) =* "H" ->
        add_both 'F';
        debug [%here];
        advance 2
      | "P" ->
        (* L.891 also account for "campbell", "raspberry" *)
        add_both 'P';
        debug [%here];
        advance (if string_at (!current + 1) [| "P"; "B" |] then 2 else 1)
      | "Q" ->
        add_both 'K';
        debug [%here];
        advance (if get_at (!current + 1) =* "Q" then 2 else 1)
      | "R" ->
        if not
             (!current = last
             && (not (force is_slavo_germanic))
             && string_at (!current - 2) [| "IE" |]
             && not (string_at (!current - 4) [| "ME"; "MA" |])
             )
        then (* L.910 french e.g. 'rogier', but exclude 'hochmeier' *)
          Buffer.add_char primary 'R';
        Buffer.add_char secondary 'R';
        debug [%here];
        advance (if get_at (!current + 1) =* "R" then 2 else 1)
      | "S" when string_at (!current - 1) [| "ISL"; "YSL" |] ->
        (* L.932 special cases 'island', 'isle', 'carlisle', 'carlysle' *)
        debug [%here];
        advance 1
      | "S" when !current = 0 && string_at 0 [| "SUGAR" |] ->
        (* L.939 special case 'sugar-' *)
        add 'X' 'S';
        debug [%here];
        advance 1
      | "S" when string_at !current [| "SH" |] ->
        (* L.951 germanic *)
        add_both (if string_at (!current + 1) [| "HEIM"; "HOEK"; "HOLM"; "HOLZ" |] then 'S' else 'X');
        debug [%here];
        advance 2
      | "S" when string_at !current [| "SIO"; "SIA"; "SIAN" |] ->
        (* L.968 italian & armenian *)
        let s = if not (force is_slavo_germanic) then 'X' else 'S' in
        add 'S' s;
        debug [%here];
        advance 3
      | "S"
        when (!current = 0 && string_at (!current + 1) [| "M"; "N"; "L"; "W" |])
             || string_at (!current + 1) [| "Z" |] ->
        (* L.987 german & anglicisations, e.g. 'smith' match 'schmidt',
           'snider' match 'schneider' also, -sz- in slavic language
           although in hungarian it is pronounced 's' *)
        add 'S' 'X';
        debug [%here];
        advance (if string_at (!current + 1) [| "Z" |] then 2 else 1)
      | "S" when string_at !current [| "SC" |] -> (
        (* L.1007 Schlesinger's rule *)
        match !current with
        | i when get_at (i + 2) =* "H" ->
          (* L.1010 dutch origin, e.g. 'school', 'schooner' *)
          (match string_at (i + 3) [| "OO"; "ER"; "EN"; "UY"; "ED"; "EM" |] with
          | true ->
            if string_at (i + 3) [| "ER"; "EN" |]
            then (* L.1015 'schermerhorn', 'schenker' *)
              Buffer.add_char primary 'X'
            else Buffer.add_string primary "SK";
            Buffer.add_string secondary "SK"
          | false ->
            let s = if i = 0 && (not (is_vowel 3)) && get_at 3 <>* "W" then 'S' else 'X' in
            add 'X' s);
          debug [%here];
          advance 3
        | i when string_at (i + 2) [| "I"; "E"; "Y" |] ->
          add_both 'S';
          debug [%here];
          advance 3
        | _ ->
          add_both 'S';
          add_both 'K';
          debug [%here];
          advance 3
      )
      | "S" ->
        if !current = last && string_at (!current - 2) [| "AI"; "OI" |]
        then (* L.1063 french e.g. 'resnais', 'artois' *)
          Buffer.add_char secondary 'S'
        else add_both 'S';
        debug [%here];
        advance (if string_at (!current + 1) [| "S"; "Z" |] then 2 else 1)
      | "T" when string_at !current [| "TION"; "TIA"; "TCH" |] ->
        add_both 'X';
        debug [%here];
        advance 3
      | "T" when string_at !current [| "TH"; "TTH" |] ->
        let p =
          (* L.1102 special case 'thomas', 'thames' or germanic *)
          if string_at (!current + 2) [| "OM"; "AM" |] || string_at 0 [| "VAN "; "VON "; "SCH" |]
          then 'T'
          else '0'
        in
        add p 'T';
        debug [%here];
        advance 2
      | "T" ->
        add_both 'T';
        debug [%here];
        advance (if string_at (!current + 1) [| "T"; "D" |] then 2 else 1)
      | "V" ->
        add_both 'F';
        debug [%here];
        advance (if get_at (!current + 1) =* "V" then 2 else 1)
      | "W" when string_at !current [| "WR" |] ->
        (* L.1137 can also be in middle of word *)
        add_both 'R';
        debug [%here];
        advance 2
      | "W" -> (
        (match
           ( !current = 0 && (is_vowel (!current + 1) || string_at !current [| "WH" |]),
             is_vowel (!current + 1) )
         with
        | true, true ->
          (* L.1150 Wasserman should match Vasserman *)
          add 'A' 'F'
        | true, false ->
          (* L.1158 need Uomo to match Womo *)
          add_both 'A'
        | false, _ -> ());
        match !current with
        | i
          when (i = last && is_vowel (i - 1))
               || string_at (i - 1) [| "EWSKI"; "EWSKY"; "OWSKI"; "OWSKY" |]
               || string_at 0 [| "SCH" |] ->
          (* L.1164 Arnow should match Arnoff *)
          Buffer.add_char secondary 'F';
          debug [%here];
          advance 1
        | i when string_at i [| "WICZ"; "WITZ" |] ->
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
      | "X" ->
        if not
             (!current = last
             && (string_at (!current - 3) [| "IAU"; "EAU" |] || string_at (!current - 2) [| "AU"; "OU" |])
             )
        then (
          (* L.1190 french e.g. breaux *)
          Buffer.add_string primary "KS";
          Buffer.add_string secondary "KS"
        );
        debug [%here];
        advance (if string_at (!current + 1) [| "C"; "X" |] then 2 else 1)
      | "Z" when get_at (!current + 1) =* "H" ->
        (* L.1209 chinese pinyin e.g. 'zhao' *)
        add_both 'J';
        debug [%here];
        advance 2
      | "Z" ->
        if string_at (!current + 1) [| "ZO"; "ZI"; "ZA" |]
           || (force is_slavo_germanic && !current > 0 && get_at (!current - 1) <>* "T")
        then Buffer.add_char secondary 'T';
        add_both 'S';
        debug [%here];
        advance (if get_at (!current + 1) =* "Z" then 2 else 1)
      | _ ->
        debug [%here];
        advance 1);
      loop ()
    end
  in
  loop ();
  let finalize b = Buffer.To_string.sub b ~pos:0 ~len:(min max_length (Buffer.length b)) in
  finalize primary, finalize secondary
