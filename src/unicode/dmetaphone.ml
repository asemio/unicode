open! Core

let preserve = function
| "ç"
 |"Ç"
 |"ñ"
 |"Ñ" ->
  true
| _ -> false

let rec slavo_germanic = function
| [] -> false
| ('W' | 'K') :: _
 |'C' :: 'Z' :: _ ->
  true
| _ :: rest -> slavo_germanic rest

let obvious_germanic = function
| 'V' :: ('A' | 'O') :: 'N' :: ' ' :: _
 |'S' :: 'C' :: 'H' :: _ ->
  true
| _ -> false

let rec spanish_special = function
| [ ('A' | 'O'); 'S' ]
 |[ _; ('A' | 'O') ] ->
  true
| [] -> false
| _ :: rest -> (spanish_special [@tailcall]) rest

(** Takes (int * (char list * char list))
    The int is how many characters to advance.
    The two lists are the next and previous characters in the string.
    Advancing (['b';'c';'d'], ['a']) by 1 returns (['c';'d'], ['b';'a'])
*)
let rec prepare = function
| 0, acc
 |_, (([], _) as acc) ->
  acc
| i, (x :: rest, dropped) -> prepare (i - 1, (rest, x :: dropped))

let double_metaphone ?(max_length = 4) ~glyphs ~num_glyphs () =
  (* It's not safe to index into the original string because ç and ñ are multibyte *)
  let is_obvious_germanic = lazy (obvious_germanic glyphs) in
  let is_slavo_germanic = lazy (slavo_germanic glyphs) in
  let is_spanish_special = lazy (spanish_special glyphs) in
  let primary = Buffer.create (max_length + 2) in
  let secondary = Buffer.create (max_length + 2) in
  let add p s =
    Buffer.add_char primary p;
    Buffer.add_char secondary s
  in
  let add_both c = add c c in

  let rec loop i pair =
    (* let debug (here : Source_code_position.t) =
         print_endline
           (sprintf !"[%d] %d %{sexp: string * string}" here.pos_lnum i
              (Buffer.contents primary, Buffer.contents secondary) )
       in *)
    let debug _ = () in
    let advance =
      match pair, glyphs, is_slavo_germanic with
      | (('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _, []), _, _ ->
        debug [%here];
        add_both 'A';
        1
      | (('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _, _), _, _ ->
        debug [%here];
        1
      | ('B' :: 'B' :: _, _), _, _ ->
        debug [%here];
        add_both 'P';
        2
      | ('B' :: _, _), _, _ ->
        debug [%here];
        add_both 'P';
        1
      | ('\x03' :: _, _), _, _ ->
        (* '0x03' == 'Ç' *)
        debug [%here];
        add_both 'S';
        1
      | ('C' :: 'H' :: 'A' :: 'E' :: _, 'A' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _), _, _
       |('C' :: 'H' :: 'E' :: 'R' :: _, 'A' :: ('M' | 'B') :: _), _, _
       |('C' :: 'H' :: 'I' :: 'A' :: _, 'A' :: _ :: _), _, _
       |( ( 'C' :: 'H' :: ('T' | 'S' | 'L' | 'R' | 'N' | 'M' | 'B' | 'H' | 'F' | 'V' | 'W' | ' ') :: _,
            'A' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _ ),
          _,
          _ )
       |([ 'C'; 'H' ], 'A' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _), _, _
       |( ('C' :: 'H' :: _, 'A' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _),
          ('V' :: ('A' | 'O') :: 'N' :: ' ' :: _ | 'S' :: 'C' :: 'H' :: _ | 'M' :: 'C' :: _),
          _ )
       |( ('C' :: 'H' :: ('I' | 'E') :: _, 'A' :: _ :: _),
          ('V' :: ('A' | 'O') :: 'N' :: ' ' :: _ | 'S' :: 'C' :: 'H' :: _ | 'M' :: 'C' :: _),
          _ ) ->
        debug [%here];
        add_both 'K';
        2
      | ('C' :: 'H' :: _, 'A' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _), _, _
       |('C' :: 'H' :: ('I' | 'E') :: _, 'A' :: _ :: _), _, _ ->
        debug [%here];
        add 'X' 'K';
        2
      | ('C' :: 'H' :: _, 'A' :: _ :: _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('C' :: 'H' :: 'A' :: 'E' :: _, _ :: _), _, _ ->
        debug [%here];
        add 'K' 'X';
        2
      | ('C' :: 'H' :: 'O' :: 'R' :: 'E' :: _, []), _, _ ->
        debug [%here];
        add_both 'X';
        2
      | ('C' :: 'H' :: 'A' :: 'R' :: 'A' :: 'C' :: _, []), _, _
       |('C' :: 'H' :: 'A' :: 'R' :: 'I' :: 'S' :: _, []), _, _
       |('C' :: 'H' :: 'O' :: 'R' :: _, []), _, _
       |('C' :: 'H' :: ('Y' | 'E') :: 'M' :: _, []), _, _
       |('C' :: 'H' :: 'I' :: 'A' :: _, _), _, _
       |('C' :: 'H' :: 'E' :: 'S' :: _, 'R' :: 'O' :: _), _, _
       |('C' :: 'H' :: 'I' :: 'T' :: _, 'R' :: 'A' :: _), _, _
       |('C' :: 'H' :: 'I' :: 'D' :: _, 'R' :: 'O' :: _), _, _
       |('C' :: 'H' :: ('T' | 'S') :: _, _), _, _
       |( ( 'C' :: 'H' :: ('L' | 'R' | 'N' | 'M' | 'B' | 'H' | 'F' | 'V' | 'W' | ' ') :: _,
            ([] | ('A' | 'O' | 'U' | 'E') :: _) ),
          _,
          _ )
       |([ 'C'; 'H' ], ([] | ('A' | 'O' | 'U' | 'E') :: _)), _, _
       |('C' :: 'H' :: _, _), 'V' :: ('A' | 'O') :: 'N' :: ' ' :: _, _
       |('C' :: 'H' :: _, _), 'S' :: 'C' :: 'H' :: _, _
       |('C' :: 'H' :: _, _ :: _), 'M' :: 'C' :: _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('C' :: 'H' :: _, []), _, _ ->
        debug [%here];
        add_both 'X';
        2
      | ('C' :: 'H' :: _, _ :: _), _, _ ->
        debug [%here];
        add 'X' 'K';
        2
      | ('C' :: 'C' :: 'I' :: 'A' :: _, _), _, _ ->
        debug [%here];
        add_both 'X';
        3
      | ('C' :: 'Z' :: _, 'I' :: 'W' :: _), _, _
       |('C' :: 'C' :: ('E' | 'I') :: _, [ 'M' ]), _, _ ->
        debug [%here];
        add_both 'K';
        1
      | ('C' :: 'C' :: _, [ 'M' ]), _, _
       |('C' :: 'C' :: 'H' :: 'U' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('C' :: 'C' :: ('I' | 'E' | 'H') :: _, [ 'A' ]), _, _
       |('C' :: 'C' :: 'E' :: ('E' | 'S') :: _, 'U' :: _), _, _ ->
        debug [%here];
        add_both 'K';
        add_both 'S';
        3
      | ('C' :: 'C' :: ('I' | 'E' | 'H') :: _, _), _, _ ->
        debug [%here];
        add_both 'X';
        3
      | ('C' :: ('C' | 'K' | 'G' | 'Q') :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('C' :: 'Z' :: _, _), _, _
       |('C' :: 'I' :: ('O' | 'E' | 'A') :: _, _), _, _ ->
        debug [%here];
        add 'S' 'X';
        2
      | ('C' :: 'A' :: 'E' :: 'S' :: 'A' :: 'R' :: _, []), _, _
       |('C' :: ('I' | 'E' | 'Y') :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        2
      | ('C' :: ' ' :: ('C' | 'Q' | 'G') :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        3
      | ('C' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        1
      | ('D' :: 'G' :: ('I' | 'E' | 'Y') :: _, _), _, _ ->
        debug [%here];
        add_both 'J';
        3
      | ('D' :: 'G' :: _, _), _, _ ->
        debug [%here];
        add_both 'T';
        add_both 'K';
        2
      | ('D' :: ('T' | 'D') :: _, _), _, _ ->
        debug [%here];
        add_both 'T';
        2
      | ('D' :: _, _), _, _ ->
        debug [%here];
        add_both 'T';
        1
      | ('F' :: 'F' :: _, _), _, _ ->
        debug [%here];
        add_both 'F';
        2
      | ('F' :: _, _), _, _ ->
        debug [%here];
        add_both 'F';
        1
      | ('G' :: 'H' :: 'I' :: _, []), _, _ ->
        debug [%here];
        add_both 'J';
        2
      | ('G' :: 'H' :: _, []), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('G' :: 'H' :: _, 'I' :: _), _, _ ->
        debug [%here];
        2
      | ('G' :: 'H' :: _, ('A' | 'E' | 'O' | 'U' | 'Y') :: ('B' | 'H' | 'D') :: _), _, _
       |('G' :: 'H' :: _, ('A' | 'E' | 'O' | 'U' | 'Y') :: _ :: ('B' | 'H' | 'D') :: _), _, _
       |('G' :: 'H' :: _, ('A' | 'E' | 'O' | 'U' | 'Y') :: _ :: _ :: ('B' | 'H') :: _), _, _ ->
        debug [%here];
        2
      | ('G' :: 'H' :: _, 'U' :: _ :: ('C' | 'G' | 'L' | 'R' | 'T') :: _), _, _ ->
        debug [%here];
        add_both 'F';
        2
      | ('G' :: 'H' :: _, _ :: _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('G' :: 'N' :: _, [ ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') ]), _, (lazy false) ->
        debug [%here];
        Buffer.add_char primary 'K';
        add_both 'N';
        2
      | ('G' :: 'N' :: 'E' :: 'Y' :: _, _), _, _
       |('G' :: 'N' :: _, _), _, (lazy true) ->
        debug [%here];
        add_both 'K';
        add_both 'N';
        2
      | ('G' :: 'N' :: _, _), _, _ ->
        debug [%here];
        Buffer.add_char secondary 'K';
        add_both 'N';
        2
      | ('G' :: 'L' :: 'I' :: _, _), _, (lazy false) ->
        debug [%here];
        Buffer.add_char primary 'K';
        add_both 'L';
        2
      | ('G' :: 'Y' :: _, []), _, _
       |('G' :: 'E' :: ('S' | 'P' | 'B' | 'L' | 'Y') :: _, []), _, _
       |('G' :: 'I' :: ('B' | 'L' | 'N' | 'E') :: _, []), _, _
       |('G' :: 'E' :: ('I' | 'R') :: _, []), _, _ ->
        debug [%here];
        add 'K' 'J';
        2
      | ('G' :: 'E' :: 'R' :: _, _), ('D' | 'R' | 'M') :: 'A' :: 'N' :: 'G' :: 'E' :: 'R' :: _, _
       |('G' :: 'E' :: 'R' :: _, ('E' | 'I') :: _), _, _
       |('G' :: 'Y' :: _, _), ('D' | 'R' | 'M') :: 'A' :: 'N' :: 'G' :: 'E' :: 'R' :: _, _
       |('G' :: 'Y' :: _, ('E' | 'I' | 'R' | 'O') :: _), _, _ ->
        debug [%here];
        add (if force is_obvious_germanic then 'K' else 'J') 'K';
        2
      | ('G' :: 'E' :: 'R' :: _, _), _, _
       |('G' :: 'Y' :: _, _), _, _ ->
        debug [%here];
        add 'K' 'J';
        2
      | ('G' :: 'E' :: 'T' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ([ 'G'; 'I'; 'E'; 'R' ], _), _, _
       |('G' :: 'I' :: 'E' :: 'R' :: ' ' :: _, _), _, _ ->
        debug [%here];
        add_both 'J';
        2
      | ('G' :: ('E' | 'I') :: _, _), _, _
       |('G' :: 'G' :: 'I' :: _, ('A' | 'O') :: _), _, _ ->
        debug [%here];
        add (if force is_obvious_germanic then 'K' else 'J') 'K';
        2
      | ('G' :: 'G' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('G' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        1
      | ( ( 'H' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _,
            ([] | ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _) ),
          _,
          _ ) ->
        debug [%here];
        add_both 'H';
        2
      | ('H' :: _, _), _, _ ->
        debug [%here];
        1
      | ('J' :: _, _), 'S' :: 'A' :: 'N' :: ' ' :: _, _
       |([ 'J'; 'O'; 'S'; 'E' ], []), _, _
       |('J' :: 'O' :: 'S' :: 'E' :: ' ' :: _, []), _, _ ->
        debug [%here];
        add_both 'H';
        1
      | ('J' :: 'O' :: 'S' :: 'E' :: _, _), _, _ ->
        debug [%here];
        add 'J' 'H';
        1
      | ('J' :: 'J' :: _, []), _, _ ->
        debug [%here];
        add 'J' 'A';
        2
      | ('J' :: _, []), _, _ ->
        debug [%here];
        add 'J' 'A';
        1
      | ([ 'J' ], _), _, _ ->
        debug [%here];
        Buffer.add_char primary 'J';
        1
      | ('J' :: ('A' | 'O') :: _, ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _), _, (lazy false) ->
        debug [%here];
        add 'J' 'H';
        1
      | ('J' :: 'J' :: _, ('S' | 'K' | 'L') :: _), _, _ ->
        debug [%here];
        2
      | ('J' :: ('L' | 'T' | 'K' | 'S' | 'N' | 'M' | 'B' | 'Z') :: _, _), _, _
       |('J' :: _, ('S' | 'K' | 'L') :: _), _, _ ->
        debug [%here];
        1
      | ('J' :: 'J' :: _, _), _, _ ->
        debug [%here];
        add_both 'J';
        2
      | ('J' :: _, _), _, _ ->
        debug [%here];
        add_both 'J';
        1
      | ('K' :: 'K' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('K' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        1
      | ([ 'L'; 'L'; ('O' | 'A') ], 'I' :: _), _, _
       |([ 'L'; 'L'; 'E' ], 'A' :: _), _, _ ->
        debug [%here];
        Buffer.add_char primary 'L';
        2
      | ('L' :: 'L' :: 'E' :: _, 'A' :: _), _, _ ->
        debug [%here];
        if force is_spanish_special then Buffer.add_char primary 'L' else add_both 'L';
        2
      | ('L' :: 'L' :: _, _), _, _ ->
        debug [%here];
        add_both 'L';
        2
      | ('L' :: _, _), _, _ ->
        debug [%here];
        add_both 'L';
        1
      | ([ 'M'; 'B' ], 'U' :: _), _, _
       |('M' :: 'B' :: 'E' :: 'R' :: _, 'U' :: _), _, _
       |('M' :: 'M' :: _, _), _, _ ->
        debug [%here];
        add_both 'M';
        2
      | ('M' :: _, _), _, _ ->
        debug [%here];
        add_both 'M';
        1
      | ('N' :: 'N' :: _, _), _, _ ->
        debug [%here];
        add_both 'N';
        2
      | ('N' :: _, _), _, _
       |('\x09' :: _, _), _, _ ->
        (* '0x09' == 'Ñ' *)
        debug [%here];
        add_both 'N';
        1
      | ('P' :: 'H' :: _, _), _, _ ->
        debug [%here];
        add_both 'F';
        2
      | ('P' :: ('P' | 'B') :: _, _), _, _ ->
        debug [%here];
        add_both 'P';
        2
      | ('P' :: _, _), _, _ ->
        debug [%here];
        add_both 'P';
        1
      | ('Q' :: 'Q' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        2
      | ('Q' :: _, _), _, _ ->
        debug [%here];
        add_both 'K';
        1
      | ([ 'R' ], 'E' :: 'I' :: ('E' | 'A') :: 'M' :: _), _, _ ->
        debug [%here];
        add_both 'R';
        1
      | ([ 'R' ], 'E' :: 'I' :: _), _, (lazy false) ->
        debug [%here];
        Buffer.add_char secondary 'R';
        1
      | ('R' :: 'R' :: _, _), _, _ ->
        debug [%here];
        add_both 'R';
        2
      | ('R' :: _, _), _, _ ->
        debug [%here];
        add_both 'R';
        1
      | ('S' :: 'L' :: _, ('I' | 'Y') :: _), _, _ ->
        debug [%here];
        1
      | ('S' :: 'U' :: 'G' :: 'A' :: 'R' :: _, []), _, _ ->
        debug [%here];
        add 'X' 'S';
        1
      | ('S' :: 'H' :: 'E' :: 'I' :: 'M' :: _, _), _, _
       |('S' :: 'H' :: 'O' :: 'E' :: 'K' :: _, _), _, _
       |('S' :: 'H' :: 'O' :: 'L' :: ('M' | 'Z') :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        2
      | ('S' :: 'H' :: _, _), _, _ ->
        debug [%here];
        add_both 'X';
        2
      | ('S' :: ('M' | 'N' | 'L' | 'W') :: _, []), _, _ ->
        debug [%here];
        add 'S' 'X';
        1
      | ('S' :: 'Z' :: _, _), _, _ ->
        debug [%here];
        add 'S' 'X';
        2
      | ('S' :: 'C' :: 'H' :: 'E' :: ('R' | 'N') :: _, _), _, _ ->
        debug [%here];
        Buffer.add_char primary 'X';
        Buffer.add_string secondary "SK";
        3
      | ('S' :: 'C' :: 'H' :: 'O' :: 'O' :: _, _), _, _
       |('S' :: 'C' :: 'H' :: 'U' :: 'Y' :: _, _), _, _
       |('S' :: 'C' :: 'H' :: 'E' :: ('D' | 'M') :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        add_both 'K';
        3
      | ('S' :: 'C' :: 'H' :: _, _ :: _), _, _
       |('S' :: 'C' :: 'H' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y' | 'W') :: _, _), _, _ ->
        debug [%here];
        add_both 'X';
        3
      | ('S' :: 'C' :: 'H' :: _, _), _, _ ->
        debug [%here];
        add 'X' 'S';
        3
      | ('S' :: 'I' :: ('O' | 'A') :: _, _), _, (lazy false) ->
        debug [%here];
        add 'S' 'X';
        3
      | ('S' :: 'I' :: ('O' | 'A') :: _, _), _, (lazy true)
       |('S' :: 'C' :: ('I' | 'E' | 'Y') :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        3
      | ('S' :: 'C' :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        add_both 'K';
        3
      | ([ 'S' ], 'I' :: ('A' | 'O') :: _), _, _ ->
        debug [%here];
        Buffer.add_char secondary 'S';
        1
      | ('S' :: 'S' :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        2
      | ('S' :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        1
      | ('T' :: 'I' :: 'O' :: 'N' :: _, _), _, _
       |('T' :: 'I' :: 'A' :: _, _), _, _
       |('T' :: 'C' :: 'H' :: _, _), _, _ ->
        debug [%here];
        add_both 'X';
        3
      | ('T' :: 'H' :: ('O' | 'A') :: 'M' :: _, _), _, _ ->
        debug [%here];
        add_both 'T';
        2
      | ('T' :: 'H' :: _, _), _, _
       |('T' :: 'T' :: 'H' :: _, _), _, _ ->
        debug [%here];
        add (if force is_obvious_germanic then 'T' else '0') 'T';
        2
      | ('T' :: ('T' | 'D') :: _, _), _, _ ->
        debug [%here];
        add_both 'T';
        2
      | ('T' :: _, _), _, _ ->
        debug [%here];
        add_both 'T';
        1
      | ('V' :: 'V' :: _, _), _, _ ->
        debug [%here];
        add_both 'F';
        2
      | ('V' :: _, _), _, _ ->
        debug [%here];
        add_both 'F';
        1
      | ('W' :: 'R' :: _, _), _, _ ->
        debug [%here];
        add_both 'R';
        2
      | ([ 'W' ], ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _), _, _
       |('W' :: 'S' :: 'K' :: ('I' | 'Y') :: _, ('E' | 'O') :: _), _, _
       |('W' :: _, _), 'S' :: 'C' :: 'H' :: _, _ ->
        debug [%here];
        Buffer.add_char secondary 'F';
        1
      | ('W' :: 'I' :: ('C' | 'T') :: 'Z' :: _, _), _, _ ->
        debug [%here];
        Buffer.add_string primary "TS";
        Buffer.add_string secondary "FX";
        4
      | ('W' :: _, _), _, _ ->
        debug [%here];
        1
      | ([ 'X' ], 'U' :: ('O' | 'A') :: _), _, _ ->
        debug [%here];
        1
      | ('X' :: ('C' | 'X') :: _, _), _, _ ->
        debug [%here];
        Buffer.add_string primary "KS";
        Buffer.add_string secondary "KS";
        2
      | ('X' :: _, _), _, _ ->
        debug [%here];
        Buffer.add_string primary "KS";
        Buffer.add_string secondary "KS";
        1
      | ('Z' :: 'H' :: _, _), _, _ ->
        debug [%here];
        add_both 'J';
        2
      | ('Z' :: 'Z' :: ('O' | 'I' | 'A') :: _, _), _, _ ->
        debug [%here];
        Buffer.add_char secondary 'T';
        add_both 'S';
        2
      | ('Z' :: 'Z' :: _, 'T' :: _), _, _ ->
        debug [%here];
        add_both 'S';
        2
      | ('Z' :: _, 'T' :: _), _, _ ->
        debug [%here];
        add_both 'S';
        1
      | ('Z' :: 'Z' :: _, _ :: _), _, (lazy true) ->
        debug [%here];
        Buffer.add_char secondary 'T';
        add_both 'S';
        2
      | ('Z' :: _, _ :: _), _, (lazy true) ->
        debug [%here];
        Buffer.add_char secondary 'T';
        add_both 'S';
        1
      | ('Z' :: 'Z' :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        2
      | ('Z' :: _, _), _, _ ->
        debug [%here];
        add_both 'S';
        1
      | (_ :: _, _), _, _ ->
        debug [%here];
        1
      | ([], _), _, _ -> 0
    in
    if (Buffer.length primary < max_length || Buffer.length secondary < max_length)
       && i + advance < num_glyphs
    then (loop [@tailcall]) (i + advance) (prepare (advance, pair))
  in

  let start_pos =
    match glyphs with
    | ('G' | 'K' | 'P') :: 'N' :: _
     |'W' :: 'R' :: _
     |'P' :: 'S' :: _ ->
      (* debug [%here]; *)
      1
    | 'X' :: _ ->
      add_both 'S';
      (* debug [%here]; *)
      1
    | 'W' :: ('A' | 'E' | 'I' | 'O' | 'U' | 'Y') :: _ ->
      add 'A' 'F';
      0
    | 'W' :: 'H' :: _ ->
      add_both 'A';
      0
    | _ -> 0
  in
  let pair = prepare (start_pos, (glyphs, [])) in
  loop start_pos pair;
  let finalize b = Buffer.To_string.sub b ~pos:0 ~len:(min max_length (Buffer.length b)) in
  finalize primary, finalize secondary
