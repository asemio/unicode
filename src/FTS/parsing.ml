open! Core_kernel
open Angstrom

let unquoted_string_parser =
  let stop_at = function
    | ' '
     |':' ->
      true
    | _ -> false
  in
  let buf = Buffer.create 16 in
  Angstrom.scan_state true (fun escaping -> function
    | '\'' when escaping -> Some false
    | '\'' as c ->
      Buffer.add_char buf c;
      Some true
    | c when stop_at c -> None
    | c ->
      Buffer.add_char buf c;
      Some false
  )
  >>= fun _ ->
  match Buffer.length buf with
  | 0 -> fail "Empty"
  | _ ->
    let s = Buffer.contents buf in
    Buffer.clear buf;
    return s

let quoted_string_parser =
  let buf = Buffer.create 16 in
  Angstrom.scan_state false (fun escaping c ->
    match c, escaping with
    | '\'', true ->
      Buffer.add_char buf c;
      Some false
    | '\'', false -> Some true
    | _, true -> None
    | c, false ->
      Buffer.add_char buf c;
      Some false
  )
  >>= fun _ ->
  let s = Buffer.contents buf in
  Buffer.clear buf;
  if String.is_empty s then return "'" else return s

let number_parser =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false
    )
  >>| Int.of_string

let maybe p = option None (p >>| Option.return)

let tsvector raw =
  let quoted_entry =
    choice
      [
        ( char '\'' *> quoted_string_parser >>| fun x ->
          (* print_endline "AAA"; *)
          x
        );
        ( unquoted_string_parser >>| fun x ->
          (* print_endline "BBB"; *)
          x
        );
      ]
  in
  let lexeme =
    let+ token = quoted_entry
    and+ positions = maybe (char ':' *> sep_by1 (char ',') number_parser) in
    match positions with
    | None -> Tsvector.Token token
    | Some pos -> Tsvector.Indexed (token, pos)
  in
  let spaces = skip_many (char ' ') in
  let spaces1 = skip_many1 (char ' ') in
  let parser =
    spaces *> sep_by spaces1 lexeme
    <* spaces
    >>| Tsvector.((fun lexemes -> { seq = Sequence.of_list lexemes; last = None; btree = None }))
  in
  parse_string ~consume:Consume.All parser raw

(* let tsquery raw = *)
