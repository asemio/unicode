open! Core_kernel
open Angstrom
open Basic

let unquoted_string_parser ~stop_at =
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
  >>= fun s ->
  let i = Int.of_string s in
  if Int.is_positive i then return i else fail (sprintf "Invalid position or distance: %d" i)

let distance_parser ~message:failure_msg = choice ~failure_msg [ number_parser; char '-' >>| const 1 ]

let maybe p = option None (p >>| Option.return)

let tsvector raw =
  let stop_at = function
    | ' '
     |':' ->
      true
    | _ -> false
  in
  let string_parser = char '\'' *> quoted_string_parser <|> unquoted_string_parser ~stop_at in
  let lexeme =
    let+ token = string_parser
    and+ positions = maybe (char ':' *> sep_by1 (char ',') number_parser) in
    match positions with
    | None -> Token token
    | Some pos -> Indexed (token, pos)
  in
  let spaces = skip_many (char ' ') in
  let spaces1 = skip_many1 (char ' ') in
  let parser =
    spaces *> sep_by spaces1 lexeme
    <* spaces
    >>| Tsvector.(
          fun lexemes ->
            let seq = Sequence.of_list lexemes in
            { seq; last = lazy (get_last seq); btree = lazy (Btree.of_seq seq) }
        )
  in
  parse_string ~consume:Consume.All parser raw

type folder = {
  andl: Tsquery.t list;
  orl: Tsquery.t list;
}
[@@deriving sexp]

let tsquery raw =
  let open Tsquery in
  let stop_at = function
    | ' '
     |':'
     |'|'
     |'&'
     |'('
     |')'
     |'<' ->
      true
    | _ -> false
  in
  let string_parser = char '\'' *> quoted_string_parser <|> unquoted_string_parser ~stop_at in
  let spaces = skip_many (char ' ') in
  let token_parser = string_parser >>| token in
  (* let sequence ~sep nested = spaces *> sep_by1 (spaces *> char sep <* spaces) nested <* spaces in *)
  let expr =
    fix (fun expr ->
      let parens = char '(' *> expr <* char ')' in
      let negation = char '!' *> spaces *> choice [ parens; token_parser ] >>| fun x -> NOT x in
      let sequence ~sep nested =
        let+ ll =
          many1
            (let+ one = spaces *> nested
             and+ () = spaces
             and+ sep = sep
             and+ () = spaces in
             one, sep)
        and+ last = nested <* spaces in
        List.append ll [ last, NEIGHBOR 0 ]
      in
      let op_list =
        let sep =
          let message =
            "Invalid distance in 'followed by' operator: expected '-' or an integer. Ex.: <->, <2>, <3>, \
             etc."
          in
          choice
            [
              (char '<' *> distance_parser ~message <* char '>' >>| fun x -> NEIGHBOR x);
              char '&' >>| const AND;
              char '|' >>| const OR;
            ]
        in
        sequence ~sep (choice [ negation; parens; token_parser ]) >>| fun ll ->
        let rec loop level acc : (t * op) list -> t * op * (t * op) list = function
          | (x, op) :: rest when [%equal: op] level op -> loop level (x :: acc) rest
          | (_, op) :: _ as ll when op << level ->
            let clause, op, rest = loop op [] ll in
            loop level acc ((clause, op) :: rest)
          | (x, op) :: rest -> Clause (level, List.rev (x :: acc)), op, rest
          | rest -> (
            match acc with
            | [ x ] -> x, level, rest
            | _ -> Clause (level, List.rev acc), level, rest
          )
        in
        let result =
          match loop (NEIGHBOR 0) [] ll with
          | x, NEIGHBOR 0, [] -> x
          | x -> failwithf !"Impossible case: %{sexp: t * op * (t * op) list}" x ()
        in
        result
      in
      choice [ op_list; negation; parens; token_parser ] <* spaces
    )
  in
  let parser = expr in
  parse_string ~consume:Consume.All parser raw
