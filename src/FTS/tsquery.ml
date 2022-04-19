open! Core_kernel

type op =
  | OR
  | AND
  | NEIGHBOR of int
[@@deriving sexp, equal]

let ( << ) a b =
  match a, b with
  | OR, OR -> false
  | OR, AND
   |OR, NEIGHBOR _ ->
    true
  | AND, OR
   |AND, AND ->
    false
  | AND, NEIGHBOR _ -> true
  | NEIGHBOR _, NEIGHBOR 0 -> true
  | NEIGHBOR _, _ -> false

type t =
  | Token  of string
  | Prefix of string
  | Clause of op * t list
  | NOT    of t
[@@deriving sexp, variants]

let quote s =
  let buf = Buffer.create (String.length s + 5) in
  Buffer.add_char buf '\'';
  String.iter s ~f:(function
    | '\'' -> Buffer.add_string buf "''"
    | c -> Buffer.add_char buf c
    );
  Buffer.add_char buf '\'';
  Buffer.contents buf

let group op ~f seq = Sequence.map seq ~f |> Sequence.to_list |> clause op

module Record = struct
  type ('r, 'a) combinator = t list -> ('r, 'a) Field.t -> 'r -> 'a -> t list

  let symbol sexp_of x = sexp_of x |> Sexp.to_string

  let single to_string : ('r, 'a) combinator = (fun acc _f _r x -> Token (to_string x) :: acc)

  let multiple ?(op = OR) to_sequence to_string : ('r, 'a) combinator =
   fun acc f _r x ->
    let seq = to_sequence x in
    match Sequence.is_empty seq with
    | true -> acc
    | false -> (Sequence.map seq ~f:(sprintf !"%{Field.name}=%{to_string}" f) |> group op ~f:token) :: acc

  let skip : ('r, 'a) combinator = (fun acc _f _r _x -> acc)

  let string : ('r, 'a) combinator = (fun acc f r x -> single Fn.id acc f r x)

  let bool v : ('r, 'a) combinator = (fun acc _f _r x -> if x then Token v :: acc else acc)

  let opt to_string : ('r, 'a) combinator =
   fun acc f _r x ->
    Option.value_map x ~default:acc ~f:(fun v ->
      Token (sprintf "%s=%s" (Field.name f) (to_string v)) :: acc
    )

  let create ?(op = AND) fold = fold ~init:[] |> clause op
end

let create ~accept_prefix ~across_words seq =
  let f = if accept_prefix then prefix else token in
  seq |> group across_words ~f

let create_trigrams ~across_trigrams ~across_words seq =
  seq
  |> Sequence.map ~f:(fun word -> Tokenizers.trigram word |> group across_trigrams ~f:token)
  |> Sequence.to_list
  |> clause across_words

let name ~accept_prefix ~across_words raw = Parsers.name raw |> create ~accept_prefix ~across_words

let tag ~accept_prefix ~across_words raw = Parsers.tag raw |> create ~accept_prefix ~across_words

let english_trigrams ~across_trigrams ~across_words raw =
  Parsers.english raw |> create_trigrams ~across_trigrams ~across_words

let tag_trigrams ~across_trigrams ~across_words raw =
  Parsers.tag raw |> create_trigrams ~across_trigrams ~across_words

let to_string =
  let symbol_of_op = function
    | AND -> " & "
    | OR -> " | "
    | NEIGHBOR 1 -> " <-> "
    | NEIGHBOR n -> sprintf " <%d> " n
  in
  function
  | Clause (_, []) -> ""
  | x ->
    let rec loop depth = function
      | Token s -> quote s
      | Prefix s -> sprintf "%s:*" (loop depth (Token s))
      | NOT x -> sprintf "!%s" (loop (depth + 1) x)
      | Clause (_, [ x ]) -> loop depth x
      | Clause (op, ll) ->
        List.map ~f:(loop (depth + 1)) ll
        |> String.concat ~sep:(symbol_of_op op)
        |> if depth = 0 then Fn.id else sprintf "(%s)"
    in
    loop 0 x
