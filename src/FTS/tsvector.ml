open! Core

type lexeme =
  | Token   of string
  | Indexed of (string * int list)
[@@deriving sexp, variants]

type entry =
  [ `Indexed   of int Int.Map.t
  | `Unindexed
  ]
[@@deriving sexp_of]

type btree = entry String.Map.t [@@deriving sexp_of]

type t = {
  seq: lexeme Sequence.t;
  mutable last: int option;
  mutable btree: btree option;
}
[@@deriving sexp_of]

module Record = struct
  type ('r, 'a) combinator = lexeme list -> ('r, 'a) Field.t -> 'r -> 'a -> lexeme list

  let symbol sexp_of x = sexp_of x |> Sexp.to_string

  let single to_string : ('r, 'a) combinator = (fun acc _f _r x -> Token (to_string x) :: acc)

  let multiple to_sequence to_string : ('r, 'a) combinator =
   fun acc f _r x ->
    let seq = to_sequence x in
    match Sequence.is_empty seq with
    | true -> acc
    | false ->
      Sequence.fold seq ~init:acc ~f:(fun acc x ->
          Token (sprintf !"%{Field.name}=%{to_string}" f x) :: acc)

  let skip : ('r, 'a) combinator = (fun acc _f _r _x -> acc)

  let string : ('r, 'a) combinator = (fun acc f r x -> single Fn.id acc f r x)

  let use parser : ('r, 'a) combinator =
   (fun acc _f _r x -> Sequence.fold (parser x).seq ~init:acc ~f:(fun acc x -> x :: acc))

  let bool v : ('r, 'a) combinator = (fun acc _f _r x -> if x then Token v :: acc else acc)

  let opt to_string : ('r, 'a) combinator =
   fun acc f _r x ->
    Option.value_map x ~default:acc ~f:(fun v ->
        Token (sprintf "%s=%s" (Field.name f) (to_string v)) :: acc)

  let create fold = { seq = fold ~init:[] |> Sequence.of_list; last = None; btree = None }
end

let make seq = Sequence.folding_map seq ~init:1 ~f:(fun acc s -> acc + 1, Indexed (s, [ acc ]))

let create seq = { seq = make seq; last = None; btree = None }

let create_trigrams seq =
  { seq = Sequence.bind seq ~f:Tokenizers.trigram |> make; last = None; btree = None }

let concat left right =
  let last_left =
    match left.last with
    | None ->
      let last =
        Sequence.fold left.seq ~init:None ~f:(fun acc x ->
            match x, acc with
            | Token _, x -> x
            | Indexed (_, ll), None -> List.reduce ll ~f:max
            | Indexed (_, ll), Some y -> List.reduce (y :: ll) ~f:max)
      in
      left.last <- last;
      last
    | Some _ as x -> x
  in
  let seq =
    (match last_left with
    | Some last ->
      Sequence.map right.seq ~f:(function
        | Token _ as x -> x
        | Indexed (x, ll) -> Indexed (x, List.map ll ~f:(( + ) last)))
    | None -> right.seq)
    |> Sequence.append left.seq
  in
  { seq; last = None; btree = None }

let name raw = Parsers.name raw |> create

let tag raw = Parsers.tag raw |> create

let english_trigrams raw = Parsers.english raw |> create_trigrams

let tag_trigrams raw = Parsers.tag raw |> create_trigrams

let to_string { seq; _ } =
  Sequence.map seq ~f:(function
    | Token s -> Tsquery.quote s
    | Indexed (s, ll) ->
      sprintf !"%{Tsquery.quote}:%s" s (List.map ll ~f:Int.to_string |> String.concat ~sep:","))
  |> Sequence.to_array
  |> String.concat_array ~sep:" "
