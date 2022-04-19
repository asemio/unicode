open! Core_kernel
open Basic

type t = {
  seq: lexeme Sequence.t;
  last: int option Lazy.t;
  btree: (Btree.t Lazy.t[@sexp.opaque]);
}
[@@deriving sexp_of]

let get_last seq =
  Sequence.fold seq ~init:None ~f:(fun acc x ->
    match x, acc with
    | Token _, x -> x
    | Indexed (_, ll), None -> List.reduce ll ~f:max
    | Indexed (_, ll), Some y -> List.reduce (y :: ll) ~f:max
  )

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
        Token (sprintf !"%{Field.name}=%{to_string}" f x) :: acc
      )

  let skip : ('r, 'a) combinator = (fun acc _f _r _x -> acc)

  let string : ('r, 'a) combinator = (fun acc f r x -> single Fn.id acc f r x)

  let use parser : ('r, 'a) combinator =
   (fun acc _f _r x -> Sequence.fold (parser x).seq ~init:acc ~f:(fun acc x -> x :: acc))

  let bool v : ('r, 'a) combinator = (fun acc _f _r x -> if x then Token v :: acc else acc)

  let opt to_string : ('r, 'a) combinator =
   fun acc f _r x ->
    Option.value_map x ~default:acc ~f:(fun v ->
      Token (sprintf "%s=%s" (Field.name f) (to_string v)) :: acc
    )

  let create fold =
    let seq = fold ~init:[] |> Sequence.of_list in
    { seq; last = lazy (get_last seq); btree = lazy (Btree.of_seq seq) }
end

let make seq = Sequence.folding_map seq ~init:1 ~f:(fun acc s -> acc + 1, Indexed (s, [ acc ]))

let create seq =
  let seq = make seq in
  { seq; last = lazy (get_last seq); btree = lazy (Btree.of_seq seq) }

let create_trigrams seq =
  let seq = Sequence.bind seq ~f:Tokenizers.trigram |> make in
  { seq; last = lazy (get_last seq); btree = lazy (Btree.of_seq seq) }

let concat left right =
  let seq =
    (match force left.last with
    | Some last ->
      Sequence.map right.seq ~f:(function
        | Token _ as x -> x
        | Indexed (x, ll) -> Indexed (x, List.map ll ~f:(( + ) last))
        )
    | None -> right.seq)
    |> Sequence.append left.seq
  in
  { seq; last = lazy (get_last seq); btree = lazy (Btree.of_seq seq) }

let name raw = Parsers.name raw |> create

let tag raw = Parsers.tag raw |> create

let english_trigrams raw = Parsers.english raw |> create_trigrams

let tag_trigrams raw = Parsers.tag raw |> create_trigrams

let to_string { seq; _ } =
  Sequence.map seq ~f:(function
    | Token s -> Tsquery.quote s
    | Indexed (s, ll) ->
      sprintf !"%{Tsquery.quote}:%s" s (List.map ll ~f:Int.to_string |> String.concat ~sep:",")
    )
  |> Sequence.to_array
  |> String.concat_array ~sep:" "
