open! Core_kernel

type lexeme =
  | Token   of string
  | Indexed of string
[@@deriving sexp, variants]

type entry =
  [ `Indexed   of int Int.Map.t
  | `Unindexed
  ]

type btree = entry String.Map.t

type t = {
  seq: lexeme Sequence.t;
  mutable btree: btree option;
}

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

  let create fold = { seq = fold ~init:[] |> Sequence.of_list; btree = None }
end

let make seq = Sequence.map seq ~f:indexed

let create seq = { seq = make seq; btree = None }

let create_trigrams seq = { seq = Sequence.bind seq ~f:Tokenizers.trigram |> make; btree = None }

let concat { seq = x; _ } { seq = y; _ } = { seq = Sequence.append x y; btree = None }

let name raw = Parsers.name raw |> create

let tag raw = Parsers.tag raw |> create

let english_trigrams raw = Parsers.english raw |> create_trigrams

let tag_trigrams raw = Parsers.tag raw |> create_trigrams

let to_string { seq; _ } =
  Sequence.folding_map seq ~init:1 ~f:(fun i -> function
    | Token s -> i, Tsquery.quote s
    | Indexed s -> i + 1, sprintf !"%{Tsquery.quote}:%d" s i
  )
  |> Sequence.to_array
  |> String.concat_array ~sep:" "
