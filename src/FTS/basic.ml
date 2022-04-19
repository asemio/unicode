open! Core_kernel

type lexeme =
  | Token   of string
  | Indexed of (string * int list)
[@@deriving sexp, variants]

module Btree
  (* : sig
       type t [@@deriving sexp_of]

       val of_seq : lexeme Sequence.t -> t
     end *) =
struct
  type entry =
    [ `Indexed   of int Int.Map.t
    | `Unindexed
    ]
  [@@deriving sexp_of]

  type t = entry String.Map.t [@@deriving sexp_of]

  let of_seq seq =
    Sequence.fold seq ~init:String.Map.empty ~f:(fun map -> function
      | Token key ->
        let acc =
          String.Map.update map key ~f:(function
            | Some (`Indexed _ as x) -> x
            | None
             |Some `Unindexed ->
              `Unindexed
            )
        in
        acc
      | Indexed (key, positions) ->
        let acc =
          String.Map.update map key ~f:(function
            | None
             |Some `Unindexed ->
              `Indexed
                (List.fold positions ~init:Int.Map.empty ~f:(fun acc key -> Int.Map.set acc ~key ~data:1))
            | Some (`Indexed init) ->
              `Indexed (List.fold positions ~init ~f:(fun acc key -> Int.Map.set acc ~key ~data:1))
            )
        in
        acc
    )
end
