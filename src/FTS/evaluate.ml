open! Core_kernel

let fold_neighbor ~gap ll =
  let rec loop = function
    | None, [] -> Int.Map.empty
    | Some acc, [] -> acc
    | None, first :: rest -> if Int.Map.is_empty first then first else loop (Some first, rest)
    | Some acc, first :: rest ->
      let positions =
        Int.Map.filter_mapi acc ~f:(fun ~key:position ~data:width ->
          Int.Map.find first (position + width + gap - 1) |> Option.map ~f:(( + ) width)
        )
      in
      if Int.Map.is_empty positions then positions else loop (Some positions, rest)
  in
  loop (None, ll)

let union a b =
  Int.Map.merge a b ~f:(fun ~key -> function
    | `Left x
     |`Right x ->
      Some x
    | `Both (x, y) when x = y -> Some x
    | `Both (x, y) ->
      failwithf "Invalid union. Position: %d. Widths: %d, %d. Please report this bug." key x y ()
  )

let intersect a b =
  Int.Map.merge a b ~f:(fun ~key -> function
    | `Left _
     |`Right _ ->
      None
    | `Both (x, y) when x = y -> Some x
    | `Both (x, y) ->
      failwithf "Invalid intersect. Position: %d. Widths: %d, %d. Please report this bug." key x y ()
  )

let rec get_positions (btree : Tsvector.btree) : Tsquery.t -> int Int.Map.t = function
| Token s -> (
  match String.Map.find btree s with
  | Some (`Indexed x) -> x
  | Some `Unindexed
   |None ->
    Int.Map.empty
)
| Prefix prefix -> (
  String.Map.binary_search btree `Last_equal_to prefix ~compare:(fun ~key ~data:_ x ->
    match [%compare: string] key x with
    | 0 -> 0
    | c when c < 0 -> c
    | _ when String.is_prefix ~prefix key -> 0
    | c -> c
  )
  |> function
  | None -> Int.Map.empty
  | Some (last, _) ->
    String.Map.fold_range_inclusive btree ~min:prefix ~max:last ~init:Int.Map.empty
      ~f:(fun ~key:_ ~data acc ->
      match data with
      | `Unindexed -> acc
      | `Indexed data -> union data acc
    )
)
| Clause (_, []) -> Int.Map.empty
| Clause (OR, ll) -> List.map ll ~f:(get_positions btree) |> List.reduce_exn ~f:union
| Clause (AND, ll) -> List.map ll ~f:(get_positions btree) |> List.reduce_exn ~f:intersect
| Clause (NEIGHBOR gap, ll) -> fold_neighbor ~gap (List.map ll ~f:(get_positions btree))

let get_btree (vector : Tsvector.t) : Tsvector.btree =
  match vector with
  | { btree = Some x; _ } -> x
  | { btree = None; seq; _ } ->
    let _, map =
      Sequence.fold seq ~init:(0, String.Map.empty) ~f:(fun (i, map) -> function
        | Token key ->
          let acc =
            String.Map.update map key ~f:(function
              | Some (`Indexed _ as x) -> x
              | None
               |Some `Unindexed ->
                `Unindexed
              )
          in
          i, acc
        | Indexed (key, _) ->
          let acc =
            String.Map.update map key ~f:(function
              | None
               |Some `Unindexed ->
                `Indexed (Int.Map.singleton i 1)
              | Some (`Indexed acc) -> `Indexed (Int.Map.set acc ~key:i ~data:1)
              )
          in
          i + 1, acc
      )
    in
    vector.btree <- Some map;
    map

let matches vector root_query =
  let btree = get_btree vector in
  let rec loop : Tsquery.t -> bool = function
    | Token s -> String.Map.mem btree s
    | Prefix prefix ->
      String.Map.binary_search btree
        ~compare:(fun ~key ~data:_ x -> [%compare: string] key x)
        `First_greater_than_or_equal_to prefix
      |> Option.value_map ~default:false ~f:(fun (x, _) -> String.is_prefix ~prefix x)
    | Clause (AND, ll) -> List.for_all ll ~f:loop
    | Clause (OR, ll) -> List.exists ll ~f:loop
    | Clause (NEIGHBOR _, []) -> true
    | Clause (NEIGHBOR _, _) as query -> get_positions btree query |> Int.Map.is_empty |> not
  in
  loop root_query
