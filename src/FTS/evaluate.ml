open! Core_kernel
open Basic

let fold_neighbor ~gap ll =
  let rec loop = function
    | None, [] -> Int.Map.empty
    | Some acc, [] -> acc
    | None, first :: rest -> if Int.Map.is_empty first then first else loop (Some first, rest)
    | Some acc, first :: rest ->
      (* TODO: replace by a merge *)
      let positions =
        Int.Map.filter_mapi acc ~f:(fun ~key:position ~data:width ->
          Int.Map.find first (position + width + gap - 1) |> Option.map ~f:(( + ) width)
        )
      in
      if Int.Map.is_empty positions then positions else loop (Some positions, rest)
  in
  loop (None, ll)

let union a b =
  (* print_endline (sprintf !"A: %{sexp: int Int.Map.t}" a); *)
  (* print_endline (sprintf !"B: %{sexp: int Int.Map.t}" b); *)
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

let rec get_positions (vector : Tsvector.t) : Tsquery.t -> int Int.Map.t = function
| Token s -> (
  match String.Map.find (force vector.btree) s with
  | Some (`Indexed x) -> x
  | Some `Unindexed
   |None ->
    Int.Map.empty
)
| Prefix prefix -> (
  String.Map.binary_search (force vector.btree) `Last_equal_to prefix ~compare:(fun ~key ~data:_ x ->
    match [%compare: string] key x with
    | 0 -> 0
    | c when c < 0 -> c
    | _ when String.is_prefix ~prefix key -> 0
    | c -> c
  )
  |> function
  | None -> Int.Map.empty
  | Some (last, _) ->
    String.Map.fold_range_inclusive (force vector.btree) ~min:prefix ~max:last ~init:Int.Map.empty
      ~f:(fun ~key:_ ~data acc ->
      match data with
      | `Unindexed -> acc
      | `Indexed data -> union data acc
    )
)
| Clause (_, []) -> Int.Map.empty
| Clause (OR, ll) ->
  (* print_endline (sprintf !"%{sexp: Btree.t}" (force vector.btree)); *)
  List.map ll ~f:(get_positions vector) |> List.reduce_exn ~f:union
(* | Clause (OR, ll) ->
  List.fold_until ll ~init:None ~finish:Fn.id ~f:(fun acc x ->
    match x with
    | NOT xx ->
      if get_positions xx |> Int.Map.is_empty
      then
  ) *)
| Clause (AND, ll) -> List.map ll ~f:(get_positions vector) |> List.reduce_exn ~f:intersect
| Clause (NEIGHBOR gap, ll) ->
  (* print_endline (sprintf !"ll: %{sexp: Tsquery.t list}" ll); *)
  let mapped = List.map ll ~f:(get_positions vector) in
  (* print_endline (sprintf !"MAPPED: %{sexp: int Int.Map.t list}" mapped); *)
  fold_neighbor ~gap mapped
| NOT (NOT sub) -> get_positions vector sub
| NOT sub ->
  (* In "all", the value is the number of occurences *)
  let all =
    String.Map.fold (force vector.btree) ~init:Int.Map.empty ~f:(fun ~key:_ ~data:entry acc ->
      match entry with
      | `Unindexed -> acc
      | `Indexed map ->
        Int.Map.fold map ~init:acc ~f:(fun ~key ~data:_ acc ->
          (* btree only contains widths of one *)
          Int.Map.update acc key ~f:(Option.value_map ~default:1 ~f:succ)
        )
    )
  in
  print_endline (sprintf !"ALL: %{sexp: int Int.Map.t}" all);
  let positions = get_positions vector sub in
  print_endline (sprintf !"POSITIONS: %{sexp: int Int.Map.t}" positions);
  if Int.Map.is_empty positions then all else Int.Map.empty
(* let returned =
     Int.Map.fold positions ~init:all ~f:(fun ~key ~data acc ->
       print_endline (sprintf !"Key: %d. Data: %d. X: %d" key data (key + data - 1));
       Int.Map.change acc
         (key + data - 1)
         ~f:(fun found ->
           match found, sub with
           | None, _ -> failwith "Impossible None case in NOT fold"
           | Some _, NOT _ -> failwith "Impossible NOT case in NOT fold"
           | Some _, Token _ -> None
           | Some _, Prefix _ -> None
           | Some 1, Clause _ -> None
           | Some x, Clause _ -> Some (x - 1))
     )
     |> Int.Map.map ~f:(const 1)
   in
   print_endline (sprintf !"RETURNED: %{sexp: int Int.Map.t}" returned);
   returned *)

let rec eval (vector : Tsvector.t) : Tsquery.t -> bool = function
| Token s -> String.Map.mem (force vector.btree) s
| NOT x -> not (eval vector x)
| Prefix prefix ->
  String.Map.binary_search (force vector.btree)
    ~compare:(fun ~key ~data:_ x -> [%compare: string] key x)
    `First_greater_than_or_equal_to prefix
  |> Option.value_map ~default:false ~f:(fun (x, _) -> String.is_prefix ~prefix x)
| Clause (AND, ll) -> List.for_all ll ~f:(eval vector)
| Clause (OR, ll) -> List.exists ll ~f:(eval vector)
| Clause (NEIGHBOR _, []) -> true
| Clause (NEIGHBOR _, _) as query -> get_positions vector query |> Int.Map.is_empty |> not

let matches vector root_query = eval vector root_query
