open! Core_kernel

let%expect_test "Tsvector" =
  let test raw =
    raw |> Parsing.tsvector |> sprintf !"%{sexp: (Tsvector.t, string) Result.t}" |> print_endline
  in
  test "a:1 b:2 x";
  [%expect
    {|
    (Ok
     ((seq ((Indexed (a (1))) (Indexed (b (2))) (Token x))) (last ()) (btree ()))) |}];
  test "";
  [%expect {| (Ok ((seq ()) (last ()) (btree ()))) |}];
  test "aa:2 b:1 aa:5";
  [%expect
    {|
    (Ok
     ((seq ((Indexed (aa (2))) (Indexed (b (1))) (Indexed (aa (5))))) (last ())
      (btree ()))) |}];
  test "a'b";
  [%expect {| (Ok ((seq ((Token a'b))) (last ()) (btree ()))) |}];
  test "a  b";
  [%expect {| (Ok ((seq ((Token a) (Token b))) (last ()) (btree ()))) |}];
  test "a:382.2  b";
  [%expect {| (Error ": end_of_input") |}];
  test "a:1,2  b:3,2";
  [%expect {| (Ok ((seq ((Indexed (a (1 2))) (Indexed (b (3 2))))) (last ()) (btree ()))) |}];
  test "'abc'";
  [%expect {| (Ok ((seq ((Token abc))) (last ()) (btree ()))) |}];
  test "'a' b";
  [%expect {| (Ok ((seq ((Token a) (Token b))) (last ()) (btree ()))) |}];
  test "'ab ' x";
  [%expect {| (Ok ((seq ((Token "ab ") (Token x))) (last ()) (btree ()))) |}];
  test "'a''b' x";
  [%expect {| (Ok ((seq ((Token a'b) (Token x))) (last ()) (btree ()))) |}];
  test "'''' x'";
  [%expect {| (Ok ((seq ((Token ') (Token x'))) (last ()) (btree ()))) |}];
  test "a' b";
  [%expect {| (Ok ((seq ((Token a') (Token b))) (last ()) (btree ()))) |}];
  test " ";
  [%expect {| (Ok ((seq ()) (last ()) (btree ()))) |}];
  test " a' b'";
  [%expect {| (Ok ((seq ((Token a') (Token b'))) (last ()) (btree ()))) |}];
  test "a' '";
  [%expect {| (Ok ((seq ((Token a') (Token '))) (last ()) (btree ()))) |}];
  test "a' 'b";
  [%expect {| (Ok ((seq ((Token a') (Token b))) (last ()) (btree ()))) |}];
  test "a' 'b";
  [%expect {| (Ok ((seq ((Token a') (Token b))) (last ()) (btree ()))) |}];
  test "' a' '";
  [%expect {| (Ok ((seq ((Token " a") (Token '))) (last ()) (btree ()))) |}]
