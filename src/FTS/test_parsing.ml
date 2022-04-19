open! Core

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

let%expect_test "Tsquery" =
  let test raw =
    let result = Parsing.tsquery raw in
    sprintf
      !"%s\n%{sexp: (Tsquery.t, string) Result.t}"
      (Result.ok result |> Option.bind ~f:Tsquery.to_string |> Option.value ~default:"---")
      result
    |> print_endline
  in
  test "abc";
  [%expect {|
    'abc'
    (Ok (Token abc)) |}];
  test "abc|def";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test "'abc'|def";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test "abc|'def'";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test "'abc'|'def'";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test " 'abc'|'def'";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test "  'abc'|'def'";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test "  abc | 'def'";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test "  abc | 'def'  ";
  [%expect {|
    'abc' | 'def'
    (Ok (Clause OR ((Token abc) (Token def)))) |}];
  test "abc | 'def ' | ghi";
  [%expect {|
    'abc' | 'def ' | 'ghi'
    (Ok (Clause OR ((Token abc) (Token "def ") (Token ghi)))) |}];
  test "abc & def & ghi";
  [%expect {|
    'abc' & 'def' & 'ghi'
    (Ok (Clause AND ((Token abc) (Token def) (Token ghi)))) |}];
  test " abc | def & ghi ";
  [%expect
    {|
    ('abc' | 'def') & 'ghi'
    (Ok (Clause AND ((Clause OR ((Token abc) (Token def))) (Token ghi)))) |}];
  test " abc | def & ghi | jkl ";
  [%expect
    {|
    ('abc' | 'def') & ('ghi' | 'jkl')
    (Ok
     (Clause AND
      ((Clause OR ((Token abc) (Token def)))
       (Clause OR ((Token ghi) (Token jkl)))))) |}];
  test " abc & def | ghi & jkl ";
  [%expect
    {|
    'abc' & ('def' | 'ghi') & 'jkl'
    (Ok
     (Clause AND ((Token abc) (Clause OR ((Token def) (Token ghi))) (Token jkl)))) |}];
  test "abc <-> def <-> ghi <-> jkl | mno";
  [%expect
    {|
    'abc' <-> 'def' <-> 'ghi' <-> ('jkl' | 'mno')
    (Ok
     (Clause (NEIGHBOR 1)
      ((Token abc) (Token def) (Token ghi) (Clause OR ((Token jkl) (Token mno)))))) |}];
  test "abc <3> def <-> ghi <0> jkl";
  [%expect
    {|
    ---
    (Error
     ": Invalid distance in 'followed by' operator: expected '-' or an integer. Ex.: <->, <2>, <3>, etc.") |}];
  test "abc | (def <2> 'ghi') & xyz";
  [%expect
    {|
    ('abc' | ('def' <2> 'ghi')) & 'xyz'
    (Ok
     (Clause AND
      ((Clause OR ((Token abc) (Clause (NEIGHBOR 2) ((Token def) (Token ghi)))))
       (Token xyz)))) |}]
