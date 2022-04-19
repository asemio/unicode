open! Core

let%expect_test "Get positions" =
  let vector = Tsvector.tag "the quick foxy brown fox jumps over the lazy fox" in
  let test btree query =
    print_endline (Tsquery.to_string query |> Option.value ~default:"---");
    Evaluate.get_positions btree query |> sprintf !"%{sexp: int Int.Map.t}" |> print_endline
  in
  let btree = Evaluate.get_btree vector in
  test btree Tsquery.(Token "brown");
  [%expect {|
    'brown'
    ((3 1)) |}];
  test btree Tsquery.(Token "the");
  [%expect {|
    'the'
    ((0 1) (7 1)) |}];
  test btree Tsquery.(Token "br");
  [%expect {|
    'br'
    () |}];
  test btree Tsquery.(Prefix "brown");
  [%expect {|
    'brown':*
    ((3 1)) |}];
  test btree Tsquery.(Prefix "br");
  [%expect {|
    'br':*
    ((3 1)) |}];
  test btree Tsquery.(Prefix "fox");
  [%expect {|
    'fox':*
    ((2 1) (4 1) (9 1)) |}];
  test btree Tsquery.(Clause (OR, [ Prefix "fox"; Token "the" ]));
  [%expect {|
    'fox':* | 'the'
    ((0 1) (2 1) (4 1) (7 1) (9 1)) |}];
  test btree Tsquery.(Clause (AND, [ Prefix "fox"; Token "fox" ]));
  [%expect {|
    'fox':* & 'fox'
    ((4 1) (9 1)) |}];
  test btree Tsquery.(Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]));
  [%expect {|
    'fox':* <-> 'brown'
    ((2 2)) |}];
  test btree Tsquery.(Clause (NEIGHBOR 1, [ Token "quick"; Prefix "fox"; Token "brown" ]));
  [%expect {|
    'quick' <-> 'fox':* <-> 'brown'
    ((1 3)) |}];
  test btree
    Tsquery.(Clause (NEIGHBOR 1, [ Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]); Token "fox" ]));
  [%expect {|
    ('fox':* <-> 'brown') <-> 'fox'
    ((2 3)) |}];
  test btree
    Tsquery.(Clause (NEIGHBOR 1, [ Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]); Token "jumps" ]));
  [%expect {|
      ('fox':* <-> 'brown') <-> 'jumps'
      () |}];
  test btree
    Tsquery.(
      Clause
        ( NEIGHBOR 1,
          [
            Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]);
            Clause (OR, [ Token "jumps"; Clause (NEIGHBOR 1, [ Prefix "f"; Token "jumps" ]) ]);
          ] ));
  [%expect {|
    ('fox':* <-> 'brown') <-> ('jumps' | ('f':* <-> 'jumps'))
    ((2 4)) |}];
  test btree Tsquery.(Clause (NEIGHBOR 2, [ Prefix "fox"; Prefix "fox" ]));
  [%expect {|
    'fox':* <2> 'fox':*
    ((2 2)) |}];
  test btree Tsquery.(Clause (NEIGHBOR 3, [ Prefix "fox"; Prefix "fox" ]));
  [%expect {|
    'fox':* <3> 'fox':*
    () |}];
  test btree Tsquery.(Clause (NEIGHBOR 2, [ Prefix "fox"; Clause (OR, [ Prefix "fox"; Prefix "jump" ]) ]));
  [%expect {|
    'fox':* <2> ('fox':* | 'jump':*)
    ((2 2)) |}]

let%expect_test "Matching" =
  let test vector query = Evaluate.matches vector query |> Bool.to_string |> print_endline in
  test (Tsvector.tag "Hello world") Tsquery.(Token "world");
  [%expect {| true |}];
  test (Tsvector.tag "Hello world") Tsquery.(Token "hello");
  [%expect {| true |}];
  test (Tsvector.tag "Hello world") Tsquery.(Token "wo");
  [%expect {| false |}];
  test (Tsvector.tag "Hello world") Tsquery.(Prefix "wo");
  [%expect {| true |}];
  test (Tsvector.tag "Hello world") Tsquery.(Clause (AND, [ Prefix "wo"; Token "blah" ]));
  [%expect {| false |}];
  test (Tsvector.tag "Hello world") Tsquery.(Clause (OR, [ Prefix "wo"; Token "blah" ]));
  [%expect {| true |}];
  test (Tsvector.tag "Hello world") Tsquery.(Clause (AND, [ Prefix "wo"; Prefix "h" ]));
  [%expect {| true |}];
  test (Tsvector.tag "Hello world") Tsquery.(Clause (NEIGHBOR 1, [ Prefix "wo"; Prefix "h" ]));
  [%expect {| false |}];
  test (Tsvector.tag "Hello world") Tsquery.(Clause (NEIGHBOR 1, [ Prefix "h"; Prefix "wo" ]));
  [%expect {| true |}];
  let vector = Tsvector.tag "the quick foxy brown fox jumps over the lazy fox" in
  test vector Tsquery.(Clause (NEIGHBOR 1, [ Prefix "fox"; Prefix "br" ]));
  [%expect {| true |}];
  test vector Tsquery.(Clause (AND, [ Clause (NEIGHBOR 1, [ Prefix "fox"; Prefix "br" ]); Token "jump" ]));
  [%expect {| false |}]
