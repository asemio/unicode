open! Core_kernel

let%expect_test "Get positions" =
  let vector = Tsvector.tag "the quick foxy brown fox jumps over the lazy fox" in
  let test vector query =
    print_endline (Tsquery.to_string query);
    Evaluate.get_positions vector query |> sprintf !"%{sexp: int Int.Map.t}" |> print_endline
  in
  test vector Tsquery.(Token "brown");
  [%expect {|
    'brown'
    ((4 1)) |}];
  test vector Tsquery.(Token "the");
  [%expect {|
    'the'
    ((1 1) (8 1)) |}];
  test vector Tsquery.(Token "br");
  [%expect {|
    'br'
    () |}];
  test vector Tsquery.(Prefix "brown");
  [%expect {|
    'brown':*
    ((4 1)) |}];
  test vector Tsquery.(Prefix "br");
  [%expect {|
    'br':*
    ((4 1)) |}];
  test vector Tsquery.(Prefix "fox");
  [%expect {|
    'fox':*
    ((3 1) (5 1) (10 1)) |}];
  test vector Tsquery.(Clause (OR, [ Prefix "fox"; Token "the" ]));
  [%expect {|
    'fox':* | 'the'
    ((1 1) (3 1) (5 1) (8 1) (10 1)) |}];
  test vector Tsquery.(Clause (AND, [ Prefix "fox"; Token "fox" ]));
  [%expect {|
    'fox':* & 'fox'
    ((5 1) (10 1)) |}];
  test vector Tsquery.(Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]));
  [%expect {|
    'fox':* <-> 'brown'
    ((3 2)) |}];
  test vector Tsquery.(Clause (NEIGHBOR 1, [ Token "quick"; Prefix "fox"; Token "brown" ]));
  [%expect {|
    'quick' <-> 'fox':* <-> 'brown'
    ((2 3)) |}];
  test vector
    Tsquery.(Clause (NEIGHBOR 1, [ Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]); Token "fox" ]));
  [%expect {|
    ('fox':* <-> 'brown') <-> 'fox'
    ((3 3)) |}];
  test vector
    Tsquery.(Clause (NEIGHBOR 1, [ Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]); Token "jumps" ]));
  [%expect {|
      ('fox':* <-> 'brown') <-> 'jumps'
      () |}];
  test vector
    Tsquery.(
      Clause
        ( NEIGHBOR 1,
          [
            Clause (NEIGHBOR 1, [ Prefix "fox"; Token "brown" ]);
            Clause (OR, [ Token "jumps"; Clause (NEIGHBOR 1, [ Prefix "f"; Token "jumps" ]) ]);
          ]
        )
    );
  [%expect {|
    ('fox':* <-> 'brown') <-> ('jumps' | ('f':* <-> 'jumps'))
    ((3 4)) |}];
  test vector Tsquery.(Clause (NEIGHBOR 2, [ Prefix "fox"; Prefix "fox" ]));
  [%expect {|
    'fox':* <2> 'fox':*
    ((3 2)) |}];
  test vector Tsquery.(Clause (NEIGHBOR 3, [ Prefix "fox"; Prefix "fox" ]));
  [%expect {|
    'fox':* <3> 'fox':*
    () |}];
  test vector
    Tsquery.(Clause (NEIGHBOR 2, [ Prefix "fox"; Clause (OR, [ Prefix "fox"; Prefix "jump" ]) ]));
  [%expect {|
    'fox':* <2> ('fox':* | 'jump':*)
    ((3 2)) |}]

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
  [%expect {| false |}];
  (* NOT *)
  test (Tsvector.tag "Hello world") Tsquery.(NOT (Token "world"));
  [%expect {| false |}];
  test (Tsvector.tag "Hello world") Tsquery.(NOT (Clause (NEIGHBOR 1, [ Prefix "h"; Prefix "wo" ])));
  [%expect {| false |}];
  test (Tsvector.tag "Hello world") Tsquery.(Clause (NEIGHBOR 1, [ Prefix "h"; NOT (Prefix "wo") ]));
  [%expect {| false |}]

let%expect_test "Full circle" =
  let test svector squery =
    let vector = Parsing.tsvector svector |> Result.ok_or_failwith in
    let query = Parsing.tsquery squery |> Result.ok_or_failwith in
    (* print_endline (sprintf !"%{sexp: Tsvector.t}\n%{sexp: Tsquery.t}\n" vector query); *)
    print_endline (sprintf !"%{Tsvector}\n%{Tsquery}\n%b" vector query (Evaluate.matches vector query))
  in
  test "'a':1 'b':2 'c':3 'd':4 'e':5" "a <-> b";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':5
    'a' <-> 'b'
    true |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':5" "a <-> !b";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':5
    'a' <-> !'b'
    false |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':5" "a <-> !c";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':5
    'a' <-> !'c'
    true |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':5" "b <-> a";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':5
    'b' <-> 'a'
    false |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':5" "b <-> !a";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':5
    'b' <-> !'a'
    true |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':3" "a <-> !c";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':3
    'a' <-> !'c'
    true |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':5" "b <-> c";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':5
    'b' <-> 'c'
    true |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':5" "b <-> !c";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':5
    'b' <-> !'c'
    false |}];
  test "'a':1 'b':2 'c':3 'd':4 'e':3" "b <-> !c";
  [%expect {|
    'a':1 'b':2 'c':3 'd':4 'e':3
    'b' <-> !'c'
    false |}];
  test "'a':1 'b':2 'e':3 'd':4 'c':3" "b <-> !c";
  [%expect {|
    'a':1 'b':2 'e':3 'd':4 'c':3
    'b' <-> !'c'
    false |}];
  test "'a':1 'b':2 'e':3 'd':4 'c':3" "c <-> d";
  [%expect {|
    'a':1 'b':2 'e':3 'd':4 'c':3
    'c' <-> 'd'
    true |}];
  test "'a':1 'b':2 'e':3 'd':4 'c':3" "e <-> d";
  [%expect {|
    'a':1 'b':2 'e':3 'd':4 'c':3
    'e' <-> 'd'
    true |}];
  test "'a':1 'b':2 'e':3 'd':4 'c':3" "!c <-> d";
  [%expect {|
    'a':1 'b':2 'e':3 'd':4 'c':3
    !'c' <-> 'd'
    false |}];
  test "'a':1 'b':2 'e':3 'd':4 'c':3" "(a <-> b) <-> c";
  [%expect {|
    'a':1 'b':2 'e':3 'd':4 'c':3
    ('a' <-> 'b') <-> 'c'
    true |}];
  test "'a':1 'b':2 'e':3 'd':4 'c':3" "!(a <-> b) <-> d";
  [%expect {|
    'a':1 'b':2 'e':3 'd':4 'c':3
    !('a' <-> 'b') <-> 'd'
    true |}];
  test "'a':1 'b':2 'e':3 'd':4 'c':3" "!(a <-> b) <-> c";
  [%expect {|
    'a':1 'b':2 'e':3 'd':4 'c':3
    !('a' <-> 'b') <-> 'c'
    false|}];
  test "'a':1 'b':2 'e':2 'd':4 'c':3" "!(a <-> b) <-> c";
  [%expect {|
    'a':1 'b':2 'e':2 'd':4 'c':3
    !('a' <-> 'b') <-> 'c'
    false|}]
(* test "'a':1 'b':2 'e':2 'd':4 'c':3" "a <-> b <-> (d | c)";
   [%expect {| true |}];
   test "'a':1 'b':2 'e':2 'd':4 'c':3" "a <-> b <-> (d | e)";
   [%expect {| false |}];
   test "'a':1 'b':2 'e':2 'd':4 'c':3" "a <-> b <-> (d | !e)";
   [%expect {| true |}];
   test "'a':1 'b':2 'e':2 'd':4 'c':3" "a <-> b <-> (e | !d)";
   [%expect {| true |}];
   test "'a':1 'b':2 'e':2 'd':4 'c':3" "a <-> b <-> (e | !c)";
   [%expect {| false |}];
   test "'a':1 'b':2 'e':2 'd':4 'c':3" "a <-> b <-> (!c | c)";
   [%expect {| true |}];
   test "'a':1 'b':2 'e':2 'd':4 'c':3" "a <-> b <-> (!z)";
   [%expect {| true |}] *)
