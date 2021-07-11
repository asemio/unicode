open! Core_kernel

type variant =
  | A
  | B
  | C of string
[@@deriving sexp]

type test = {
  aa: string;
  bb: int;
  cc: bool;
  dd: variant;
  ee: variant list;
  ff: int list;
}
[@@deriving fields]

let%expect_test "TSVector" =
  let open Tsvector in
  let test x = print_endline x in
  let test_string = "0123.abc: Hello world!" in
  name test_string |> to_string |> test;
  [%expect {|
    'APK':1 '_APK':2 'HL':3 '_HL':4 'ARLT':5 '_FRLT':6 |}];
  tag test_string |> to_string |> test;
  [%expect {| '123.abc':1 'hello':2 'world':3 |}];
  english_trigrams test_string |> to_string |> test;
  [%expect
    {| '012':1 '123':2 '23a':3 '3ab':4 'abc':5 'hel':6 'ell':7 'llo':8 'wor':9 'orl':10 'rld':11 |}];
  tag_trigrams test_string |> to_string |> test;
  [%expect
    {| '123':1 '23.':2 '3.a':3 '.ab':4 'abc':5 'hel':6 'ell':7 'llo':8 'wor':9 'orl':10 'rld':11 |}];
  let open Record in
  { aa = "hello"; bb = 123; cc = true; dd = C "world"; ee = [ B; C "foo" ]; ff = [ 1; 2; 3 ] }
  |> Fields_of_test.Direct.fold ~aa:(use tag_trigrams) ~bb:skip ~cc:(bool "cc")
       ~dd:(single (symbol [%sexp_of: variant]))
       ~ee:(multiple Sequence.of_list (symbol [%sexp_of: variant]))
       ~ff:(multiple Sequence.of_list (symbol [%sexp_of: int]))
  |> create
  |> to_string
  |> test;
  [%expect {| 'ff=3' 'ff=2' 'ff=1' 'ee=(C foo)' 'ee=B' '(C world)' 'cc' 'llo':1 'ell':2 'hel':3 |}]
