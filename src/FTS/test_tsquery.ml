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

let%expect_test "TSQuery" =
  let open Tsquery in
  let test = print_endline in
  let test_string = "Title: hello world, WELCOME!" in
  tag ~accept_prefix:true ~across_words:OR test_string |> to_string |> test;
  [%expect {| 'title':* | 'hello':* | 'world':* | 'welcome':* |}];
  tag ~accept_prefix:false ~across_words:AND test_string |> to_string |> test;
  [%expect {| 'title' & 'hello' & 'world' & 'welcome' |}];
  name ~accept_prefix:true ~across_words:OR test_string |> to_string |> test;
  [%expect {| 'TTL':* | '_TTL':* | 'HL':* | '_HL':* | 'ARLT':* | '_FRLT':* | 'ALKM':* | '_FLKM':* |}];
  name ~accept_prefix:false ~across_words:AND test_string |> to_string |> test;
  [%expect {| 'TTL' & '_TTL' & 'HL' & '_HL' & 'ARLT' & '_FRLT' & 'ALKM' & '_FLKM' |}];
  english_trigrams ~across_trigrams:(NEIGHBOR 1) ~across_words:AND test_string |> to_string |> test;
  [%expect
    {| ('tit' <-> 'itl' <-> 'tle') & ('hel' <-> 'ell' <-> 'llo') & ('wor' <-> 'orl' <-> 'rld') & ('wel' <-> 'elc' <-> 'lco' <-> 'com' <-> 'ome') |}];
  english_trigrams ~across_trigrams:(NEIGHBOR 1) ~across_words:OR test_string |> to_string |> test;
  [%expect
    {| ('tit' <-> 'itl' <-> 'tle') | ('hel' <-> 'ell' <-> 'llo') | ('wor' <-> 'orl' <-> 'rld') | ('wel' <-> 'elc' <-> 'lco' <-> 'com' <-> 'ome') |}];
  english_trigrams ~across_trigrams:OR ~across_words:AND test_string |> to_string |> test;
  [%expect
    {| ('tit' | 'itl' | 'tle') & ('hel' | 'ell' | 'llo') & ('wor' | 'orl' | 'rld') & ('wel' | 'elc' | 'lco' | 'com' | 'ome') |}];
  tag_trigrams ~across_trigrams:(NEIGHBOR 1) ~across_words:AND test_string |> to_string |> test;
  [%expect
    {| ('tit' <-> 'itl' <-> 'tle') & ('hel' <-> 'ell' <-> 'llo') & ('wor' <-> 'orl' <-> 'rld') & ('wel' <-> 'elc' <-> 'lco' <-> 'com' <-> 'ome') |}];
  tag_trigrams ~across_trigrams:(NEIGHBOR 1) ~across_words:OR test_string |> to_string |> test;
  [%expect
    {| ('tit' <-> 'itl' <-> 'tle') | ('hel' <-> 'ell' <-> 'llo') | ('wor' <-> 'orl' <-> 'rld') | ('wel' <-> 'elc' <-> 'lco' <-> 'com' <-> 'ome') |}];
  tag_trigrams ~across_trigrams:OR ~across_words:AND test_string |> to_string |> test;
  [%expect
    {| ('tit' | 'itl' | 'tle') & ('hel' | 'ell' | 'llo') & ('wor' | 'orl' | 'rld') & ('wel' | 'elc' | 'lco' | 'com' | 'ome') |}];
  let open Record in
  { aa = "hello"; bb = 123; cc = true; dd = C "world"; ee = [ B; C "foo" ]; ff = [ 1; 2; 3 ] }
  |> Fields_of_test.Direct.fold ~aa:string ~bb:skip ~cc:(bool "cc")
       ~dd:(single (symbol [%sexp_of: variant]))
       ~ee:(multiple Sequence.of_list (symbol [%sexp_of: variant]))
       ~ff:(multiple Sequence.of_list (symbol [%sexp_of: int]))
  |> create
  |> to_string
  |> test;
  [%expect {| ('ff=1' | 'ff=2' | 'ff=3') & ('ee=B' | 'ee=(C foo)') & '(C world)' & 'cc' & 'hello' |}];
  NOT (Clause (OR, [ Token "abc"; Prefix "de" ])) |> to_string |> test;
  [%expect {| !('abc' | 'de':*) |}];
  NOT (Token "abc") |> to_string |> test;
  [%expect {| !'abc' |}]
