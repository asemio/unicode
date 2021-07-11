open! Core_kernel

let%expect_test "Parse" =
  let test x = Sequence.to_array x |> sprintf !"%{sexp: string array}" |> print_endline in
  test (Parsers.name "smith schmidt");
  [%expect {| (SM0 _XMT XMT _SMT) |}];
  test (Parsers.name "amélie amely amelit");
  [%expect {| (AML _AML AML _AML AMLT _AMLT) |}];
  test (Parsers.name "Paraphrasing rhododendron");
  [%expect {| (PRFR _PRFR RTTN _RTTN) |}]
