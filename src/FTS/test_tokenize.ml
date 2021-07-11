open! Core_kernel

let%expect_test "Tokenize" =
  let test x = Sequence.to_array x |> sprintf !"%{sexp: string array}" |> print_endline in
  test (Tokenizers.simple "hello world");
  [%expect {| (hello world) |}];
  test (Tokenizers.simple "hello  world");
  [%expect {| (hello world) |}];
  test (Tokenizers.simple "");
  [%expect {| () |}];
  test (Tokenizers.simple " ");
  [%expect {| () |}];
  test (Tokenizers.simple "  ");
  [%expect {| () |}];
  test (Tokenizers.simple " x ");
  [%expect {| (x) |}];
  test (Tokenizers.simple "ab");
  [%expect {| (ab) |}];
  test (Tokenizers.simple " a  b  ");
  [%expect {| (a b) |}];
  test (Tokenizers.simple "a aa\n b   b   cdefefefae");
  [%expect {| (a aa b b cdefefefae) |}];
  test (Tokenizers.simple "          ");
  [%expect {| () |}];

  test (Tokenizers.trigram "hello world");
  [%expect {| (hel ell llo "lo " "o w" " wo" wor orl rld) |}];
  test (Tokenizers.trigram "a");
  [%expect {| (a) |}];
  test (Tokenizers.trigram "ab");
  [%expect {| (ab) |}];
  test (Tokenizers.trigram "abc");
  [%expect {| (abc) |}];
  test (Tokenizers.trigram "abcd");
  [%expect {| (abc bcd) |}];
  test (Tokenizers.trigram " a");
  [%expect {| (" a") |}]
