open! Core_kernel

let%expect_test "Normalize" =
  let test s = print_endline s in
  test (Normalizers.name "");
  [%expect {|  |}];
  test (Normalizers.name "hello");
  [%expect {| hello |}];
  test (Normalizers.name "hELlo");
  [%expect {| hello |}];
  test (Normalizers.name "H E L L   O");
  [%expect {| hello |}];
  test (Normalizers.name "\the!l(()(627@81l-4!\no ");
  [%expect {| hell-o |}];

  test (Normalizers.tag "\thè!l(()(62781l-4!\no ");
  [%expect {| hel62781l-4o |}];
  test (Normalizers.tag "00h@0ll 00x00 ");
  [%expect {| h@0ll00x00 |}];
  test (Normalizers.tag " 00he0ll  0");
  [%expect {| 00he0ll0 |}];

  test (Normalizers.english " h8â*7&_@.L0L-Ö ");
  [%expect {| h8a7@l0l-o |}]
