open! Core
open Unicode

let str a b = print_endline (sprintf "%s\n%s\n%b" a b String.(a = b))

let int a b = print_endline (sprintf "%d\n%d\n%b" a b Int.(a = b))

let from_bytes ll =
  let buf = Buffer.create 30 in
  List.iter ll ~f:(fun x -> Buffer.add_char buf (Char.of_int_exn x));
  Buffer.contents buf

let from_codepoints ll =
  let buf = Buffer.create 30 in
  List.iter ll ~f:(fun x -> Uutf.Buffer.add_utf_8 buf (Uchar.of_scalar_exn x));
  Buffer.contents buf

let%expect_test "Normalization" =
  let a1 = [ 65; 109; 195; 169; 108; 105; 101 ] in
  let a2 = [ 65; 109; 101; 204; 129; 108; 105; 101 ] in
  str (from_bytes a1) (create (from_bytes a1) |> cmp_bytes);
  [%expect {|
    Amélie
    Amélie
    true |}];

  str (from_bytes a1) (create (from_bytes a2) |> cmp_bytes);
  [%expect {|
    Amélie
    Amélie
    true |}];

  let cp1252 = [ 65; 109; 233; 108; 105; 101 ] in
  str (from_bytes a1) (create ~encoding_name:"CP-1252" (from_bytes cp1252) |> cmp_bytes);
  [%expect {|
    Amélie
    Amélie
    true |}];

  let b1 = [ 226; 140; 152; 32; 195; 169 ] in
  let b2 = [ 226; 140; 152; 32; 101; 204; 129 ] in
  str (from_bytes b1) (create (from_bytes b1) |> cmp_bytes);
  [%expect {|
    ⌘ é
    ⌘ é
    true |}];

  str (from_bytes b1) (create (from_bytes b2) |> cmp_bytes);
  [%expect {|
    ⌘ é
    ⌘ é
    true |}]

let%expect_test "Trim" =
  let utf8 =
    from_bytes
      [ 32; 9; 32; 65; 109; 195; 169; 108; 105; 101; 32; 240; 159; 135; 171; 240; 159; 135; 183; 10 ]
  in
  let u = create utf8 in

  int 20 (String.length utf8);
  [%expect {|
    20
    20
    true |}];
  int 12 (length u);
  [%expect {|
    12
    12
    true |}];

  let trimmed =
    from_bytes [ 65; 109; 195; 169; 108; 105; 101; 32; 240; 159; 135; 171; 240; 159; 135; 183 ]
  in
  (* UTF-8 trim *)
  str trimmed (trim u |> cmp_bytes);
  [%expect {|
    Amélie 🇫🇷
    Amélie 🇫🇷
    true |}];

  let sliced = from_bytes [ 109; 195; 169; 108; 105; 101; 32 ] in
  (* Trim UTF-8 works on grapheme clusters *)
  str sliced (slice (trim u) 1 (-1) |> cmp_bytes);
  [%expect {|
    mélie
    mélie
    true |}];

  let a = 0x410 in
  let b = 0x411 in
  let c = 0x412 in
  let d = 0x413 in
  let e = 0x414 in
  let f = 0x415 in
  let abcdef = create (from_codepoints [ a; b; c; d; e; f ]) in

  (* Slice before before *)
  str (from_codepoints [ d; e ]) (slice abcdef (-3) (-1) |> cmp_bytes);
  [%expect {|
    ГД
    ГД
    true |}];

  (* Slice before in *)
  str "" (slice abcdef (-3) 2 |> cmp_bytes);
  [%expect {| true |}];

  (* Slice before after *)
  str (from_codepoints [ d; e; f ]) (slice abcdef (-3) 12 |> cmp_bytes);
  [%expect {|
    ГДЕ
    ГДЕ
    true |}];

  (* Slice in in *)
  str (from_codepoints [ a; b ]) (slice abcdef 0 2 |> cmp_bytes);
  [%expect {|
    АБ
    АБ
    true |}];

  (* Slice 0 0 *)
  str (from_codepoints [ a; b; c; d; e; f ]) (slice abcdef 0 0 |> cmp_bytes);
  [%expect {|
    АБВГДЕ
    АБВГДЕ
    true |}];

  (* Slice in 0 *)
  str (from_codepoints [ c; d; e; f ]) (slice abcdef 2 0 |> cmp_bytes);
  [%expect {|
    ВГДЕ
    ВГДЕ
    true |}];

  (* Slice in after *)
  str (from_codepoints [ c; d; e; f ]) (slice abcdef 2 10 |> cmp_bytes);
  [%expect {|
    ВГДЕ
    ВГДЕ
    true |}];

  (* Slice in before *)
  str (from_codepoints [ c; d ]) (slice abcdef 2 (-2) |> cmp_bytes);
  [%expect {|
    ВГ
    ВГ
    true |}];

  (* Slice after after *)
  str "" (slice abcdef 10 12 |> cmp_bytes);
  [%expect {| true |}];

  (* Slice after after (rev) *)
  str "" (slice abcdef 15 12 |> cmp_bytes);
  [%expect {| true |}];

  (* Slice after in *)
  str "" (slice abcdef 10 0 |> cmp_bytes);
  [%expect {| true |}];

  (* Slice after before *)
  str "" (slice abcdef 10 (-2) |> cmp_bytes);
  [%expect {| true |}]

let%expect_test "Case Mapping" =
  let aa = 0x410 in
  let bb = 0x411 in
  let cc = 0x412 in
  let a = 0x430 in
  let b = 0x431 in
  let c = 0x432 in
  let space = 0x20 in

  let all = create (from_codepoints [ a; b; c; space; aa; bb; cc ]) in

  (* to_lower *)
  str (from_codepoints [ a; b; c; space; a; b; c ]) (to_lower all |> cmp_bytes);
  [%expect {|
    абв абв
    абв абв
    true |}];

  (* to_upper *)
  str (from_codepoints [ aa; bb; cc; space; aa; bb; cc ]) (to_upper all |> cmp_bytes);
  [%expect {|
    АБВ АБВ
    АБВ АБВ
    true |}];

  (* to_title (cyrillic) *)
  str (from_codepoints [ aa; b; c; space; aa; b; c ]) (to_title all |> cmp_bytes);
  [%expect {|
    Абв Абв
    Абв Абв
    true |}];

  (* to_title (latin) *)
  str "Abc Abc" (to_title (create (from_codepoints [ 97; 98; 99; 32; 65; 66; 67 ])) |> cmp_bytes);
  [%expect {|
    Abc Abc
    Abc Abc
    true |}]

let%expect_test "Unaccent" =
  str "Editions Maika Cote ((r))" (create "Éditions Maïka Côté ｟r｠" |> unaccent |> cmp_bytes);
  [%expect {|
    Editions Maika Cote ((r))
    Editions Maika Cote ((r))
    true |}];

  str ">+-~" (create {s|˃˖˗˜|s} |> unaccent |> cmp_bytes);
  [%expect {|
    >+-~
    >+-~
    true |}];

  str "letesitmeny" (create {s|létesítmény|s} |> unaccent |> cmp_bytes);
  [%expect {|
    letesitmeny
    letesitmeny
    true
 |}];

  str "emlekezteto" (create {s|emlékeztető|s} |> unaccent |> cmp_bytes);
  [%expect {|
    emlekezteto
    emlekezteto
    true
 |}];

  str "dongjianshi" (create {s|dǒngjiānshì|s} |> unaccent |> cmp_bytes);
  [%expect {|
   dongjianshi
   dongjianshi
   true
|}];

  str "glosniej" (create {s|głośniej|s} |> unaccent |> cmp_bytes);
  [%expect {|
    glosniej
    glosniej
    true
|}];

  str "zajal" (create {s|zajął|s} |> unaccent |> cmp_bytes);
  [%expect {|
    zajal
    zajal
    true
|}];

  str "tozsamosc" (create {s|tożsamość|s} |> unaccent |> cmp_bytes);
  [%expect {|
    tozsamosc
    tozsamosc
    true
|}]

let%expect_test "Normalize" =
  str "figueroa garcia otel jr" (create "  Figuéroa-Garcia ｟Ô℡｠ Jr. " |> standardize |> cmp_bytes);
  [%expect {|
    figueroa garcia otel jr
    figueroa garcia otel jr
    true |}]

let%expect_test "Squish" =
  (* squish start *)
  str "abc" (create " abc" |> squish |> cmp_bytes);
  [%expect {|
    abc
    abc
    true |}];

  (* squish end *)
  str "abc" (create "abc " |> squish |> cmp_bytes);
  [%expect {|
    abc
    abc
    true |}];

  (* squish both *)
  str "abc" (create "\t abc   " |> squish |> cmp_bytes);
  [%expect {|
    abc
    abc
    true |}];

  (* squish middle *)
  str "abc def" (create "\t abc \r\tdef   " |> squish |> cmp_bytes);
  [%expect {|
    abc def
    abc def
    true |}]
