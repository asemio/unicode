open! Core_kernel

let%expect_test "Double Metaphone" =
  let test s =
    let custom = Unicode.create s |> Unicode.dmetaphone in
    let original =
      Unicode.(create s |> standardize ~ignore:Unicode__.Dmetaphone.ignore |> cmp_bytes)
      |> Dmetaphone_cpp.dmetaphone_both
    in
    if [%equal: string * string] custom original
    then sprintf !"VALID %{sexp: string * string}" custom |> print_endline
    else
      sprintf !"!!! %{sexp: string * string} -- %{sexp: string * string}" custom original |> print_endline
  in

  test "Smith";
  [%expect {| VALID (SM0 XMT) |}];
  print_endline (sprintf !"%{sexp: string * string}" Unicode.(create "" |> Unicode.dmetaphone));
  [%expect {| ("" "") |}];
  (*
          Soundex test suite from Rosetta Code
        *)
  test "Soundex";
  [%expect {| VALID (SNTK SNTK) |}];
  test "Example";
  [%expect {| VALID (AKSM AKSM) |}];
  test "Sownteks";
  [%expect {| VALID (SNTK SNTK) |}];
  test "Ekzampul";
  [%expect {| VALID (AKSM AKTS) |}];
  test "Euler";
  [%expect {| VALID (ALR ALR) |}];
  test "Gauss";
  [%expect {| VALID (KS KS) |}];
  test "Hilbert";
  [%expect {| VALID (HLPR HLPR) |}];
  test "Knuth";
  [%expect {| VALID (N0 NT) |}];
  test "Lloyd";
  [%expect {| VALID (LT LT) |}];
  test "Lukasiewicz";
  [%expect {| VALID (LKST LKSF) |}];
  test "Ellery";
  [%expect {| VALID (ALR ALR) |}];
  test "Ghosh";
  [%expect {| VALID (KX KX) |}];
  test "Heilbronn";
  [%expect {| VALID (HLPR HLPR) |}];
  test "Kant";
  [%expect {| VALID (KNT KNT) |}];
  test "Ladd";
  [%expect {| VALID (LT LT) |}];
  test "Lissajous";
  [%expect {| VALID (LSJS LSHS) |}];
  test "Wheaton";
  [%expect {| VALID (ATN ATN) |}];
  test "Ashcraft";
  [%expect {| VALID (AXKR AXKR) |}];
  test "Burroughs";
  [%expect {| VALID (PRFS PRFS) |}];
  test "Burrows";
  [%expect {| VALID (PRS PRS) |}];
  test "O'Hara";
  [%expect {| VALID (AR AR) |}];
  (*
          Test cases added from initial algorithm implementation mismatches
        *)
  test "each";
  [%expect {| VALID (AK AK) |}];
  test "GLIA";
  [%expect {| VALID (KL L) |}];
  test "ALGIERS";
  [%expect {| VALID (ALJR ALKR) |}];
  test "ÂGE";
  [%expect {| VALID (AJ AK) |}];
  test "JOSÉ";
  [%expect {| VALID (HS HS) |}];
  test "zhe";
  [%expect {| VALID (J J) |}];
  test "garçon";
  (* The original algorithm doesn't handle unicode multibyte glyphs such as Ç and Ñ correctly *)
  [%expect {| !!! (KRSN KRSN) -- (KRN KRN) |}]

let english = [%blob "english.txt"]

let french = [%blob "french.txt"]

let pinyin = [%blob "pinyin.txt"]

let hungarian = [%blob "hungarian.txt"]

let polish = [%blob "polish.txt"]

let%expect_test "Dictionaries" =
  let test dict =
    String.split_lines dict
    |> List.fold ~init:(0, 0, "", []) ~f:(fun (good, bad, shortest, all) word ->
         let unicode = Unicode.create word in
         match
           ( Unicode.(standardize ~ignore:Unicode__.Dmetaphone.ignore unicode |> cmp_bytes)
             |> Dmetaphone_cpp.dmetaphone_both,
             Unicode.dmetaphone unicode )
         with
         | original, custom when [%equal: string * string] original custom -> good + 1, bad, shortest, all
         | _ ->
           let shortest =
             if String.is_empty shortest || String.length word < String.length shortest
             then word
             else shortest
           in
           let all = if bad < 100 then word :: all else [] in
           good, bad + 1, shortest, all
       )
    |> function
    | good, 0, _, _ -> print_endline (sprintf "100%% matching, %d words." good)
    | good, bad, shortest, [] ->
      print_endline (sprintf "Matching: %d. Incorrect: %d. Shortest incorrect: '%s'" good bad shortest)
    | good, bad, shortest, words ->
      print_endline
        (sprintf "Matching: %d. Incorrect: %d. Shortest incorrect: '%s'. All incorrect: %s" good bad
           shortest (String.concat ~sep:", " words)
        )
  in
  test english;
  [%expect {| 100% matching, 58109 words. |}];
  test french;
  (* Issue with the C++ version's handling of multibyte Unicode. The OCaml version is correct. *)
  [%expect
    {| Matching: 22680. Incorrect: 60. Shortest incorrect: 'REÇU'. All incorrect: TIERÇAGE, SOUPÇONS, SOUPÇONNEUX, SOUPÇONNÉS, SOUPÇONNER, SOUPÇONNÉ, SOUPÇONNE, SOUPÇON, REÇUT, REÇUS, REÇUES, REÇUE, REÇU, REÇOIVENT, REÇOIT, RANÇON, POINÇON, PERÇUS, PERÇUE, PERÇU, PERÇOIVENT, PERÇOIT, MALFAÇON, MAÇONS, MAÇONNIQUE, MAÇONNERIE, MAÇON, LIMAÇON, LEÇONS, LEÇON, GLAÇON, GARÇONS, GARÇON, FRANÇOIS, FRANÇAIS, FRANÇAIS, FIANÇAILLES, FAÇONS, FAÇONNER, FAÇON, ÉTANÇONNER, DÉNONÇAIT, DÉNONÇAIENT, DÉÇUS, DÉÇUE, DÉÇU, DEÇÀ, CONÇUS, CONÇUES, CONÇUE, CONÇU, CONÇOIT, COMMERÇANTS, COMMERÇANT, COMMENÇONS, COLIMAÇON, CALEÇON, BESANÇON, BALANÇOIRE, APERÇU |}];
  test pinyin;
  [%expect {| 100% matching, 8841 words. |}];
  test hungarian;
  [%expect {| 100% matching, 8598 words. |}];
  test polish;
  [%expect {| 100% matching, 5000 words. |}]
