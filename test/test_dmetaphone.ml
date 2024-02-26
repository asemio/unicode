open! Core

let cpp_dmetaphone box =
  try
    Unicode.(standardize box ~preserve:Unicode__.Dmetaphone.preserve ~case:Char.uppercase |> cmp_bytes)
    |> String.substr_replace_all ~pattern:"Ç" ~with_:"\xc7"
    |> String.substr_replace_all ~pattern:"ç" ~with_:"\xc7"
    |> String.substr_replace_all ~pattern:"Ñ" ~with_:"\xd1"
    |> String.substr_replace_all ~pattern:"ñ" ~with_:"\xd1"
    |> Dmetaphone_cpp.dmetaphone_both
  with
  | _ -> "", ""

let%expect_test "Double Metaphone" =
  let test s =
    let box = Unicode.create s in
    let custom = Unicode.dmetaphone box in
    let original = cpp_dmetaphone box in
    if [%equal: string * string] custom original
    then sprintf !"VALID %{sexp: string * string}" custom |> print_endline
    else
      sprintf !"!!! %{sexp: string * string} -- %{sexp: string * string}" custom original |> print_endline
  in

  test "Smith";
  [%expect {| VALID (SM0 XMT) |}];
  print_endline (sprintf !"%{sexp: string * string}" Unicode.(create "" |> Unicode.dmetaphone));
  [%expect {| ("" "") |}];
  (* All the special case examples mentioned in the original code *)
  test "xavier";
  [%expect {| VALID (SF SFR) |}];
  test "caesar";
  [%expect {| VALID (SSR SSR) |}];
  test "chianti";
  [%expect {| VALID (KNT KNT) |}];
  test "michael";
  [%expect {| VALID (MKL MXL) |}];
  test "chemistry";
  [%expect {| VALID (KMST KMST) |}];
  test "chorus";
  [%expect {| VALID (KRS KRS) |}];
  test "architect";
  [%expect {| VALID (ARKT ARKT) |}];
  test "arch";
  [%expect {| VALID (ARX ARK) |}];
  test "orchestra";
  [%expect {| VALID (ARKS ARKS) |}];
  test "orchid";
  [%expect {| VALID (ARKT ARKT) |}];
  test "wachtler";
  [%expect {| VALID (AKTL FKTL) |}];
  test "wechsler";
  [%expect {| VALID (AKSL FKSL) |}];
  test "tichner";
  [%expect {| VALID (TXNR TKNR) |}];
  test "McHugh";
  [%expect {| VALID (MK MK) |}];
  test "czerny";
  [%expect {| VALID (SRN XRN) |}];
  test "focaccia";
  [%expect {| VALID (FKX FKX) |}];
  test "McClellan";
  [%expect {| VALID (MKLL MKLL) |}];
  test "bellocchio";
  [%expect {| VALID (PLX PLX) |}];
  test "bacchus";
  [%expect {| VALID (PKS PKS) |}];
  test "accident";
  [%expect {| VALID (AKST AKST) |}];
  test "accede";
  [%expect {| VALID (AKST AKST) |}];
  test "succeed";
  [%expect {| VALID (SKST SKST) |}];
  test "bacci";
  [%expect {| VALID (PX PX) |}];
  test "bertucci";
  [%expect {| VALID (PRTX PRTX) |}];
  test "mac caffrey";
  [%expect {| VALID (MKFR MKFR) |}];
  test "mac gregor";
  [%expect {| VALID (MKRK MKRK) |}];
  test "edge";
  [%expect {| VALID (AJ AJ) |}];
  test "edgar";
  [%expect {| VALID (ATKR ATKR) |}];
  test "ghislane";
  [%expect {| VALID (JLN JLN) |}];
  test "ghiradelli";
  [%expect {| VALID (JRTL JRTL) |}];
  test "hugh";
  [%expect {| VALID (H H) |}];
  test "laugh";
  [%expect {| VALID (LF LF) |}];
  test "McLaughlin";
  [%expect {| VALID (MKLF MKLF) |}];
  test "cough";
  [%expect {| VALID (KF KF) |}];
  test "gough";
  [%expect {| VALID (KF KF) |}];
  test "rough";
  [%expect {| VALID (RF RF) |}];
  test "tough";
  [%expect {| VALID (TF TF) |}];
  test "cagney";
  [%expect {| VALID (KKN KKN) |}];
  test "tagliaro";
  [%expect {| VALID (TKLR TLR) |}];
  test "biaggi";
  [%expect {| VALID (PJ PK) |}];
  test "jose";
  [%expect {| VALID (HS HS) |}];
  test "san jacinto";
  [%expect {| VALID (SNHS SNHS) |}];
  test "Yankelovich";
  [%expect {| VALID (ANKL ANKL) |}];
  test "Jankelowicz";
  [%expect {| VALID (JNKL ANKL) |}];
  test "bajador";
  [%expect {| VALID (PJTR PHTR) |}];
  test "cabrillo";
  [%expect {| VALID (KPRL KPR) |}];
  test "gallegis";
  [%expect {| VALID (KLJS KLKS) |}];
  test "dumb";
  [%expect {| VALID (TM TM) |}];
  test "thumb";
  [%expect {| VALID (0M TM) |}];
  test "campbell";
  [%expect {| VALID (KMPL KMPL) |}];
  test "raspberry";
  [%expect {| VALID (RSPR RSPR) |}];
  test "rogier";
  [%expect {| VALID (RJ RJR) |}];
  test "hochmeier";
  [%expect {| VALID (HKMR HKMR) |}];
  test "island";
  [%expect {| VALID (ALNT ALNT) |}];
  test "isle";
  [%expect {| VALID (AL AL) |}];
  test "carlisle";
  [%expect {| VALID (KRLL KRLL) |}];
  test "carlysle";
  [%expect {| VALID (KRLL KRLL) |}];
  test "sugar";
  [%expect {| VALID (XKR SKR) |}];
  test "smith";
  [%expect {| VALID (SM0 XMT) |}];
  test "schmidt";
  [%expect {| VALID (XMT SMT) |}];
  test "snider";
  [%expect {| VALID (SNTR XNTR) |}];
  test "schneider";
  [%expect {| VALID (XNTR SNTR) |}];
  test "school";
  [%expect {| VALID (SKL SKL) |}];
  test "schooner";
  [%expect {| VALID (SKNR SKNR) |}];
  test "schermerhorn";
  [%expect {| VALID (XRMR SKRM) |}];
  test "schenker";
  [%expect {| VALID (XNKR SKNK) |}];
  test "resnais";
  [%expect {| VALID (RSN RSNS) |}];
  test "artois";
  [%expect {| VALID (ART ARTS) |}];
  test "thomas";
  [%expect {| VALID (TMS TMS) |}];
  test "thames";
  [%expect {| VALID (TMS TMS) |}];
  test "Wasserman";
  [%expect {| VALID (ASRM FSRM) |}];
  test "Vasserman";
  [%expect {| VALID (FSRM FSRM) |}];
  test "Uomo";
  [%expect {| VALID (AM AM) |}];
  test "Womo";
  [%expect {| VALID (AM FM) |}];
  test "Arnow";
  [%expect {| VALID (ARN ARNF) |}];
  test "Arnoff";
  [%expect {| VALID (ARNF ARNF) |}];
  test "filipowicz";
  [%expect {| VALID (FLPT FLPF) |}];
  test "breaux";
  [%expect {| VALID (PR PR) |}];
  test "zhao";
  [%expect {| VALID (J J) |}];
  (* Soundex test suite from Rosetta Code *)
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
  (* Test cases added from algorithm development *)
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
  [%expect {| VALID (KRSN KRSN) |}];
  test "Knuth";
  [%expect {| VALID (N0 NT) |}];
  test "sanju";
  [%expect {| VALID (SNJ SNJ) |}];
  test "much";
  [%expect {| VALID (MK MK) |}];
  test "needed";
  [%expect {| VALID (NTT NTT) |}];
  test "much-needed";
  [%expect {| VALID (MKNT MKNT) |}];
  test "faux";
  [%expect {| VALID (F F) |}];
  test "pas";
  [%expect {| VALID (PS PS) |}];
  test "faux pas";
  [%expect {| VALID (FKSP FKSP) |}];
  test "chore";
  [%expect {| VALID (XR XR) |}];
  test "choreography";
  [%expect {| VALID (XRKR XRKR) |}];
  test "cocciuto";
  [%expect {| VALID (KXT KXT) |}];
  test "róngnà";
  [%expect {| VALID (RNN RNKN) |}];
  test "josef";
  [%expect {| VALID (JSF HSF) |}];
  test "sch";
  [%expect {| VALID (X S) |}];
  test "chvil";
  [%expect {| VALID (KFL KFL) |}];
  test "ghi";
  [%expect {| VALID (J J) |}];
  test "ache";
  [%expect {| VALID (AX AK) |}];
  test "achy";
  [%expect {| VALID (AX AK) |}];
  test "schutzzauber";
  [%expect {| VALID (XTSP XTTS) |}]

let english = [%blob "english.txt"]

let french = [%blob "french.txt"]

let pinyin = [%blob "pinyin.txt"]

let languages =
  [
    [%blob "words/br.txt"];
    [%blob "words/cs.txt"];
    [%blob "words/en.txt"];
    [%blob "words/et.txt"];
    [%blob "words/fr.txt"];
    [%blob "words/hu.txt"];
    [%blob "words/it.txt"];
    [%blob "words/ms.txt"];
    [%blob "words/pl.txt"];
    [%blob "words/sk.txt"];
    [%blob "words/sr.txt"];
    [%blob "words/tr.txt"];
    [%blob "words/bs.txt"];
    [%blob "words/da.txt"];
    [%blob "words/eo.txt"];
    [%blob "words/eu.txt"];
    [%blob "words/gl.txt"];
    [%blob "words/id.txt"];
    [%blob "words/lt.txt"];
    [%blob "words/nl.txt"];
    [%blob "words/pt.txt"];
    [%blob "words/sl.txt"];
    [%blob "words/sv.txt"];
    [%blob "words/vi.txt"];
    [%blob "words/ca.txt"];
    [%blob "words/de.txt"];
    [%blob "words/es.txt"];
    [%blob "words/fi.txt"];
    [%blob "words/hr.txt"];
    [%blob "words/is.txt"];
    [%blob "words/lv.txt"];
    [%blob "words/no.txt"];
    [%blob "words/ro.txt"];
    [%blob "words/sq.txt"];
    [%blob "words/tl.txt"];
  ]

let test_language dict =
  String.split_lines dict
  |> List.fold ~init:(0, 0, "", []) ~f:(fun (good, bad, shortest, all) word ->
       let unicode = Unicode.create word in
       match cpp_dmetaphone unicode, Unicode.dmetaphone unicode with
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

let%expect_test "In depth" =
  test_language english;
  [%expect {|
    100% matching, 58109 words. |}];
  test_language french;
  [%expect {|
      100% matching, 22740 words. |}];
  test_language pinyin;
  [%expect {|
    100% matching, 8841 words. |}]

let%expect_test "Benchmark" =
  let benchmark dict =
    let t0 = Time_ns.now () in
    String.split_lines dict
    |> List.iter ~f:(fun word ->
         let unicode = Unicode.create word in
         let _code = Unicode.dmetaphone unicode in
         ()
       );
    let t1 = Time_ns.now () in
    print_endline
      (sprintf !"Time: %dms"
        ((Time_ns.diff t1 t0)
        |> Time_ns.Span.to_int_ms
        |> Int.round_nearest ~to_multiple_of:100
        ))
  in
  benchmark english;
  [%expect {| Time: 1600ms |}]

(* let%expect_test "Most common per language" =
  List.iter languages ~f:(fun lang -> test_language lang);
  [%expect
    {|
    100% matching, 7052 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 36346 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 50000 words.
    100% matching, 10665 words. |}] *)
