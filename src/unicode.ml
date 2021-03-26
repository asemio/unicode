open! Core_kernel

let valid_encodings =
  [
    "UTF-8";
    "UTF-16";
    "UTF-16LE";
    "UTF-16BE";
    "ANSI_X3.4-1968";
    "ISO-IR-6";
    "ANSI_X3.4-1986";
    "ISO_646.IRV:1991";
    "ASCII";
    "ISO646-US";
    "US-ASCII";
    "US";
    "IBM367";
    "CP367";
    "CSASCII";
    "ISO_8859-1:1987";
    "ISO-IR-100";
    "ISO_8859-1";
    "ISO-8859-1";
    "LATIN1";
    "L1";
    "IBM819";
    "CP819";
    "CSISOLATIN1";
  ]

type standard_encoding =
  [ `ISO_8859_1
  | `US_ASCII
  | `UTF_16
  | `UTF_16BE
  | `UTF_16LE
  | `UTF_8
  ]
[@@deriving sexp, compare, equal]

type original_encoding =
  | Standard     of standard_encoding
  | Windows_1252
[@@deriving sexp, compare, equal]

type lookup = {
  indexes: int array;
  lengths: int array;
  length: int;
}

type with_length = {
  bytes: string;
  length: int;
}
[@@deriving sexp, compare, equal]

type kind =
  | Passthrough    of string
  | Raw            of {
      raw: string;
      encoding: original_encoding;
    }
  | Utf8           of string
  | Utf8_len       of with_length
  | Normalized     of string
  | Normalized_len of with_length
[@@deriving sexp, compare, equal]

type t = { mutable value: kind } [@@deriving sexp, compare, equal]

type enc =
  [ `Ok
  | `Partial
  ]

let recode ?nln ?encoding out_encoding (src : [ `Channel of In_channel.t | `String  of string ])
   (dst : [ `Channel of Out_channel.t | `Buffer  of Buffer.t ]) =
  let rec loop d e =
    match Uutf.decode d with
    | `Uchar _ as u ->
      let (_ : enc) = Uutf.encode e u in
      loop d e
    | `End ->
      let (_ : enc) = Uutf.encode e `End in
      ()
    | `Malformed _ ->
      let (_ : enc) = Uutf.encode e (`Uchar Uutf.u_rep) in
      loop d e
    | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding src in
  let e = Uutf.encoder out_encoding dst in
  loop d e

let create ?encoding_name raw =
  let value =
    match encoding_name with
    | None -> Passthrough raw
    | Some name -> (
      match String.uppercase name with
      | "WINDOWS-1252"
       |"CP-1252" ->
        Raw { raw; encoding = Windows_1252 }
      | _ -> (
        match Uutf.encoding_of_string name with
        | None -> Passthrough raw
        | Some encoding -> Raw { raw; encoding = Standard encoding }
      )
    )
  in
  { value }

let convert_1252 str =
  try Utf8conv.utf8_of_windows1252 str with
  | _ ->
    failwithf "Not a valid Windows-1252/CP-1252 string, are you sure it's the right format? : %s" str ()

let to_utf8 encoding str =
  let buf = Buffer.create (String.length str + 10) in
  recode ?encoding `UTF_8 (`String str) (`Buffer buf);
  Buffer.contents buf

let normalize str = Uunf_string.normalize_utf_8 `NFC str

let raw_bytes box =
  match box.value with
  | Raw { raw; _ } -> raw
  | Passthrough bytes
   |Utf8 bytes
   |Utf8_len { bytes; _ }
   |Normalized bytes
   |Normalized_len { bytes; _ } ->
    bytes

let utf8_bytes box =
  match box.value with
  | Raw { raw; encoding } ->
    let utf8 =
      match encoding with
      | Windows_1252 -> convert_1252 raw
      | Standard enc -> to_utf8 (Some enc) raw
    in
    box.value <- Utf8 utf8;
    utf8
  | Passthrough raw ->
    let utf8 = to_utf8 None raw in
    box.value <- Utf8 utf8;
    utf8
  | Utf8 bytes
   |Utf8_len { bytes; _ }
   |Normalized bytes
   |Normalized_len { bytes; _ } ->
    bytes

let cmp_bytes box =
  match box.value with
  | Raw _
   |Passthrough _ ->
    let bytes = utf8_bytes box in
    let normalized = normalize bytes in
    box.value <- Normalized normalized;
    normalized
  | Utf8 bytes ->
    let normalized = normalize bytes in
    box.value <- Normalized normalized;
    normalized
  | Utf8_len { bytes; length } ->
    let normalized = normalize bytes in
    box.value <- Normalized_len { bytes = normalized; length };
    normalized
  | Normalized bytes
   |Normalized_len { bytes; _ } ->
    bytes

let rebox ?length box bytes =
  let value =
    match box.value, length with
    | Normalized _, Some length
     |Normalized_len _, Some length ->
      Normalized_len { bytes; length }
    | Normalized _, None
     |Normalized_len _, None ->
      Normalized bytes
    | Raw _, Some length
     |Passthrough _, Some length
     |Utf8 _, Some length
     |Utf8_len _, Some length ->
      Utf8_len { bytes; length }
    | Raw _, None
     |Passthrough _, None
     |Utf8 _, None
     |Utf8_len _, None ->
      Utf8 bytes
  in
  { value }

(* It's safe to operate at the codepoint level for anything related to casing *)
let map cmap s =
  let buf = Buffer.create (String.length s + 10) in
  let add_map () _ u =
    let u =
      match u with
      | `Malformed _ -> Uutf.u_rep
      | `Uchar u -> u
    in
    match cmap u with
    | `Self -> Uutf.Buffer.add_utf_8 buf u
    | `Uchars us -> List.iter us ~f:(Uutf.Buffer.add_utf_8 buf)
  in
  Uutf.String.fold_utf_8 add_map () s;
  Buffer.contents buf

let to_upper box = { value = Utf8 (map Uucp.Case.Map.to_upper (utf8_bytes box)) }

let to_lower box = { value = Utf8 (map Uucp.Case.Map.to_lower (utf8_bytes box)) }

let to_title box =
  let utf8 = utf8_bytes box in
  let buf = Buffer.create (String.length utf8 + 10) in
  let (_ : bool) =
    Uutf.String.fold_utf_8
      (fun acc _ -> function
        | `Malformed _ ->
          Uutf.Buffer.add_utf_8 buf Uutf.u_rep;
          false
        | `Uchar u -> (
          match Uucp.White.is_white_space u, acc with
          | true, _ ->
            Uutf.Buffer.add_utf_8 buf u;
            true
          | false, true -> (
            match Uucp.Case.Map.to_upper u with
            | `Self ->
              Uutf.Buffer.add_utf_8 buf u;
              false
            | `Uchars us ->
              List.iter us ~f:(Uutf.Buffer.add_utf_8 buf);
              false
          )
          | false, false -> (
            match Uucp.Case.Map.to_lower u with
            | `Self ->
              Uutf.Buffer.add_utf_8 buf u;
              false
            | `Uchars us ->
              List.iter us ~f:(Uutf.Buffer.add_utf_8 buf);
              false
          )
        ))
      true utf8
  in
  { value = Utf8 (Buffer.contents buf) }

let length box =
  match box.value with
  | Raw _
   |Passthrough _ ->
    let bytes = utf8_bytes box in
    let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc _ -> acc + 1) 0 bytes in
    box.value <- Utf8_len { bytes; length };
    length
  | Utf8 bytes ->
    let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc _ -> acc + 1) 0 bytes in
    box.value <- Utf8_len { bytes; length };
    length
  | Normalized bytes ->
    let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc _ -> acc + 1) 0 bytes in
    box.value <- Normalized_len { bytes; length };
    length
  | Utf8_len { length; _ }
   |Normalized_len { length; _ } ->
    length

let is_non_blank box =
  let bytes = raw_bytes box in
  String.exists bytes ~f:(fun x -> not (Char.is_whitespace x))

let num_bytes = function
| x when x <= 0x7f -> 1
| x when x <= 0x7ff -> 2
| x when x <= 0xffff -> 3
| _ -> 4

(* Left here for documentation purposes. This function generates the base file for Unaccent_rules.ml. *)
(*
let generate_replacements () =
  let buf = Buffer.create 1024 in

  Buffer.add_string buf "let replacements = [\n";
  List.iter Unaccent_rules.replacements ~f:(fun (a, b) ->
    Buffer.add_string buf (sprintf "\t{rep|%s|rep}, {rep|%s|rep};\n" (normalize a) (normalize b))
  );
  Buffer.add_string buf "]\n";

  Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] ~mode:Output "unaccent.test" (fun chan ->
    Lwt_io.write chan (Buffer.contents buf)
  )
*)

let unaccent box =
  let bytes = cmp_bytes box in
  let buf = Buffer.create (String.length bytes + 10) in
  Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun () glyph ->
      match String.Map.find Unaccent_rules.lookup glyph with
      | Some rep -> Buffer.add_string buf rep
      | None -> Buffer.add_string buf glyph)
    () bytes;
  { value = Normalized (Buffer.contents buf) }

let standardize ?(rep = ' ') box =
  let bytes = cmp_bytes box in
  let buf = Buffer.create (String.length bytes + 10) in
  let (_ : bool) =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun top_prev_junk glyph ->
        let chunk =
          match String.Map.find Unaccent_rules.lookup glyph with
          | Some rep -> rep
          | None -> glyph
        in
        String.fold chunk ~init:top_prev_junk ~f:(fun prev_junk c ->
            if Char.is_alphanum c
            then begin
              if prev_junk && Int.(Buffer.length buf <> 0) then Buffer.add_char buf rep;
              Buffer.add_char buf (Char.lowercase c);
              false
            end
            else true))
      false bytes
  in
  { value = Normalized (Buffer.contents buf) }

let dmetaphone box =
  let bytes = unaccent box |> cmp_bytes in
  { value = Normalized (Dmetaphone.dmetaphone bytes) }

let unicode_trim box =
  let bytes = utf8_bytes box in
  let left, right =
    Uutf.String.fold_utf_8
      (fun acc i m ->
        match m with
        | `Malformed _ -> acc
        | `Uchar u -> (
          match acc with
          | None, _ when not (Uucp.White.is_white_space u) -> Some i, Some (i, u)
          | (None, _) as x -> x
          | (Some _ as x), _ when not (Uucp.White.is_white_space u) -> x, Some (i, u)
          | Some _, _ -> acc
        ))
      (None, None) bytes
  in
  let sliced =
    String.slice bytes (Option.value ~default:0 left)
      (Option.value_map ~default:0 ~f:(fun (i, u) -> i + (Uchar.to_scalar u |> num_bytes)) right)
  in
  rebox box sliced

let squish box =
  let bytes = utf8_bytes box in
  let buf = Buffer.create (String.length bytes) in
  let _, _, length =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun acc glyph ->
        Uutf.String.fold_utf_8
          (fun (after_first, prev_ws, len) i m ->
            begin
              match m with
              | `Malformed _ -> `Text Uutf.u_rep
              | `Uchar u when Uucp.White.is_white_space u -> `WS
              | `Uchar u -> `Text u
            end
            |> function
            | `WS -> after_first, true, len
            | `Text u when after_first && prev_ws ->
              Buffer.add_char buf ' ';
              Uutf.Buffer.add_utf_8 buf u;
              true, false, if Int.(i = 0) then Int.(len + 2) else len
            | `Text u ->
              Uutf.Buffer.add_utf_8 buf u;
              true, false, if Int.(i = 0) then Int.(len + 1) else len)
          acc glyph)
      (false, false, 0) bytes
  in
  let squished = Buffer.contents buf in
  rebox ~length box squished

let trim box =
  let bytes = raw_bytes box in
  let trimmed = String.strip bytes in
  rebox box trimmed

let index box =
  let bytes = utf8_bytes box in
  let indexes = Queue.create ~capacity:30 () in
  let lengths = Queue.create ~capacity:30 () in
  let (_ : int) =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun pos m ->
        let len = String.length m in
        Queue.enqueue indexes pos;
        Queue.enqueue lengths len;
        pos + len)
      0 bytes
  in

  ( bytes,
    { indexes = Queue.to_array indexes; lengths = Queue.to_array lengths; length = Queue.length indexes }
  )

let slice box u_from u_to =
  let bytes, lookup = index box in
  let result =
    if Int.(u_from >= lookup.length) || Int.(lookup.length - u_from + u_to <= 0)
    then ""
    else (
      let norm_from = if Int.is_negative u_from then lookup.length + u_from else u_from in
      let norm_to = if Int.is_negative u_to then lookup.length + u_to - 1 else u_to - 1 in
      let s_from =
        try lookup.indexes.(norm_from) with
        | _ -> 0
      in
      let s_to =
        try lookup.indexes.(norm_to) + lookup.lengths.(norm_to) with
        | _ -> Array.last lookup.indexes + Array.last lookup.lengths
      in
      if s_from > s_to then "" else String.slice bytes s_from s_to
    )
  in
  rebox box result

let ( = ) box1 box2 = String.( = ) (cmp_bytes box1) (cmp_bytes box2)

let ( <> ) box1 box2 = String.( <> ) (cmp_bytes box1) (cmp_bytes box2)

let ( < ) box1 box2 = String.( < ) (cmp_bytes box1) (cmp_bytes box2)

let ( <= ) box1 box2 = String.( <= ) (cmp_bytes box1) (cmp_bytes box2)

let ( >= ) box1 box2 = String.( >= ) (cmp_bytes box1) (cmp_bytes box2)

let ( > ) box1 box2 = String.( > ) (cmp_bytes box1) (cmp_bytes box2)
