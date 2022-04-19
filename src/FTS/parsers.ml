open! Core

let ( >>? ) seq = function
| None -> seq
| Some f -> Sequence.filter ~f seq

let ( >>| ) seq = function
| None -> seq
| Some f -> Sequence.map ~f seq

let ( >>= ) seq = function
| None -> seq
| Some f -> Sequence.bind ~f seq

let at_least n s = String.length s > n

let create ~tokenizer ?filter_pre ~normalizer ?mapper ?filter_post raw =
  tokenizer raw
  >>? filter_pre
  |> Sequence.map ~f:normalizer
  >>= mapper
  >>? filter_post
  |> Sequence.filter ~f:(at_least 0)

(** Simple tokenizer + name normalizer + dmetaphone mapper *)
let name raw =
  create raw ~tokenizer:Tokenizers.simple ~filter_pre:(at_least 2) ~normalizer:Normalizers.name
    ~mapper:(fun s ->
      let x, y = Unicode.create s |> Unicode.dmetaphone in
      Array.to_sequence_mutable [| x; sprintf "_%s" y |])

let tag raw = create raw ~tokenizer:Tokenizers.simple ~normalizer:Normalizers.tag

let english raw =
  create raw ~tokenizer:Tokenizers.simple ~normalizer:Normalizers.english ~filter_post:(fun s ->
      String.Set.mem English_stopwords.set s |> not)
