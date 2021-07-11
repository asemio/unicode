open! Core_kernel

external stub_dmetaphone : string -> string * string = "stub_dmetaphone"

let dmetaphone = function
| "" -> failwith "Could not generate a dmetaphone code, empty string"
| bytes -> (
  match stub_dmetaphone bytes with
  | "", _ -> failwith "Could not generate a dmetaphone code, invalid string"
  | code, _ -> code
)

let dmetaphone_both = function
| "" -> failwith "Could not generate a dmetaphone code, empty string"
| bytes -> stub_dmetaphone bytes
