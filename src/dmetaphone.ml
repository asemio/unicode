open! Core_kernel

external stub_dmetaphone : string -> string = "stub_dmetaphone"

let dmetaphone = function
| "" -> failwith "Could not generate a dmetaphone code, empty string"
| bytes ->
  begin match stub_dmetaphone bytes with
  | "" -> failwith "Could not generate a dmetaphone code, invalid string"
  | code -> code
  end
