open! Core_kernel

let create ~filter raw = Unicode.create raw |> Unicode.standardize ~rep:"" ~filter |> Unicode.raw_bytes

let name raw =
  create raw ~filter:(function
    | 'A' .. 'Z'
     |'a' .. 'z'
     |'-' ->
      true
    | _ -> false
    )

let tag raw =
  let leading = ref true in
  create raw ~filter:(function
    | '0' -> not !leading
    | '1' .. '9'
     |'A' .. 'Z'
     |'a' .. 'z'
     |'-'
     |'.'
     |'@' ->
      if !leading then leading := false;
      true
    | _ ->
      if !leading then leading := false;
      false
    )

let english raw =
  create raw ~filter:(function
    | 'A' .. 'Z'
     |'a' .. 'z'
     |'0' .. '9'
     |'@'
     |'-' ->
      true
    | _ -> false
    )
