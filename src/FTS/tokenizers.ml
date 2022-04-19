open! Core

let simple =
  let regex = Re.Perl.re ~opts:[ `Multiline ] "\\s+" |> Re.compile in
  (fun raw -> Re.Seq.split regex raw |> Sequence.of_seq)

let ngram ~n = function
| word when String.length word <= 3 -> Sequence.singleton word
| word -> Sequence.init (String.length word - 2) ~f:(fun i -> String.slice word i (i + n))

let trigram = ngram ~n:3
