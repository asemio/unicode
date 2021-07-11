val valid_encodings : string list

type t [@@deriving sexp, compare, equal]

val create : ?encoding_name:string -> string -> t

val raw_bytes : t -> string

val utf8_bytes : t -> string

val cmp_bytes : t -> string

val to_upper : t -> t

val to_lower : t -> t

val to_title : t -> t

val length : t -> int

val is_non_blank : t -> bool

val unaccent : t -> t

val standardize : ?rep:string -> ?ignore:(string -> bool) -> ?filter:(char -> bool) -> t -> t

val dmetaphone : ?max_length:int -> t -> string * string

val trim : ?unicode_ws:bool -> t -> t

val squish : t -> t

val split_glyphs_cmp : t -> string array

val slice : t -> int -> int -> t

val ( = ) : t -> t -> bool

val ( <> ) : t -> t -> bool

val ( < ) : t -> t -> bool

val ( <= ) : t -> t -> bool

val ( >= ) : t -> t -> bool

val ( > ) : t -> t -> bool
