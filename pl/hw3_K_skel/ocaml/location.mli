
type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
}

val empty : t
val curr : Lexing.lexbuf -> t
val symbol_rloc: unit -> t
val rhs_loc: int -> t

val print: string -> t -> string -> unit

