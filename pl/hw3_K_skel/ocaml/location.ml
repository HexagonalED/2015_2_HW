open Lexing

type t = { 
	loc_start: position; 
	loc_end: position; };;

(* empty : t
   create an empty 't' object.
   [dummy_pos] is defined in stdlib/lexing.ml *)
let empty = { 
	loc_start = dummy_pos; 
	loc_end = dummy_pos; };;

(* curr : Lexing.lexbuf -> t
   get the location of the current token from the [lexbuf] *)
let curr lexbuf = {
	loc_start = lexbuf.lex_start_p;
	loc_end = lexbuf.lex_curr_p;
};;

(* symbol_rloc : unit -> t 
   get the position of the string that matches the left-hand side of the rule*)
let symbol_rloc () = {
	loc_start = Parsing.symbol_start_pos ();
	loc_end = Parsing.symbol_end_pos ();
};;

(* rhs_loc : int -> t 
   get the position of the string that matches the [n]th item 
   on the right-hand side of the rule [1] is for the leftmost item. *)
let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
};;

(* for printing error messges *)
let (msg_file, msg_line, msg_chars, msg_to, msg_colon, msg_head) =
  ("File \"", "\", line ", ", characters ", "-", ":", "")
  
(* print : string -> t -> string -> unit*)
let print filename loc err_msg =
  let startchar = loc.loc_start.pos_cnum in
  let endchar = loc.loc_end.pos_cnum in
  let (startchar, endchar) =
    if startchar < 0 then (0, 1) else (startchar, endchar)
  in
    print_string (msg_file^filename);
    print_string (msg_chars^(string_of_int startchar));
    print_string (msg_to^(string_of_int endchar)^msg_colon);
	print_string ("\n" ^ err_msg ^ "\n")

