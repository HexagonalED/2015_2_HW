{
open Lexing	
open Parser

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
  | Keyword_as_label of string
  | Literal_overflow of string
;;

exception Error of error * Location.t
exception NotFound;;

let keyword2token = function 
  |	"and"			-> AND
  | "as"			-> AS
  | "assert"		-> ASSERT
  | "begin"			-> BEGIN
  | "class"			-> CLASS
  | "constraint"	-> CONSTRAINT 
  | "do"			-> DO 
  | "done"			-> DONE 
  | "downto"		-> DOWNTO 
  | "else"			-> ELSE 
  | "end"			-> END 
  | "exception"		-> EXCEPTION 
  | "external"		-> EXTERNAL 
  | "false"			-> FALSE 
  | "for"			-> FOR 
  | "fun"			-> FUN 
  | "function"		-> FUNCTION 
  | "functor"		-> FUNCTOR 
  | "if"			-> IF 
  | "in"			-> IN 
  | "include"		-> INCLUDE 
  | "inherit"		-> INHERIT 
  | "initializer"	-> INITIALIZER 
  | "lazy"			-> LAZY 
  | "let"			-> LET 
  | "match"			-> MATCH 
  | "method"		-> METHOD 
  | "module"		-> MODULE 
  | "mutable"		-> MUTABLE 
  | "new"			-> NEW 
  | "object"		-> OBJECT 
  | "of"			-> OF 
  | "open"			-> OPEN 
  | "or"			-> OR 
  | "private"		-> PRIVATE 
  | "rec"			-> REC 
  | "sig"			-> SIG 
  | "struct"		-> STRUCT 
  | "then"			-> THEN 
  | "to"			-> TO 
  | "true"			-> TRUE 
  | "try"			-> TRY 
  | "type"			-> TYPE 
  | "val"			-> VAL 
  | "virtual"		-> VIRTUAL 
  | "when"			-> WHEN 
  | "while"			-> WHILE 
  | "with"			-> WITH 

  | "mod"			-> INFIXOP3("mod") 
  | "land"			-> INFIXOP3("land") 
  | "lor"			-> INFIXOP3("lor") 
  | "lxor"			-> INFIXOP3("lxor") 
  | "lsl"			-> INFIXOP4("lsl") 
  | "lsr"			-> INFIXOP4("lsr") 
  | "asr"			-> INFIXOP4("asr")
  | _ -> raise NotFound
 

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in (* /incr str_buff*)
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.empty;;
(*
let in_comment () = !comment_start_loc <> [];;
let comment_start_loc = ref [];;
*)
(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else String.sub s 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> s.[dst] <- c; remove (src + 1) (dst + 1)
  in remove 0 0

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255)
  then raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                    Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;


}

(*-------- header end----------*)


let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
    decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
			  
(*-----------------------------------------------------------------*)
rule token = parse
  | newline
      { 
        token lexbuf
      }
  | blank +
      { token lexbuf }
						
  | "_"
      { UNDERSCORE }
  | lowercase identchar * 
      { let s = Lexing.lexeme lexbuf in 
   	      try
		    keyword2token s
		  with NotFound ->
		  LIDENT s
	  }	  
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | int_literal
      { try
          INT (int_of_string(Lexing.lexeme lexbuf))
        with Failure _ ->
         raise (Error(Literal_overflow "int", Location.curr lexbuf))
      }
  | float_literal
      { FLOAT (remove_underscores(Lexing.lexeme lexbuf)) }
	  

  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string()) }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }

  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      }


  | "#"  { SHARP }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ".." { DOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "<-" { LESSMINUS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "[>" { LBRACKETGREATER }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  | ">]" { GREATERRBRACKET }
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }

  | "!=" { INFIXOP0 "!=" }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "-." { MINUSDOT }

  | "!" symbolchar *
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar +
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3(Lexing.lexeme lexbuf) }
  
  | "(*"		{ comments 0 lexbuf } 

  | eof { EOF }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0), 
	  				Location.curr lexbuf))
	  }
and comments level = parse
  | "*)"	{  if level = 0 then token lexbuf
  			   else comments (level-1) lexbuf  }
  | "(*"	{ comments (level+1) lexbuf	}
  | _		{ comments level lexbuf }
  | eof		{ print_endline "comments are not closed";
  		  raise End_of_file
		}
and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string_char(char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        let s = Lexing.lexeme lexbuf in
        for i = 0 to String.length s - 1 do
          store_string_char s.[i];
        done;
        string lexbuf
      }
  | eof
      { raise (Error (Unterminated_string, !string_start_loc)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

	  
(*------trail----------*)
{
	
}

