
/* $Id: parser.mly, 2007/07/29 Taekyung Kim $ */

/* The parser definition */

%{

open Location
open Parsetree;;

let raise_error str = 
  raise (Syntaxerr.Error str)

(* mktyp : core_type_desc -> core_type *)
let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_rloc() }
  
(* mkpat : pattern_desc -> pattern *)
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_rloc() }
  
(* mkexp : expression_desc -> expression *)
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_rloc() }
  
(* mkstr : structure_item_desc -> structure_item *)
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_rloc() }
  
(* mkinfix : expression -> string -> expression -> expression  *)
let mkinfix arg1 name arg2 =
     mkexp(Pexp_apply(
	          {pexp_desc = Pexp_ident(Lident name);
			   pexp_loc = rhs_loc 2 }, [arg1;arg2]))
(* mkuminus : string -> expression -> expression *)
let mkuminus op expr = 
  match op, expr.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | "-" , Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float ("-" ^ f)))
  | "-." , Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float ("-" ^ f)))
  | _, _ ->
      mkexp(Pexp_apply({pexp_desc = Pexp_ident(Lident ("~"^op)); pexp_loc = rhs_loc 1} , [expr]))
(*  | _ , _ -> raise_error "unknown uminus"  *)
 
(* mklistpat : pattern list -> pattern 
   To make 'list' pattern by using construct and tuple  *)
let rec mklistpat plist = 
  match plist with
    [] -> mkpat(Ppat_construct(Lident "[]", None))
  | hpat::tpat -> 
      let tlist = mklistpat tpat in
	  let loc = {loc_start = hpat.ppat_loc.loc_start;
	             loc_end = tlist.ppat_loc.loc_end; }
	  in
	  let tuple_pat = {ppat_desc = Ppat_tuple [hpat; tlist]; ppat_loc = loc} in
	  {ppat_desc = Ppat_construct(Lident "::", Some tuple_pat); ppat_loc = loc}            
(* mklistexp : expression list -> expression 
   To make 'list' expression by using construct and tuple *)
let rec mklistexp elist =
  match elist with
    [] -> mkexp(Pexp_construct(Lident "[]", None))
  | hexp::texp ->
      let tlist = mklistexp texp in
	  let loc = {loc_start = hexp.pexp_loc.loc_start;
	             loc_end = tlist.pexp_loc.loc_end; }
	  in
	  let tuple_exp = {pexp_desc = Pexp_tuple [hexp; tlist]; pexp_loc =loc} in
	  {pexp_desc = Pexp_construct(Lident "::", Some tuple_exp); pexp_loc = loc}


%}

/* Tokens  */
/* include useless keywords such as ASSERT,CLASS, FOR and WHILE */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PLUS
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUESTIONQUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH

/* (* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*) */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation

%%

/* Entry points */

implementation:
    structure EOF                        { $1 }
;

/* Toplevel structure */
structure:
    structure_tail                              { $1  }
  | seq_expr structure_tail  
     { { pstr_desc = Pstr_eval $1; pstr_loc = $1.pexp_loc } :: $2}
;
structure_tail:
    /* empty */                                 { [] }
  | SEMISEMI                                    { [] }
  | SEMISEMI seq_expr structure_tail            
     { { pstr_desc = Pstr_eval ($2); pstr_loc = $2.pexp_loc } :: $3 }
  | SEMISEMI structure_item structure_tail      { $2 :: $3 }
  | structure_item structure_tail               { $1 :: $2 }
;
structure_item:
    LET rec_flag let_bindings
      { match $3 with 
	      [{ppat_desc = Ppat_any}, exp] -> mkstr(Pstr_eval exp)
		| _ -> mkstr(Pstr_value($2, $3)) }
  | TYPE type_declarations
      { mkstr(Pstr_type($2)) } 
  | EXCEPTION UIDENT constructor_arguments
      { mkstr(Pstr_exception($2,$3))  }
  | OPEN mod_longident
      { mkstr(Pstr_open $2) }
;

/* Core expressions */
seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1,$3)) }
;
expr:
    simple_expr %prec below_SHARP
      { $1  }
  | simple_expr simple_expr_list
      { mkexp(Pexp_apply($1, $2)) }
  | LET rec_flag let_bindings IN seq_expr
      { mkexp(Pexp_let($2,$3,$5)) }
  | FUNCTION opt_bar match_cases
      { mkexp(Pexp_function($3)) }
  | FUN simple_pattern fun_def
      { mkexp(Pexp_function([$2,$3])) }
  | MATCH seq_expr WITH opt_bar match_cases
      { mkexp(Pexp_match($2, $5)) }
  | TRY seq_expr WITH opt_bar match_cases
      { mkexp(Pexp_try($2,$5)) }
  | TRY seq_expr WITH error
      { raise_error "try, with (|)" }
  | expr_comma_list %prec below_COMMA
      { mkexp(Pexp_tuple($1)) }
  | constr_longident simple_expr %prec below_SHARP
      { mkexp(Pexp_construct($1, Some $2)) }
  | IF seq_expr THEN expr ELSE expr
      { mkexp(Pexp_ifthenelse($2,$4,Some $6)) }
  | IF seq_expr THEN expr
      { mkexp(Pexp_ifthenelse($2,$4,None)) }
  | expr COLONCOLON expr  
      { mkexp(Pexp_construct(Lident "::", Some(mkexp(Pexp_tuple[$1;$3])))) }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
      { mkexp(Pexp_construct(Lident "::", Some(mkexp(Pexp_tuple[$5;$7])))) } 
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3  }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3  }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3  }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3  }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3  }
  | expr PLUS expr
      { mkinfix $1 "+" $3  }
  | expr MINUS expr
      { mkinfix $1 "-" $3  }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 }
  | expr STAR expr
      { mkinfix $1 "*" $3 }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 }
  | expr LESS expr
      { mkinfix $1 "<" $3 }
  | expr GREATER expr
      { mkinfix $1 ">" $3 }
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident $1)  }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident %prec prec_constant_constructor
      { mkexp(Pexp_construct($1,None)) }
  | LPAREN seq_expr RPAREN
      { $2 }
  | LPAREN seq_expr error
      { raise_error "missing )" }
  | BEGIN seq_expr END
      { $2 }
  | BEGIN END
      { mkexp(Pexp_construct(Lident "()", None))}
  | BEGIN seq_expr error
      { raise_error "missing end"  }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      {  match mklistexp $2 with
            {pexp_desc = p; pexp_loc = _ }  ->
              {pexp_desc = p; pexp_loc = symbol_rloc()} }
  | LBRACKET expr_semi_list opt_semi error
      { raise_error "missing ]"  }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(
	          {pexp_desc = Pexp_ident(Lident $1);
			   pexp_loc = rhs_loc 1 }, [$2])) } 
;
simple_expr_list:
    simple_expr
      { [$1] }
  | simple_expr simple_expr_list
      { $1 :: $2 }
;

let_bindings:
    let_binding                                 { [$1] }
  | let_bindings AND let_binding                { $1 @ [$3] }
;
let_binding:
    val_ident fun_binding
      { ({ppat_desc = Ppat_var $1; ppat_loc = rhs_loc 1}, $2) }
  | pattern EQUAL seq_expr
      { ($1,$3) }
;
fun_binding:
    strict_binding
      { $1  }
  | type_constraint EQUAL seq_expr
      { mkexp(Pexp_constraint($3, $1))  }
;
strict_binding:
    EQUAL seq_expr
      { $2  }
  | simple_pattern fun_binding
      { mkexp(Pexp_function([$1,$2])) }
;
match_cases:
    pattern match_action                        { [($1,$2)]  }
  | match_cases BAR pattern match_action        { $1 @ [($3,$4)]  }
;
fun_def:
    match_action                                { $1  }
  | simple_pattern fun_def
      { mkexp(Pexp_function([$1,$2])) }
;
match_action:
    MINUSGREATER seq_expr                       { $2  }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $1 @ [$3]  }
  | expr COMMA expr                             { [$1; $3]  }
;
expr_semi_list:
    expr                                        { [$1] }
  | expr_semi_list SEMI expr                    { $1 @ [$3]  }
;
type_constraint:
    COLON core_type                             { Some $2  }
  | COLON error                                 { raise_error "missing type"  }
;

/* Patterns */
pattern:
    simple_pattern
      { $1  }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1,$3)) }
  | pattern_comma_list  %prec below_COMMA
      { mkpat(Ppat_tuple($1))  }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2))   }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_construct(Lident "::", Some(mkpat(Ppat_tuple([$1; $3]))))) }
  | pattern BAR pattern
      { mkpat(Ppat_or($1,$3)) }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpat(Ppat_var $1)  }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | constr_longident
      { mkpat(Ppat_construct($1,None)) }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { match mklistpat $2 with 
 	      {ppat_desc = p; ppat_loc = _} -> 
	           {ppat_desc = p; ppat_loc = symbol_rloc()} }			   
  | LBRACKET pattern_semi_list opt_semi error
      { raise_error "missing ]"   }
  | LPAREN pattern RPAREN
      { $2 }
  | LPAREN pattern error
      { raise_error "missing )"  }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2,$4))  }
  | LPAREN pattern COLON core_type error
      { raise_error "missing )"  }
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $1 @ [$3] }
  | pattern COMMA pattern                       { [$1; $3] }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $1 @ [$3] }
;

/* Type declarations */
type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $1 @ [$3] }
;

type_declaration:
    type_parameters LIDENT type_kind
      { ($2, {ptype_params = $1;
	          ptype_kind = $3;
			  ptype_loc = symbol_rloc()}) }
;
type_kind:
    /*empty*/
      { Ptype_abstract(None) }
  | EQUAL core_type
      { Ptype_abstract(Some $2) }
  | EQUAL constructor_declarations
      { Ptype_variant($2)  }
  | EQUAL private_flag BAR constructor_declarations
      { Ptype_variant($4)  }
;

type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1]  }
  | LPAREN type_parameter_list RPAREN           { $2 }
;

type_parameter:
   QUOTE ident                   { $2 }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $1 @ [$3] }
;

constructor_declarations:
    constructor_declaration                     { [$1]  }
  | constructor_declarations BAR constructor_declaration { $1 @ [$3]  }
;
constructor_declaration:
    constr_ident constructor_arguments          { ($1,$2, symbol_rloc())  }
;
constructor_arguments:
    /*empty*/                                   { [] }
  | OF core_type_list                           { $2 }
;

/* Core types */

core_type:
    core_type2                                 { $1 }
;
core_type2:
    simple_core_type_or_tuple                  { $1 }
  | core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow ($1, $3)) }
;

simple_core_type:
    simple_core_type2  %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ -> raise_error "???" }
;
simple_core_type2:
    QUOTE ident
      { mktyp(Ptyp_var ($2)) }
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  | type_longident
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type2 type_longident
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident
      { mktyp(Ptyp_constr($4, List.rev $2)) }
;
simple_core_type_or_tuple:
    simple_core_type                            { $1 }
  | simple_core_type STAR core_type_list
      { mktyp( Ptyp_tuple($1::$3))  }
;
core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $1 @ [$3] }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $1 @ [$3] }
;

/* Constants */

constant:
    INT                                         { Const_int ($1) }
  | CHAR                                        { Const_char ($1) }
  | STRING                                      { Const_string ($1) }
  | FLOAT                                       { Const_float ($1) }
;
signed_constant:
    constant                                    { $1 }
  | MINUS INT                                   { Const_int(-1 * $2) }
  | MINUS FLOAT                                 { Const_float("-" ^ $2) }
;
/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1  }
  | LIDENT                                      { $1 }
;

val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;

operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | PLUS                                        { "+" }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;

type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;

mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
;

/* Miscellaneous */
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;

private_flag:
    /* empty */                                 { () }
  | PRIVATE                                     { () }
;

opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;

subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
%%
