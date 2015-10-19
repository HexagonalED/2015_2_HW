(* $Id: parsetree.mli, 2007/07/16 17:--:--
   Hyeonseung Im and Taekyung Kim $ *)

(* Abstract syntax tree produced by parsing *)

(* Auxiliary a.s.t. types *)

type long_ident =
    Lident of string
  | Ldot of long_ident * string
      
type constant = 
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string

type rec_flag = Nonrecursive | Recursive

(* Type expressions for the core language *)

type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }

and core_type_desc = 
    Ptyp_any                                     (* _ *)
  | Ptyp_var of string                           (* ' ident *)
  | Ptyp_arrow of core_type * core_type          (* typexpr -> typexpr *)
  | Ptyp_tuple of core_type list                 (* typexpr * ... * typexpr *)
  | Ptyp_constr of long_ident * core_type list   (* typeconstr
                                                  * typexpr typeconstr
                                                  * ( typexpr {, typexpr} ) typeconstr *)

(* Value expressions for the core language *)

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any                                     (* _ *)
  | Ppat_var of string                           (* value-name *)
  | Ppat_constant of constant                    (* constant *)
  | Ppat_tuple of pattern list                   (* patter , ..., pattern *)
  | Ppat_construct of long_ident * pattern option 
                                                 (* constr if NONE
                                                    constr pattern if SOME pattern *)
  | Ppat_constraint of pattern * core_type       (* (pattern : typexpr) *)
  | Ppat_alias of pattern * string               (* pattern as value-name *)
  | Ppat_or of pattern * pattern                 (* pattern | pattern *)

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of long_ident                     (* value-name *)	
  | Pexp_constant of constant                    (* constants: int, char, string, float *)
  | Pexp_let of rec_flag * (pattern * expression) list * expression
                                                 (* let [rec] let-binding {and let-binding} in expr *)
  | Pexp_function of (pattern * expression) list (* function pattern -> expr | ... | pattern -> expr *)
  | Pexp_apply of expression * expression list   (* expr1 {expr}+ where expr1 is of function type *)
  | Pexp_match of expression * (pattern * expression) list    
                                                 (* match expr with pattern-matching *)
  | Pexp_try of expression * (pattern * expression) list      
                                                 (* try expr with pattern-matching *)
  | Pexp_tuple of expression list                (* expr, expr{, expr} *)
  | Pexp_construct of long_ident * expression option 
                                                 (* constr if NONE
                                                    constr expression if SOME expression *)
  | Pexp_ifthenelse of expression * expression * expression option   
                                                 (* if expr then expr [else expr] *)
  | Pexp_sequence of expression * expression     (* expr ; expr *)
  | Pexp_constraint of expression * core_type option  
                                                 (* type annotated expressions *)
                                           
(* Type declarations *)

and type_declaration =
  { ptype_params: string list;                   (* /*empty*/ | ' ident | (' ident_1, ..., ' ident_n) *)
    ptype_kind: type_kind;
    ptype_loc: Location.t }

and type_kind =
    Ptype_abstract of core_type option           (* abstract type if NONE, otherwise type abbreviation *)
  | Ptype_variant of (string * core_type list * Location.t) list
                                                 (* string is for constructor_identifier
                                                  * core_type list is for constructor_arguments, 
                                                  * if the list is empty, a constant constructor *)

and exception_declaration = core_type list

(* Value expressions for the module language *)

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression           (* let _ = expression *)
  | Pstr_value of rec_flag * (pattern * expression) list    
                                      (* let [rec] letbinding {and letbinding} 
                                       * letbinding ::= pattern = expression 
                                       *            | value-name {parameter} [:typexpr] = expression *)
                                                  
  | Pstr_type of (string * type_declaration) list
                                      (* type typedef {and typedef} 
                                       * string = typeconstr-name 
                                       * typdef ::= [type-params] typeconstr-name [type-information] *)
  | Pstr_exception of string * exception_declaration
                                      (* exception constr-name [of typexpr {* typexpr} *)
  | Pstr_open of long_ident
