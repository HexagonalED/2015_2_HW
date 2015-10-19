(* Abstracted Syntax Tree for clonechecking *)

type exp =
  (* Expression *)
    VarExp
  | IntExp of int
  | CharExp of char
  | StringExp of string
  | FloatExp
  | LetExp of exps list * exps
  | FnExp of exps list
  | AppExp of exps * exps list
  | MatchExp of exps * exps list
  | TryExp of exps * exps list
  | TupleExp of exps list
  | ConExp of string
  | IfExp of exps * exps * exps option
  | SeqExp of exps * exps
  | ConstraintExp of exps
  (* Declaration *)
  | ValDec of exps list
  | SeqDec of exps list
and exps = exp * int            (* expression, size *) 

(* Auxilary function *)
let sizeof ((_, n) : exps) = n
let expof ((e, _) : exps) = e
