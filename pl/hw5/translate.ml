(
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.SEQ(e1,e2) -> (trans e1)@[Sm5.POP]@(trans e2)
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR(eV) -> [Sm5.PUSH (Sm5.Id eV);Sm5.LOAD]
    | K.SUB(e1,e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL(e1,e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV(e1,e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL(e1,e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS(e1,e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT(eE) -> trans eE @ [Sm5.NOT]
    | K.ASSIGN(x,e) ->(*?*) 
        trans e @ [Sm5.PUSH (Sm5.Id x) ;Sm5.STORE;Sm5.PUSH(Sm5.Id x) ; Sm5.LOAD]
    | K.IF(cond,et,ef) -> 
        trans cond @ [Sm5.JTR ((trans et),(trans ef))]
    | K.LETF(f,x,e1,e2) -> (*?*)
        [Sm5.PUSH (Sm5.Fn(x,[Sm5.BIND f]@trans e1)) ; Sm5.BIND f] @ 
        trans e2 @ [Sm5.UNBIND;Sm5.POP]
    | K.CALLR(f,arg_var) -> (*?*)
        [Sm5.PUSH (Sm5.Id(f));Sm5.PUSH (Sm5.Id (f)); Sm5.PUSH(Sm5.Id (arg_var)) ; Sm5.LOAD ; Sm5.PUSH(Sm5.Id(arg_var));Sm5.CALL]
    | K.CALLV(f,arg_exp) -> (*?*)
        [Sm5.PUSH (Sm5.Id(f));Sm5.PUSH (Sm5.Id f)] @ trans arg_exp @ [Sm5.MALLOC ; Sm5.CALL]
    | K.WRITE(e) -> (*!*)
        trans e @ [Sm5.MALLOC ; Sm5.BIND("lise") ; Sm5.PUSH (Sm5.Id("lise"));Sm5.STORE;Sm5.PUSH(Sm5.Id("lise"));Sm5.LOAD;Sm5.PUSH(Sm5.Id("lise"));Sm5.LOAD;Sm5.PUT]
(*    | K.WHILE(cond,eBody) -> (*!*)
    | K.FOR(x,e1,e2,eBody) -> (*!*)*)
    | _ -> failwith "Unimplemented"
end
