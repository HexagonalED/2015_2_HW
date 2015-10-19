let sfold f l = List.fold_right (fun x n -> (f x) @ n) l []

(* explode : exps -> exps list *) 
let rec explode (p:Abstract.exps) =
  match p with
  | (_, 0) -> []
  | (_, 1) -> [p]
  | (Abstract.LetExp (l, e'), _) -> p :: sfold explode (l @ [e'])
  | (Abstract.FnExp l, _) -> p :: sfold explode l
  | (Abstract.AppExp (e, l), _) -> p :: sfold explode (e :: l)
  | (Abstract.MatchExp (e, l), _) -> p :: sfold explode (e :: l)
  | (Abstract.TryExp (e, l), _) -> p :: sfold explode (e :: l)
  | (Abstract.TupleExp l, _) -> p :: sfold explode l
  | (Abstract.IfExp (e, e1, None), _) -> p :: sfold explode [e; e1]
  | (Abstract.IfExp (e, e1, Some e2), _) -> p :: sfold explode [e; e1; e2]
  | (Abstract.SeqExp (e1, e2), _) -> p :: sfold explode [e1; e2]
  | (Abstract.ConstraintExp e, _) -> p :: explode e
  | (Abstract.ValDec l, _) -> p :: sfold explode l
  (* p is not added, nullifying the effect of changing the order of declarations  *)
  | (Abstract.SeqDec l, _) -> sfold explode l
  | _ -> []

(* compares : exps list -> exps list -> int *)
let compares (l:Abstract.exps list) (l':Abstract.exps list) =
  let tbl1 = Hashtbl.create (List.length l') in
  (* eqn x: find all exps in l' that are identical to x *)
  let eqn x = List.exists ((=) x) (Hashtbl.find_all tbl1 (Hashtbl.hash x)) in
  let addit n x = if Hashtbl.mem tbl1 n && eqn x then () else Hashtbl.add tbl1 n x in
  (* 
    Todo. why do we need eqn2 which appears to be the same as eqn? 
   *)
  let eqn2 x = List.exists (fun y -> x = y) (Hashtbl.find_all tbl1 (Hashtbl.hash x)) in
  List.iter (fun x -> addit (Hashtbl.hash x) x) l';
  List.fold_right (fun x n -> if eqn2 x then n + 1 else n) l 0

(* compare : exps -> exps -> float *)
let compare (p:Abstract.exps) (p':Abstract.exps) =
  let l = List.rev (explode p) in
  let l' = List.rev (explode p') in
  let eqnode1 = compares l l' in
  let eqnode2 = compares l' l in
  if (Abstract.sizeof p) + (Abstract.sizeof p') = 0
    then 0.0 
    else (float_of_int (eqnode1 + eqnode2)) /. (float_of_int ((Abstract.sizeof p) + (Abstract.sizeof p')))

