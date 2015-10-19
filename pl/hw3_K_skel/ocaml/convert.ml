(* Abstract OCAML AST *)
let sum l = List.fold_left (fun s (_, n) -> s + n) 0 l

let rec change_long_ident i =
  match i with
  | Parsetree.Lident s -> s
  | Parsetree.Ldot (i, s) -> (change_long_ident i) ^ "." ^ s

let change_const c =
  match c with
  | Parsetree.Const_int n -> Abstract.IntExp n
  | Parsetree.Const_char c -> Abstract.CharExp c
  | Parsetree.Const_string s -> Abstract.StringExp s
  | Parsetree.Const_float _ -> Abstract.FloatExp

let rec change_exp e =
  match e.Parsetree.pexp_desc with
  | Parsetree.Pexp_ident _ -> (Abstract.VarExp, 0)
  | Parsetree.Pexp_constant c -> (change_const c, 0)
  | Parsetree.Pexp_let (_, l, e) -> 
    let l' = List.map (fun (_, e) -> change_exp e) l in
    let e' = change_exp e in
    (Abstract.LetExp (l', e'), (sum l') + (Abstract.sizeof e') + 1)
  | Parsetree.Pexp_function l -> 
    let l' = List.map (fun (_, e) -> change_exp e) l in
    (Abstract.FnExp l', (sum l') + 1)
  | Parsetree.Pexp_apply (e, l) ->
    let e' = change_exp e in
    let l' = List.map change_exp l in
    (Abstract.AppExp (e', l'), (Abstract.sizeof e') + (sum l') + 1)
  | Parsetree.Pexp_match (e, l) ->
    let e' = change_exp e in
    let l' = List.map (fun (_, e) -> change_exp e) l in
    (Abstract.MatchExp (e', l'), (Abstract.sizeof e') + (sum l') + 1)
  | Parsetree.Pexp_try (e, l) ->
    let e' = change_exp e in
    let l' = List.map (fun (_, e) -> change_exp e) l in
    (Abstract.TryExp (e', l'), (Abstract.sizeof e') + (sum l') + 1)
  | Parsetree.Pexp_tuple l ->
    let l' = List.map change_exp l in
    (Abstract.TupleExp l', (sum l') + 1)
  | Parsetree.Pexp_construct (c, None) -> (Abstract.ConExp (change_long_ident c), 0)
  | Parsetree.Pexp_construct (c, Some e) -> 
    let e' = change_exp e in
    (Abstract.AppExp ((Abstract.ConExp (change_long_ident c), 0), [e']), (Abstract.sizeof e') + 1)
  | Parsetree.Pexp_ifthenelse (e, e1, None) ->
    let e' = change_exp e in
    let e1' = change_exp e1 in
    (Abstract.IfExp (e', e1', None), (Abstract.sizeof e') + (Abstract.sizeof e1') + 1)
  | Parsetree.Pexp_ifthenelse (e, e1, Some e2) ->
    let e' = change_exp e in
    let e1' = change_exp e1 in
    let e2' = change_exp e2 in
    (Abstract.IfExp (e', e1', Some e2'), (Abstract.sizeof e') + (Abstract.sizeof e1') + (Abstract.sizeof e2') + 1)
  | Parsetree.Pexp_sequence (e1, e2) ->
    let e1' = change_exp e1 in
    let e2' = change_exp e2 in
    (Abstract.SeqExp (e1', e2'), (Abstract.sizeof e1') + (Abstract.sizeof e2') + 1)
  | Parsetree.Pexp_constraint (e, _) ->
    let e' = change_exp e in
    (Abstract.ConstraintExp e', (Abstract.sizeof e') + 1)

let count = ref 0
let getFreshExps () = let n = !count in count := !count + 1; (Abstract.StringExp ("this is unique string" ^ (string_of_int n)), 0)

let rec check_impl_exp e =
  match e.Parsetree.pexp_desc with
  | Parsetree.Pexp_function l -> (match l with [_, e'] -> check_impl_exp e' | _ -> true)
  | Parsetree.Pexp_apply (e', l) -> (match e'.Parsetree.pexp_desc with Parsetree.Pexp_ident (Parsetree.Lident "raise") -> false | _ -> true)
  | _ -> true

let change_item name i =
  match name with
  | None -> 
    (match i.Parsetree.pstr_desc with
     | Parsetree.Pstr_eval e -> 
       let e' = if check_impl_exp e then change_exp e else getFreshExps () in
       Some (Abstract.ValDec [e'], (Abstract.sizeof e') + 1)
     | Parsetree.Pstr_value (_, l) ->
       let l' = List.map (fun (_, e) -> if check_impl_exp e then change_exp e else getFreshExps ()) l in
       if l' = [] then None else Some (Abstract.ValDec l', (sum l') + 1)
     | _ -> None)
  | Some s ->
    (match i.Parsetree.pstr_desc with
     | Parsetree.Pstr_eval e -> 
       let e' = if check_impl_exp e then change_exp e else getFreshExps () in
       Some (Abstract.ValDec [e'], (Abstract.sizeof e') + 1)
     | Parsetree.Pstr_value (_, l) ->
       let l = List.filter (fun (p, e) -> match p.Parsetree.ppat_desc with Parsetree.Ppat_var s' -> List.exists (fun x -> x = s') s | _ -> false) l in
       let l' = List.map (fun (_, e) -> if check_impl_exp e then change_exp e else getFreshExps ()) l in
       if l' = [] then None else Some (Abstract.ValDec l', (sum l') + 1)
     | _ -> None)
 
let change name p =
  let p' = List.map (change_item name) p in 
  let p'' = List.fold_left (fun l r -> match r with Some e -> l @ [e] | None -> l) [] p' in
  (Abstract.SeqDec p'', (sum p''))
