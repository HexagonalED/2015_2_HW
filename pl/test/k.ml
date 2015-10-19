(*
 * SNU 4190.310 Programming Languages 2015 Fall
 *  K- Interpreter Skeleton Code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list      (* call by value *)
  | CALLR of id * id list       (* call by referenece *)
  | RECORD of (id * exp) list   (* record construction *)
  | FIELD of exp * id           (* access record field *)
  | ASSIGN of id * exp          (* assgin to variable *)
  | ASSIGNF of exp * id * exp   (* assign to record field *)
  | READ of id
  | WRITE of exp
  
  type program = exp

  type value =
  | Num of int
  | Bool of bool
  | Unit
  | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =(*get int value*)
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v = (* get boolvalue *)
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v = (*get unit value*)
      match v with
      | Unit -> ()
      | _ -> raise (Error "TypeError : not unit")

  let value_record v = (*get record value *)
      match v with
      | Record r -> r
      | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x = (* look for the val in address => Loc.t(Location of int) type*)
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f = (* get id,exp,env*)
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id, exp, env) -> (id, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)

(* start custom *)

    | ADD (e1,e2) -> 
        let (val1, mem1) = (eval mem env e1) in 
        let (val2, mem2) = (eval mem1 env e2) in
        ((Num ((value_int val1)+(value_int val2))), mem2)
    | SUB (e1,e2) ->
        let (val1, mem1) = (eval mem env e1) in 
        let (val2, mem2) = (eval mem1 env e2) in
        ((Num ((value_int val1)-(value_int val2))), mem2)
    | MUL (e1,e2) ->
        let (val1, mem1) = (eval mem env e1) in 
        let (val2, mem2) = (eval mem1 env e2) in
        ((Num ((value_int val1)*(value_int val2))), mem2)
    | DIV (e1,e2) ->
        let (val1, mem1) = (eval mem env e1) in 
        let (val2, mem2) = (eval mem1 env e2) in
        ((Num ((value_int val1)/(value_int val2))), mem2)
    | EQUAL (e1,e2) ->
        let (val1, mem1) = (eval mem env e1) in 
        let (val2, mem2) = (eval mem1 env e2) in
        (match val1 with
        | Num(n1) -> (match val2 with
                     | Num(n2) -> if n1 = n2 then (Bool(true),mem2) else (Bool(false),mem2)
                     | _ -> (Bool(false),mem2))
        | Bool(b1) -> (match val2 with
                       | Bool(b2) -> if b1 = b2 then (Bool(true),mem2) else (Bool(false),mem2)
                       | _ -> (Bool(false),mem2))
        | Unit -> (match val2 with
                   | Unit -> (Bool(true),mem2)
                   | _ -> (Bool(false),mem2))
        | _ -> (Bool(false),mem2))
    | LESS (e1,e2) ->
        let (val1, mem1) = (eval mem env e1) in 
        let (val2, mem2) = (eval mem1 env e2) in
        (if ((value_int val1) < (value_int val2)) then (Bool(true),mem2) else (Bool(false),mem2))
    | NOT e ->
        let (newval, newmem) = (eval mem env e) in 
        (Bool((not (value_bool newval))),newmem)
    | SEQ (e1,e2) ->
        let (val1, mem1) = (eval mem env e1) in 
        (eval mem1 env e2)
    | IF (e1,e2,e3) ->
        let (valif, memif) = (eval mem env e1) in
        (if (value_bool valif)
        then (eval memif env e2)
        else (eval memif env e3))
    | WHILE (e1,e2) ->
        let (valwhile, memwhile) = (eval mem env e1) in
        if (value_bool valwhile)
        then 
          let (valdo, memdo) = (eval memwhile env e2) in
          (eval memdo env (WHILE(e1,e2)))
        else (Unit,memwhile)
    | LETF (x,xl,e1,e2) ->
        let newenv = (Env.bind env x (Proc (xl,e1,env))) in
        (eval mem newenv e2)
    | CALLV (x,el) ->
        let (idlist,newexp,newenv) =(lookup_env_proc env x) in
        let rec expleval m explist = 
          match explist with
          | [] -> []
          | hd::tl -> let (nv,nm) = (eval m env hd) in
                      ((nv,nm)::(expleval nm tl)) in
        let rec matching env vml idl = 
          match vml with
          | [] -> (match idl with
                   | [] -> (env,emptyMemory)
                   | _ -> raise (Error "Wrong Input"))
          | (v,m)::tl -> (match idl with
                          | [] -> raise (Error "Wrong Input")
                          | hd::tail -> (let (envtl,m') = (matching env tl tail) in
                                               if m' = emptyMemory 
                                               then 
                                                 (let (l,m'') = (Mem.alloc m) in
                                                 ((Env.bind envtl hd (Addr(l))),(Mem.store m'' l v)))
                                               else
                                                 (let (l,m'') = (Mem.alloc m') in 
                                                 ((Env.bind envtl hd (Addr(l))),(Mem.store m'' l v))))) in
        let (em,mm) = (matching newenv (expleval mem el) idlist) in
        (eval mm (Env.bind em x (Proc(idlist,newexp,newenv))) newexp)
    | CALLR (x,xl) -> let (idlist, newexp,newenv) = (lookup_env_proc env x) in
                      let rec matchenv idl1 lookupidl basicenv=
                        match idl1 with
                        | [] -> if(lookupidl =[]) then basicenv else raise (Error "Wrong Input")
                        | hd::tl -> (match lookupidl with
                                    | [] -> raise (Error "Wrong Input")
                                    | lhd::ltl -> (Env.bind (matchenv tl ltl basicenv) hd (Addr(lookup_env_loc env lhd)))) in
                      (eval mem (Env.bind (matchenv idlist xl newenv) x (Proc(idlist,newexp,newenv))) newexp)
    | RECORD xel -> if xel = [] then (Unit,mem) 
                    else
                      (let rec reclist m e iexl = 
                          match iexl with
                          | [] -> []
                          | (hdid, hdexp)::tl -> let (nv,nm) = (eval m e hdexp) in
                                                 ((nv,nm)::(reclist nm e tl)) in
                      let rec matching recl iexl m_in =
                          match recl with
                          | [] -> (match iexl with
                                       | [] -> (Unit,m_in)
                                       | _ -> raise (Error "Wrong Input"))
                          | (v,m)::tl -> (match iexl with
                                          | [] -> raise (Error "Wrong Input")
                                          | (hdid, hdloc)::iextl -> let (nv,nm) = (matching tl iextl m) in
                                                                    if nv = Unit 
                                                                    then
                                                                      (let (l,nm') = (Mem.alloc nm) in
                                                                      (Record(fun x -> if x = hdid then l else (raise (Error "Wrong Record"))),(Mem.store nm' l v)))
                                                                    else
                                                                      (let (l,nm') = (Mem.alloc nm) in 
                                                                      (Record(fun x -> if x= hdid then l else ((value_record nv) x)), (Mem.store nm' l v))))
                      in
                      (matching (reclist mem env xel) xel))
    | FIELD (e,x) -> 
        let (v,m) = (eval mem env e) in
        (Mem.load m ((value_record v) x), m)
    | ASSIGNF (e1,x,e2) ->
        let (v,m) = (eval mem env e1) in
        let (v',m') = (eval m env e2) in
        (v', Mem.store m' ((value_record v) x) v')
    | _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
