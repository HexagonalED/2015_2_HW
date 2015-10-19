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

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
      match v with
      | Unit -> ()
      | _ -> raise (Error "TypeError : not unit")

  let value_record v =
      match v with
      | Record r -> r
      | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
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
    | NUM i -> (Num i, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR id -> ((Mem.load mem (lookup_env_loc env id)), mem)
    | ADD (e1, e2) -> 
      let (n1, m')=eval mem env e1 in
      let (n2, m'')=eval m' env e2 in
      ((Num((value_int n1)+(value_int n2))), m'')
    | SUB (e1, e2) -> 
      let (n1, m')=eval mem env e1 in
      let (n2, m'')=eval m' env e2 in
      ((Num((value_int n1)-(value_int n2))), m'')
    | MUL (e1, e2) -> 
      let (n1, m')=eval mem env e1 in
      let (n2, m'')=eval m' env e2 in
      ((Num((value_int n1)*(value_int n2))), m'')
    | DIV (e1, e2) -> 
      let (n1, m')=eval mem env e1 in
      let (n2, m'')=eval m' env e2 in
      ((Num((value_int n1)/(value_int n2))), m'')
    | EQUAL (e1, e2) ->
      let (v1, m')=eval mem env e1 in
      let (v2, m'')=eval m' env e2 in
      (match (v1, v2) with
      |(Num x, Num y)->if x=y then (Bool true, m'') else (Bool false, m'')
      |(Bool x, Bool y)->if x=y then (Bool true, m'') else (Bool false, m'')
      |(Unit, Unit)-> (Bool true, m'')
      |_->(Bool false, m'')
      )
    | LESS (e1, e2)->
      let (n1, m')=eval mem env e1 in
      let (n2, m'')=eval m' env e2 in
      (Bool ((value_int n1)<(value_int n2)),m'')
    | NOT e->
      let (b, m')=eval mem env e in
      if (value_bool b)=true then (Bool false, m') else (Bool true, m')
    | SEQ (e1, e2)->
      let (v1, m')=eval mem env e1 in
      (eval m' env e2)
    | IF (e, e1, e2)-> 
      let (b, m')=eval mem env e in
      if (value_bool b)=true then (eval m' env e1) else (eval m' env e2)
    | WHILE (e1, e2) ->
      let (b, m')=eval mem env e1 in
      if (value_bool b)=false then (Unit, m') else let (v1, m1)=eval m' env e2 in
      (eval m1 env (WHILE(e1, e2)))
    | LETF (id, idlist, e1, e2) -> (eval mem (Env.bind env id (Proc(idlist,e1,env))) e2)
    | CALLV (id, elist) -> let rec getPair mem1 li=
                           (match li with
                            |[ex]->[(eval mem1 env ex)]
                            |ex::lis->let (v, m)=eval mem1 env ex in
                                      ((v, m)::(getPair m lis))
                           ) in
                           let rec setPair env li idlist=
                           (match li with
                            |[(v,m)]->(match idlist with
                                       |[id1]->let (l, m2)=Mem.alloc m in
                                       ((Env.bind env id1 (Addr(l))),(Mem.store m2 l v))
                                       |_->raise (Error "InvalidArg"))
                            |(v,m)::lis->(match idlist with
                                          |[]->raise (Error "InvalidArg") 
                                          |[id1]->raise (Error "InvalidArg")
                                          |id1::lit->let (e,m)=setPair env lis lit in
                                                     let (l, m2)=Mem.alloc m in
                                                     ((Env.bind e id1 (Addr(l))),(Mem.store m2 l v))))
                           in 
                           let (idlist, e1, env1)=lookup_env_proc env id in
                           let (e,m)=setPair env1 (getPair mem elist) idlist in
                           (eval m (Env.bind e id (Proc(idlist, e1, env1))) e1)
    | CALLR (id, idlist) -> let rec setEnv env1 xli yli=
                            (match xli with
                             |[x]->(match yli with
                                    |[y]-> (Env.bind env1 x (Addr(lookup_env_loc env y)))
                                    |_->raise (Error "InvalidArg"))
                             |x::xx->(match yli with
                                    |[]-> raise (Error "InvalidArg")
                                    |[y]-> raise (Error "InvalidArg")
                                    |y::yy->(Env.bind (setEnv env1 xx yy) x (Addr(lookup_env_loc env y)))))
    in let (xidl, e1, env1)=lookup_env_proc env id in
    (eval mem (Env.bind (setEnv env1 xidl idlist) id (Proc(xidl, e1, env1))) e1)
    | RECORD li->(match li with
                  |[]->(Unit, mem)
                  |_->let rec getRecord mem env lis=
                      (match lis with
                      |[(id, ex)]->[(eval mem env ex)]
                      |(id, ex)::li1->let (v1, m1)=eval mem env ex in
                                      ((v1, m1)::(getRecord m1 env li1))) in
                      let rec setRecord lis edli=
                      (match lis with
                      |[(v,m)]->let (l, m2)=Mem.alloc m in
                                (match edli with
                                 |[(id, _)]->(Record(fun x->if x=id then l else raise (Error "No Record")), (Mem.store m2 l v)))
                      |(v,m)::li1->(match edli with
                                    |(id, _)::li2->let (v1, m1)=setRecord li1 li2 in
                                                   let (l, m2)=Mem.alloc m1 in
                                                   (Record(fun x->if x=id then l else ((value_record v1) x)), Mem.store m2 l v))) in
                      (setRecord (getRecord mem env li) li))
    | FIELD (e1, id)-> let (v1, m1)=eval mem env e1 in
                       ((Mem.load m1 ((value_record v1) id)), m1)
    | ASSIGNF (e1, id, e2)-> let (v1, m1)=eval mem env e1 in
                             let (v2, m2)=eval m1 env e2 in
                             (v2, (Mem.store m2 ((value_record v1) id) v2))

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
end
