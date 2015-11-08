type treasure = StarBox | NameBox of string;;
type key = Bar | Node of key * key;;

type map = 
  End of treasure
  | Branch of map * map
  | Guide of string * map;;


type keyVal = 
  Nil
  | Tr of int
  | Br of int
  | Pair of keyVal * keyVal;;

exception IMPOSSIBLE
exception wrongINPUT
exception ErrorCase



type Location = 
  LKloc of int * keyVal;;
  | TLloc of string * int;;


let rec getLKVal : Location list * int ->  keyVal = fun(li,loc) ->
  match li with
  | [] -> Nil
  | hd::tl -> 
      (match hd with
       | TLloc(_,_) -> raise ErrorCase
       | LKloc(keyloc,keyVal) -> if keyloc = loc
                            then keyVal
                            else (getLKVal (tl,loc)))
;;

let rec getTLloc : Location list * string -> int = fun (li,str) ->
  match li with
  | [] -> -1
  | hd::tl -> 
      (match hd with
       | LKloc(_,_) -> raise ErrorCase
       | TLloc(name,loc) -> if (String.compare name str)=0
                            then loc
                            else (getTLloc (tl,str)))
;;
       


let rec getReady : map -> key list  = fun m ->
  let rec dfs : (map * int) -> (Location list * Location list * int) = fun (m,dfsloc) ->
    (match m with
    | Branch(ml,mr) ->
        let (tll1,lkl1,ltloc) = (dfs (ml, dfsloc+1)) in 
        let (tll2,lkl2,rtloc) = (dfs (mr, ltloc+1)) in
        ((List.append tll1 tll2),(List.append (List.append lkl1 lkl2) Br(dfsloc)::[]),rtloc)
    | Guide(str,tmap) -> 
        let (tll,lkl,subloc) = (dfs (tmap, dfsloc+1)) in
        let loc = (getTLloc (tll,str)) in
        if loc = -1 then raise wrongINPUT 
        else
          let val = (getLKVal (lkl,loc)) in
          if val = Nil then raise wrongINPUT
          else
            (tll,(List.append lkl Pair(val,val)::[]),subloc)
    | End(tr) -> 
        (match tr with
         | StarBox -> (,LKloc(dfsloc,Tr(dfsloc))::[],dfsloc)
         | NameBox(name) -> (TLloc(name,dfsloc)::[],LKloc(dfsloc, Tr(dfsloc))::[],dfsloc)
        )

        )       
  in
  let (treasurelist, keylist, maxloc) = dfs(m,0) in

  Bar::[]
;;


