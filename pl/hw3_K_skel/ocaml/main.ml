(* Ocaml Clonechecker 
    parjong
    gla 
 *)

(* Argument Process *)
let inter_similarity = ref 0.0
let is_flag = ref false

let directory = ref ""
let d_flag = ref false

let verbose_flag = ref false

let fun_name = ref (None)

let single = ref ""         
let s_flag = ref false      (* when set, compares $single with all files in $directory *)

let optionlist =
  ["-is", Arg.Float (fun r -> inter_similarity := r; is_flag := true), 
    "   <num> is inter-similarity which must be a floating-point number between [0, 1]";
   "-d", Arg.String (fun s -> directory := s; d_flag := true), 
   "    checks clones among files in the <directory>";
   "-s", Arg.String (fun s -> single := s; s_flag := true), 
   "    compares the given file with each file in the <directory>";
   "-f", Arg.String (fun s -> fun_name := (match !fun_name with None -> (Some [s]) | Some sl -> (Some (s::sl)))),
   "    checks clones among function <name>";
   "-v", Arg.Set verbose_flag,
   "    prints output in the verbose mode"
   ]

(* Grouping *)
let rec is_group a l lt =
  match l with 
  | [] -> true
  | hd::tl -> (List.exists (fun (b,i,_) -> (a = b && hd = i) || ( a = i && hd = b )) lt) && is_group a tl lt

let rec select a l =
  match l with
  | [] -> ([],[])
  | hd::tl -> if List.exists (fun x -> x = a) hd then (List.filter (fun x -> x <> a) hd, tl) else 
              let (sh,sl) = select a tl in (sh,hd::sl)
              
let rec analysis l lt =
  match l with 
  | [] -> []
  | (a,b,_)::l -> 
    let ll = analysis l lt in
    let (ga,la) = select a ll in
    let (gb,lb) = select b la in
    if List.exists (fun x -> x = b) ga then ll else ([a;b] @ ga @ gb)::lb

let rec grouping (n,l) is =
  let compare_number = List.length l in
  let average = if compare_number = 0 then 0.0 else (List.fold_right (fun (_,_,n) m -> n +. m) l 0.0) /. (float_of_int compare_number) in
  let ll = List.filter (fun (_,_,n) -> n >= is) l in
  let result = analysis ll ll in
  if !verbose_flag then 
    (
    Printf.printf "\nTotal %d files are compared\n" n;
    Printf.printf "Average is %f\n" average;
    if result = [] then Printf.printf "No clones are detected\n" else 
    List.iter (fun l -> Printf.printf "("; List.iter (Printf.printf "%s ") l; Printf.printf ")\n\n") result
    )
  else
    (
    if result <> [] then 
      let _ = 
          match !fun_name with 
          | None -> () 
          | Some fl ->
            Printf.printf "--- ";
            List.iter (fun f -> Printf.printf "%s " f) fl; 
            Printf.printf "\n"
          in 
      List.iter (fun l -> Printf.printf "("; List.iter (Printf.printf "%s ") l; Printf.printf ")\n") result
    )
 
let sort_print l =
  let fl = (List.filter (fun (n, s) -> s > !inter_similarity) l) in
  let sl = List.sort (fun (n, s) (n', s') -> if s < s' then 1 else if s > s' then -1 else 0) fl in 
  List.iter (fun (n, s) -> Printf.printf "%s : %f\n" n s) sl 

(* Get file names *)
let back_flag = ref false
let pre_char = ref '\000'

let get_char () = if !back_flag then (back_flag := false; !pre_char) else input_char stdin
let get_back c = (back_flag := true; pre_char := c)

let rec remove_blank () =
  let c = get_char () in
  if List.exists (fun x -> x = c) [' '; '\012'; '\009'; '\010'; '\013'] then remove_blank () else get_back c

let rec get_token () =
  let c = get_char () in
  if List.exists (fun x -> x = c) [' '; '\012'; '\009'; '\010'; '\013'] then "" else (String.make 1 c) ^ (get_token ())

let read_token () = (remove_blank (); get_token ())

let rec get_files () = 
  try
    let s = read_token () in s :: (get_files ())
  with End_of_file -> []

let get_files_from_dir dir =
  let d = Unix.opendir dir in
  let rec f () = (try 
                    let 
                      s = if String.get dir 0 = '.' then Unix.readdir d else
                          if String.get dir (String.length dir - 1) = '/' then dir ^ Unix.readdir d else
                          dir ^ "/" ^ Unix.readdir d 
                    in
                      if (Unix.stat s).Unix.st_kind = Unix.S_REG then s :: f () else f ()
                  with End_of_file -> []) in
  f ()
                         
(* Auxilary function *)
let parse_file filename =
  let ic = open_in_bin filename in
  let ast = Parser.implementation Lexer.token (Lexing.from_channel ic) in
  close_in ic; ast

let rec parsel l =
  match l with
  | [] -> []
  | hd::tl -> let hd' =  
    try [(hd, parse_file hd)] with 
      Syntaxerr.Error err -> if !verbose_flag then Printf.printf "%s: %s\n" hd err; [] 
    | _ -> if !verbose_flag then Printf.printf "%s: parse fail\n" hd; [] 
  in hd' @ (parsel tl)

let rec trfold f l =
  match l with
  | [] -> []
  | h::l -> List.fold_right (fun x l -> f h x::l) l (trfold f l)

let compares astl =
  let n = List.length astl in
  let check (an, a) (bn, b) = (an, bn, Compare.compare a b) in
  (n, trfold check astl)

let abstract name l = List.map (fun (n, e) -> (n, Convert.change name e)) l

(* Main *)
let main () =
  if not !is_flag then Printf.printf "The -is flag should be specified\n" else
  if !inter_similarity < 0.0 || !inter_similarity > 1.0 then 
    Printf.printf "The inter similarity must be between 0 and 1\n" 
  else
    let l = if !d_flag then get_files_from_dir !directory else get_files () in
    if !s_flag then 
      let ((single_n, single_ast) :: l_ast) = abstract !fun_name (parsel (!single :: l)) in 
      let l_score = List.map (fun (name, ast) -> (name, Compare.compare single_ast ast)) l_ast in 
      sort_print l_score 
    else 
      grouping (compares (abstract !fun_name (parsel l))) !inter_similarity 

let usage = "Usage: clonechecker -is <num> -d <directory> -f <name>\nOptions are:"
let _ = (Arg.parse optionlist (fun _ -> ()) usage; main ());;
