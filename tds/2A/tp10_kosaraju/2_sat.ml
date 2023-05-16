open Kosaraju

type litteral = 
  | P of int
  | N of int

type clause = litteral * litteral
type twocnf = clause list
type valuation = bool array
type vertex = int

type graph = vertex list array


let eval_litt litt v = 
  match litt with 
  | P i -> v.(i)
  | N i -> not v.(i)


let rec eval f v = 
  match f with 
  | [] -> true
  | (l,l') :: t -> 
    (eval_litt l v || eval_litt l' v) && eval t v


exception Last
let increment_valuation v = 
  let n = Array.length v in 
  let rec up i = 
    match v.(i) with 
    | false -> v.(i) <- true
    | true -> 
      if i < 0 then raise Last 
      else 
        begin 
          v.(i) <- false;
          up (i - 1)
        end
  in
  up (n - 1) 


let absolute litt = 
  match litt with 
  | N i | P i -> i

let rec max_var f = 
  match f with 
  | [] -> -1
  | (a,b) :: reste -> max (max_var reste) (max (absolute a) (absolute b))


let brute_force f = 
  let maxi_var = max_var f in 
  let v = Array.make maxi_var false in 
  try 
    while not (eval f v) do 
      increment_valuation v
    done;
    Some v
  with 
  | Last -> None

let graph_of_cnf f = 
  let n = max_var f in 
  let g = Array.make (2 * (n + 1)) [] in 
  let not_l l = 
    match l with 
    | N i -> P i 
    | P i  -> N i
  in 
  let position c1 = 
    match c1 with 
    | P i -> 2 * i
    | N i -> 2 * i + 1
  in 
  let ajouter_clause c = 
    let a,b = c in 
    g.(position (not_l a)) <- (position b) :: g.(position (not_l a));
    g.(position (not_l b)) <- (position a) :: g.(position (not_l b))
  in 
  List.iter ajouter_clause f;
  g

exception Unsatisfiable
let satisfiable f = 
  let g = graph_of_cnf f in 
  let n = Array.length g in 
  let belongs_to_cfc = Array.make n (-1) in (* le numéro la cfc de chaque xi et non xi *)
  let cfcs = kosaraju g in
  let assign_cfc_number x cfc_n  = belongs_to_cfc.(x) <- cfc_n in
  let assign_for_all_of_cfc cfc i = 
    List.iter (assign_cfc_number i) cfc  (*on assigne le même numéro à tous les membres de la cfc, qui sont stockés sous forme de liste*)
  in 
  List.iteri (fun i cfc -> assign_for_all_of_cfc cfc i) cfcs;
  try 
    for x = 0 to n/2 - 1 do 
      if belongs_to_cfc.(x) = belongs_to_cfc.(2 * x + 1) then raise Unsatisfiable
    done;
    true;
  with 
  |Unsatisfiable -> false
  
type trileen = 
  | U 
  | F 
  | T 
  
exception Insat
let witness f =
  let g = graph_of_cnf f in
  let n = Array.length g in
  let components = Array.of_list (kosaraju g) in
  let nb_components = Array.length components in
  let scc_of_litt = Array.make n 0 in
  for i = 0 to nb_components - 1 do
    List.iter (fun l -> scc_of_litt.(l) <- i) components.(i)
  done;
  let valuation = Array.make nb_components U in
  let neg x =
    if x mod 2 = 1 then x - 1 else x + 1 in
  let bar i =
    match components.(i) with
    | x :: _ -> scc_of_litt.(neg x)
    | [] -> failwith "empty scc: that's illegal!" in
  try
    for i = 0 to nb_components - 1 do
      match valuation.(i) with
        | U ->
            if bar i = i then raise Insat;
            valuation.(i) <- F;
            valuation.(bar i) <- T;
        | F | T -> ()
    done;
    let v = Array.make (n / 2) false in
    for i = 0 to n / 2 - 1 do
      match valuation.(scc_of_litt.(2 * i)) with
      | U -> failwith "something went very wrong :("
      | T -> v.(i) <- true
      | F -> v.(i) <- false
    done;
    Some v
  with
  | Insat -> None