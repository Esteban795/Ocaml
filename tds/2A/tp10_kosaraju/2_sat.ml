open Kosaraju

type litteral = 
  |P of int 
  |N of int

type clause = litteral * litteral;;
type twocnf = clause list;;
type valuation = bool array;;

let rec eval_litt litt v = 
  match litt with 
  |N(n) -> not v.(n)
  |P(n) -> v.(n)


let rec eval f v = 
  match f with 
  |[] -> true
  |(a,b):: reste -> 
    (eval_litt a v || eval_litt b v) && eval reste v ;;


exception Last 
let increment_valuation v = 
  let rec loop i = 
    if i < 0 then raise Last (*on dépasse*)
    else if not v.(i) then v.(i) <- true (*on change de bit*)
    else 
    begin 
      v.(i) <- false;
      loop (i - 1)
    end
  in 
  loop (Array.length v - 1);;

let absolute litt = 
  match litt with 
  |N(i) | P(i) -> i

let rec max_var f = 
  match f with 
  |[] -> -1
  |(a,b) :: f' -> max (max_var f') (max (absolute a) (absolute b))

let brute_force f = 
  let maxi_var = max_var f in
  let v = Array.make maxi_var false in 
  try 
    while not (eval f v) do 
    increment_valuation v;
    done;
    Some v
  with
  |Last -> None


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
  let belongs_to_cfc = Array.make n (-1) in (*le numéro la cfc de chaque xi et non xi*)
  let cfcs = kosaraju g in
  let assign_cfc_number cfc_n x = belongs_to_cfc.(x) <- cfc_n;
  let assign_for_all_of_cfc cfc i = List.iter (assign i) cfc; (*on assigne le même numéro à tous les membres de la cfc, qui sont stockés sous forme de liste*)
  List.iter (fun cfc i -> assign_for_all_of_cfc cfc i) cfcs;
  try 
    for x = 0 to n/2 - 1 do 
      if belongs_to_cfc.(x) = belongs_to_cfc.(2 * x + 1) then raise Unsatisfiable
    done;
    true;
  with 
  |Unsatisfiable -> false