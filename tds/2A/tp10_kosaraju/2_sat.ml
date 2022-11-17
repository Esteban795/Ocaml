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
  let n = max_var n in 
  let g = Array.make (2*(n + 1) ) [] in (*un sommet pour xi et -xi, et on en a n+1 car on va de 0 à n*)
  in
  let not_l l = 
    match l with 
    |N i -> P i 
    |P i -> N i 
  in 
  let position cl = 
    match cl with 
    |P i -> 2 * i
    |N i -> 2 * i + 1 
  let ajouter_clause c = 
    let a,b = c in 
    g.(position (not a)) <- (position b) :: g.(position (not a));
    g.(position (not b)) <- (position a) :: g.(position (not b))
  in 
  list.iter add_clause f;
  g
    
