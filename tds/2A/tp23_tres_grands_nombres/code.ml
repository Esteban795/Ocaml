let m = (1 lsl 31) - 1;;

let u_n n = 
  let u0 = 42 in 
  let u = ref u0 in 
  for i = 0 to n - 1 do
    u := (16807 * !u + 17) mod m
  done;
  !u


let v_k_n k n = 
  (u_n n mod (1 lsl k)) + 1 lsl k

let q2 k = 
  (v_k_n k ((97 * k) mod 997)) mod 101

type ternary_tree = 
  | Leaf of int
  | Node of int* int * ternary_tree * ternary_tree * ternary_tree


let x_p p = 
  1 lsl (1 lsl p)

let find_highest_p_value n = 
  let p = ref 0 in 
  while !p < 6 && x_p !p < n do 
    incr p;
  done;
  !p

let rec build_ternary_tree value = 
  let p = find_highest_p_value value in 
  let xp = x_p p in 
  let d = value / xp in 
  let g = value mod xp in
  if g > xp then Node (value,p ,build_ternary_tree g, build_ternary_tree p, build_ternary_tree d)
  else Leaf (value)

let rec signature tt = 
  match tt with 
  | Leaf (x) -> 
      if x = 0 then 0 
      else u_n 10 mod 97
  |Node (v,p,g,c,d) ->
    if p mod 2 = 1 then 
      (signature g + u_n 20 * signature d) mod 97
    else
      (signature g + u_n 30 + signature d) mod 97

let _ = 
  (*
  Printf.printf "u_n_mod_101 5 renvoie %d\n" (u_n 5 mod 101);
  Printf.printf "u_n_mod_101 100 renvoie %d\n" (u_n 100 mod 101); 
  Printf.printf "u_n_mod_101 997 renvoie %d\n" (u_n 997 mod 101);

  Printf.printf "q2 avec 5 renvoie %d\n" (q2 5);
  Printf.printf "q2 avec 30 renvoie %d\n" (q2 30);
  Printf.printf "q2 avec 61 renvoie %d\n" (q2 61);
  *)

  
  let t1 = build_ternary_tree (v_k_n 1 10) in 
  
  let t2 = build_ternary_tree (v_k_n 2 20) in 
  let t3 = build_ternary_tree (v_k_n 32 20) in
  let t4 = build_ternary_tree (v_k_n 61 40) in
  Printf.printf "signature avec (1,10) renvoie %d\n" (signature t1);
  Printf.printf "signature avec (2,20) renvoie %d\n" (signature t2);
  Printf.printf "signature avec (32,20) renvoie %d\n" (signature t3);
  Printf.printf "signature avec (61,40) renvoie %d\n" (signature t4);