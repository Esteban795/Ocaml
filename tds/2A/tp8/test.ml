
let g_test = 
  let g = Array.make 6 [1] in 
  g;;

exception FoundObjective of (int * int)
let closest_objective g s k obj = 
  raise (FoundObjective (1,6));;


let nb_moves n p k = 
  (*let g = explicit_graph_v2 n p k in*)
  let g = g_test in
  let objectives_completed = Array.make n false in 
  let total_moves = ref 0 in
  let temp_s = ref 0 in 
  try 
    closest_objective g !temp_s 2 objectives_completed;
  with
  |FoundObjective(s,d) -> 
    begin 
      temp_s := s;
      total_moves := !total_moves + d;
      Printf.printf "On est au sommet %d, avec %d moves" !temp_s !total_moves;
    end;;

  print_int 3;;