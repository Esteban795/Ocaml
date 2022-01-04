type combination = int array
let nb_colors = 6
let nb_pegs = 4

let well_placed x y =   
  let compteur = ref 0 in 
  let n = Array.length x in
  for i = 0 to n - 1 do
    if x.(i) = y.(i) then incr compteur
  done;
  !compteur


let histogram combo = 
  let h = Array.make nb_colors 0 in
  for i = 0 to nb_pegs - 1 do
      h.(combo.(i)) <- h.(combo.(i)) + 1;
  done;
  h

let well_placed_or_not x y =
  let hx = histogram x in
  let hy = histogram y in
  let compteur = ref 0 in
  for i = 0 to nb_colors - 1 do
      compteur := !compteur + min hx.(i) hy.(i)
  done;
  !compteur



let compute_similarity u v =
  let t1 = well_placed u v in
  let t2 = well_placed_or_not u v in
  (t1, t2 - t1)


let rec pow x y = 
  if y = 0 then 1
  else if y mod 2 = 0 then pow (x * x) (y/2)
  else x * (pow (x * x) (y / 2))


let nb_combinations = pow nb_colors nb_pegs



let combination_of_int n = 
  let combination = Array.make nb_pegs 0 in
  let i = ref 0 in
  let temp = ref n in
  while !temp > 0 do
    combination.(!i) <- !temp mod nb_colors;
    temp := !temp / nb_colors;
    incr i
  done;
  combination

let int_of_combination combo = 
  let s = ref 0 in
  let l = Array.length combo in
  for i = 0 to l - 1 do
    s := !s + combo.(i) * (pow nb_colors i)
  done;
  !s

let create_similarity_table () = 
  let matrix = Array.make_matrix nb_combinations nb_combinations (0,0) in
  for i = 0 to nb_combinations - 2 do
      let ti = combination_of_int i in
      for j = 0 to nb_combinations - 2 do
          let tj = combination_of_int j in
          matrix.(i).(j) <- compute_similarity ti tj
      done;
  done;
  matrix


let similarity i j = similarity_table.(i).(j)


let rec is_compatible history candidate_move = 
  match history with
  | [] -> true
  | (x,y) :: tail -> if similarity x candidate_move = y then is_compatible tail candidate_move else false


let play_simple goal =
  let rec aux i lst = 
    if is_compatible lst i then
        if similarity i goal = (nb_pegs,0) then List.rev ((goal,(nb_pegs,0)) :: lst) 
        else aux (i + 1) (((i),similarity i goal) :: lst)
    else aux (i + 1) lst 
  in aux 0 []


let stats f = 
  let nb_moves = ref 0 in
  let maxi = ref 0 in
  let worst_case = ref 0 in
  for i = 0 to nb_combinations - 2 do
      let temp = List.length (f i) in
      nb_moves := !nb_moves + temp;
      if temp > !maxi then maxi := temp; worst_case := i;  
  done;
  ((float_of_int(!nb_moves))/.(float_of_int (nb_combinations - 1)) ,!maxi,!worst_case)