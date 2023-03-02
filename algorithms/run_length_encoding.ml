(* run-length encoding*)
let rec compress lst =
  match lst with
      | [] -> []
      | h :: t -> match compress t with
              | [] -> [(h,1)]
              | (y,n) :: tail when h = y -> (y,n+1) :: tail
              | lst -> (h,1) :: lst

(* run-length decoding*)
let rec decompress lst = 
  let rec add_n_elt n o res = 
      if n = 0 then res
      else add_n_elt (n-1) o (o :: res) in
  match lst with
      | [] -> []
      | (elt,k) :: tail -> add_n_elt k elt (decompress tail)