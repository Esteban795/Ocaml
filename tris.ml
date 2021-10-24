(*tri fusion*)
let rec eclate lst = 
  match lst with
      |[] -> ([],[])
      |[x] -> ([x],[])
      |x :: y :: tail -> let a,b = eclate tail in (x :: a, y :: b)


let rec fusion u v = 
  match u,v with
      | [],_ -> v
      | _,[] -> u
      |x :: xs,y :: ys -> if x <= y then x :: fusion xs v else y :: fusion u ys


let rec tri_fusion t = 
  match t with
      | [] | [_] -> t
      |_ -> let a,b = eclate t in fusion (tri_fusion a) (tri_fusion b)

  
(*Tri insertion sur une liste *)
let rec insere v x = 
  match v with 
      |[] -> [x]
      |h :: t when h < x -> h :: (insere t x)
      |h :: t ->  x :: h :: t

let tri_insertion lst = 
  let temp = ref [] in
  let rec aux current_lst = 
      match current_lst with
          |[] -> !temp
          |h :: t -> temp := insere !temp h;aux t
  in aux lst



(*Tri insertion tableau*)
let echange t i j =
  t.(i) <- t.(i) + t.(j);
  t.(j) <- t.(i) - t.(j);
  t.(i) <- t.(i) - t.(j);

let insertion_en_place tbl i = 
  let j = ref i in
  while !j > 0 && tbl.(!j) < tbl.(!j - 1) do
      echange tbl !j (!j-1);
      decr j
  done; 

let tri_insertion_tableau tbl = 
  for j = 0 to (Array.length tbl) - 1 do
      insertion_en_place tbl j
  done;
  (tbl)