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