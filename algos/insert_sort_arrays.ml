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