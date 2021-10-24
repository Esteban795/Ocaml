let miroir lst =
  let rec aux acc l = 
      match l with
      | [] -> acc
      | h :: t -> aux (h :: acc) t
  in aux [] lst

let binary_search t x =
  let rec aux deb fin =
      if deb >= fin then (Array.length t)
      else
          let milieu = ((deb+fin)/2) + in
              if t.(milieu) = x then milieu
              else if t.(milieu) < x then aux (milieu + 1) fin
              else aux deb milieu
      in aux 0 (Array.length t)



(*Find the maximum sum of an array*)
let somme_max_rapide tbl = 
  let maxi_ending_here = ref 0 in
  let maxi_so_far = ref 0 in
    for i = 0 to ((Array.length tbl) - 1) do
        maxi_ending_here := max (!maxi_ending_here + tbl.(i)) 0;
        maxi_so_far := max !maxi_ending_here !maxi_so_far;
    done;
  (!maxi_so_far)


(*finds how many x is in t, t must be sorted first. 
O(log(n)) *)
let premiere_occ t x =
  let rec aux deb fin =
      if deb > fin then -1 else
      if deb = fin then deb
      else
          let milieu = ((deb+fin)/2) in
              if t.(milieu) = x then aux deb milieu
              else if t.(milieu) < x then aux (milieu + 1) fin
              else aux deb milieu
      in aux 0 (Array.length t)


let derniere_occ t x =
  let rec aux deb fin =
      if deb > fin then -1 else
      if deb = (fin-1) then deb
      else
          let milieu = ((deb+fin)/2) in
              if t.(milieu) = x then aux milieu fin
              else if t.(milieu) < x then aux milieu fin
              else aux deb milieu
      in aux 0 (Array.length t)

let nb_occs_triee elt tbl =
  let first_index = premiere_occ tbl elt in
  let last_index = derniere_occ tbl elt in
  if first_index = -1 || last_index = -1 then failwith "Element not in the list."
  else (last_index - first_index) + 1