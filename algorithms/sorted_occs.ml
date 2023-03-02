(*finds how many x is in t, t must be sorted first. 
O(log(n)) *)
let first_occ t x =
  let rec aux deb fin =
      if deb > fin then -1 else
      if deb = fin then deb
      else
          let milieu = ((deb+fin)/2) in
              if t.(milieu) = x then aux deb milieu
              else if t.(milieu) < x then aux (milieu + 1) fin
              else aux deb milieu
      in aux 0 (Array.length t)


let last_occ t x =
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
  let first_index = first_occ tbl elt in
  let last_index = last_occ tbl elt in
  if first_index = -1 || last_index = -1 then failwith "Element not in the list."
  else (last_index - first_index) + 1