type 'a abr = 
  |V 
  |N of 'a abr * 'a * 'a abr


let rec appartient bst elt = 
  match bst with
  |V -> false
  |N(g,key,d) -> 
    if elt = key then true 
    else 
      if elt < key then appartient g elt
      else appartient d elt

let minimum bst = 
    match bst with
      |V -> failwith "arbre vide"
      |N(g,key,d) -> 
        let rec aux t m = 
          match t with
          |V -> m
          |N(g,c,d) -> aux g c
        in aux g key


let rec insere bst elt = 
  match bst with
  |V -> N(V,elt,V)
  |N(g,key,d) -> 
    if elt = key then bst
    else 
      if elt > key then N(g,key,insere d elt)
      else N(insere g elt, key,d)

