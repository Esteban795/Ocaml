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

let construit lst =
  let rec aux restants acc =
    match restants with
    |[] -> V 
    |x :: xs -> aux xs (insere acc x)
  in aux lst V

let rec supprime arbre elt =
  match arbre with
  |V -> V
  |N (V,x,d) when x = elt -> d
  |N (g,x,V) when x = elt -> g
  |N(g,x,d) when x < elt -> N( supprime g elt, x, d)
  |N(g,x,d) when x > elt -> N(g, x, supprime d elt)
  |N(g,x,d) -> let m = minimum dr in N(g,m,supprime dr m)

let rec supprime_min tree =
  match tree with
  |V -> failwith "arbre vide"
  |N (V,x,dr) -> dr
  |N (g,x,d) -> N (supprime_min g,x,dr)
  
type ('k,'v) dict = 
  |Vide
  |Noeud of 'k * 'v * ('k,'v) dict * ('k,'v) dict

let rec get key dict = 
  match dict with
  |Vide -> None
  |Noeud (k,v,g,d) -> if k = key then Some v 
                      else if key < k then get key g
                      else get key d

let rec set cle valeur dict =
  match dict with
  |Vide -> Noeud (cle,valeur,Vide,Vide)
  |Noeud (k,v,g,d) ->
      if k = cle then Noeud (cle,valeur,g,d)
      else if cle < k then Noeud (k,v,set cle valeur g,d)
      else Noeud (k,v,g,set cle valeur d)