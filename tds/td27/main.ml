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


let rec insere bst elt = 
  match bst with
  |V -> N(V,elt,V)
  |N(g,key,d) -> 
    if elt = key then bst
    else 
      if elt > key then N(g,key,insere d elt)
      else N(insere g elt, key,d)


let rec cardinal arbre = 
  match arbre with
  |V -> 0
  |N(g,key,d) -> 1 + cardinal g + cardinal d


let construit lst =
  let rec aux restants acc =
    match restants with
    |[] -> acc
    |x :: xs -> aux xs (insere acc x)
  in aux lst V


let elements arbre = 
  let rec aux t acc = 
      match t with
      |V -> acc
      |N(g,key,d) ->
          let temp_d = aux d acc in
          aux g (key :: temp_d)
  in aux arbre []
            

let rec extrait_min tree =
  match tree with
  |V -> failwith "arbre vide"
  |N (V,x,d) -> (x,d)
  |N (g,x,d) -> 
      let m,g' = extrait_min g in 
      (m,N(g',x,d))


let rec supprime arbre elt =
  match arbre with
  |V -> V
  |N(g,key,d) when elt > key -> N(g,key,supprime d elt)
  |N(g,key,d) when key > elt -> N(supprime g elt,key,d)
  |N(V,key,d) -> d
  |N(g,key,V) -> g
  |N(g,key,d) ->
      let mini,d_next = extrait_min d in
      N(g,mini,d_next)

let rec separe t x =
  match t with
  |V -> V,V
  |N(g,key,d) ->
      if x = key then N(g,key,V),d
      else
          if x < key then
          let lower,higher = separe g x in
          lower,N(higher,key,d)
      else 
          let lower,higher = separe d x in
          N(g,key,lower),higher


let verifie_arbre t =
  let rec aux lst =
      match lst with
      |x :: y :: ys -> (x < y) && aux (y :: ys)
      |_ -> true
  in aux (elements t)

(*complexité O(t) , espace O(t)*)


let tab_elements t = 
  Array.of_list (elements t)


let verifie_abr_2 arbre = 
  let minimum = ref min_int in
  let rec aux t = 
      match t with
      |V -> true
      |N(g,x,d) ->
            aux g && x > !minimum && (minimum := x; aux d)
  in aux arbre


let rec aux arbre indice t = 
  match arbre with
  |V -> ()
  |N(g,x,d) ->
      aux g indice t;
      t.(!indice) <- x;
      indice := !indice + 1;
      aux d indice t
  
let tab_elements_2 t = 
  match t with
  |V -> [||]
  |N(g,x,d) ->
      let len = cardinal t in
      let arr = Array.make len x in
      let i = ref 0 in
          aux t i arr;
          arr
  
type ('k,'v) dict = 
|Empty
|Node of ('k,'v) dict * 'k * 'v * ('k,'v) dict


let rec get key dict = 
  match dict with
  |Empty -> None
  |Node (g,k,v,d) -> if k = key then Some v 
                      else if key < k then get key g
                      else get key d



let rec set cle valeur dict =
  match dict with
  |Empty -> Node (Empty,cle,valeur,Empty)
  |Node(g,k,v,d) ->
      if k = cle then Node (g,cle,valeur,d)
      else if cle < k then Node (set cle valeur g,k,v,d)
      else Node (g,k,v,set cle valeur d)

      
let rec extrait_min_dict dict = 
  match dict with
  |Empty -> failwith "empty dict"
  |Node(Empty,k,v,d) -> (k,v,d)
  |Node(g,k,v,d) ->
      let cle_min,val_min,d' = extrait_min_dict d in
      (cle_min,val_min,Node(g,k,v,d'))
      

let rec remove cle dict =
  match dict with
  |Empty -> Empty
  |Node(g,k,v,d) when cle < k -> Node(remove cle g,k,v,d)
  |Node(g,k,v,d) when cle > k -> Node(g,k,v,remove cle d)
  |Node(Empty,k,v,d) -> d
  |Node(g,k,v,Empty) -> g
  |Node (g,k,v,d) ->
      let cle_min,val_min,d_next = extrait_min_dict d in
      Node(g,cle_min,val_min,d_next)

let get_occurences dict elt = 
  match get elt dict with
  |None -> 0
  |Some i -> i

let add_occurences dict elt = 
  let i = get_occurences dict elt in
  set elt (i + 1) dict

let remove_occurences dict elt =
  let i = get_occurences dict elt in
  if i = 1 then remove elt dict
  else set elt (i - 1) dict
  
let rec size dict = 
  match dict with
  |Empty -> 0
  |Node(left,_,valuation,right) -> valuation + size left + size right


type 'a multiset =
  |Empty
  |Node of int * 'a multiset * 'a * int * 'a multiset



let rec get_occurences_2 t elt = 
  match t with
  |Empty -> 0
  |Node (_,g,y,i,d) ->
      if y = elt then i
      else if elt < y then get_occurences_2 g elt
      else get_occurences_2 d elt

let rec add_occurences_2 t elt = 
  match t with
  |Empty -> Node(1,Empty,elt,1,Empty)
  |Node (n,g,y,i,d) ->
      if elt = y then Node (n + 1,g,y, i + 1,d)
      else if elt < y then Node (n + 1,add_occurences_2 g elt,y,i,d)
      else Node (n + 1,g,y,i,add_occurences_2 d elt)

let rec aux t elt = 
  match t  with
  |Empty -> Empty
  |Node (n,g,y,i,d) when elt < y -> Node (n - 1,aux g elt,y,i,d)
  |Node (n,g,y,i,d) when elt > y -> Node (n - 1,g,y,i,aux d elt)
  |Node(n,Empty,y,i,t') | Node(n,t',y,i,Empty) -> t'
  |Node(n,g,y,i,d) -> let m,i_m,d' = extrait_min d in 
      Node (n - 1,g,m,i_m,d')
  |Node(n,g,y,i,d) -> Node(n - 1,g,y, i - 1,d)
  
let rem_occurences_2 t elt = 
  if get_occurences_2 t elt = 0 then t
  else aux t elt

let size t = 
  match t with
  |Empty -> 0
  |Node(n,_,_,_,_) -> n


let rec select t index = 
  match t with
  |Empty -> failwith "invalid index"
  |Node (n,g,y,i,d) ->
      let size_g = size g in
      if index < size_g then select g index
      else if index < size_g + i then y
      else select d (index - size_g - i)