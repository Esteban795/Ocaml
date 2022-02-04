type 'a rn =
    |V
    |N of 'a rn * 'a * 'a rn
    |R of 'a rn * 'a * 'a rn

let corrige_rouge abr = 
  match abr with
  | N (R (R (a, x, b), y, c), z, d)
  | N (R (a, x, R (b, y, c)), z, d)
  | N (a, x, R (R (b, y, c), z, d))
  | N (a, x, R (b, y, R (c, z, d)))
  | N (R (a, x, b), y, R (c, z, d))
  -> R (N (a, x, b), y, N (c, z, d))
  | t -> t


let cons arbre g x d =
  match arbre with
  | N _ -> N (g, x, d)
  | R _ -> R (g, x, d)
  | V -> failwith "erreur cons"


let rec insere_aux t elt = 
  match t with
  |V -> R(V,elt,V)
  |R(g,x,d) | N(g,x,d) ->
      if x = elt then t
      else if elt < x then corrige_rouge (cons t (insere_aux g elt) x d)
      else corrige_rouge (cons t g x (insere_aux d elt))
  
let red_to_black t =
  match t with
  |R(g,x,d) -> N(g,x,d)
  |_->t
    
let rec insere abr elt = 
    red_to_black (insere_aux abr elt)

let rec supprime_min arbre =
  match arbre with
  | V -> failwith "vide"
  | R (V, x, d) -> d,false
  | N (V, x, d) -> d,true
  | R (g, x, d) | N (g, x, d) ->
  let g', a_diminue = supprime_min g in
  corrige_noir_gauche (cons arbre g' x d) a_diminue

let corrige_noir_gauche t to_do = 
  if not to_do then (t,false)
  else match t with
  |R(a,x,N(b,y,c)) ->
      corrige_rouge (N(R(a,x,b),y,c)),false
  |N(R(a,x,b),y,c) ->
      N(N(a,x,b),y,c),false
  |N(a,x,N(b,y,c)) -> 
      corrige_rouge (N(R(a,x,b),y,c)),true
  |N (a, x, R (N (b, y, c), z, N (d, t, e))) ->
      N (corrige_rouge (N (a, x, R (b, y, c))),
      z,
      N (d, t, e)),
      false
  | _ -> failwith "impossible"