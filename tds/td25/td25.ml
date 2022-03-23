type bit = Z | U

type nombre = bit list

let rec succ nombre = 
  match nombre with
  |[] -> [U]
  |Z :: xs -> U :: xs
  |U :: xs -> Z :: succ xs


let rec pred nombre = 
  match nombre with
  |[] -> failwith "mauvais truc"
  |[U] -> []
  |Z :: xs -> U :: pred xs
  |U :: xs -> Z :: xs


type 'a arbre = F of 'a | N of int * 'a arbre * 'a arbre


let rec get_arbre arbre i = 
  match arbre with
  |F x -> if i = 0 then x else failwith "depassement"
  |N(v,g,d) ->
      if i < v/2 then get_arbre g i
      else get_arbre d (i - v/2)


let rec set_arbre arbre i x = 
  match arbre with
  |F x -> if i = 0 then F x else failwith "depassement"
  |N(v,g,d) ->
      if i < v/2 then N(v,set_arbre g i x,d)
      else N(v,g,set_arbre d (i - v/2) x)

type 'a chiffre = Ze|Un of 'a arbre
type 'a liste_binaire = 'a chiffre list


let a = F 100
let b = N(2,F 50, F 25)
let c = N(8,
        N(4,N(3,F 1,F 4), N(2,F 9, F 16)),
        N(4,N(2,F 25,F 36),N(2,F 49,F 64)))
let li = [Un a; Un b; Ze; Un c]


let size = function
    |F _ -> 1
    |N(n, _, _) -> n


let get l i = 
  let rec aux lst n =
      match lst with
      |[] -> failwith "depassement"
      |Ze :: xs -> aux xs n
      |Un a :: xs -> if n < size a then (a,n)
                      else aux xs (n - size a)
  in
  let t,index = aux l i in
  get_arbre t index
          

let rec set l i x = 
  match l with
  |[] -> failwith "depassement"
  |Ze :: xs -> set xs i x
  |Un t :: ts  when size t > i -> Un (set_arbre t i x) :: ts
  |Un t :: ts -> Un t :: set ts (i - size t) x


let lier t1 t2 = 
  N (size t1 + size t2,t1,t2)
          
let rec cons_arbre l t =
    match l with
    |[] -> [Un t]
    |Ze :: xs -> Un t :: xs
    |Un t2 :: xs ->  Ze :: cons_arbre xs (lier t t2)
          
          
let cons u x =
    cons_arbre u (F x)


let rec uncons_arbre l = 
  match l with
  |[] -> failwith "depassement"
  |Un t :: ts -> (t,Ze :: ts)
  |Ze :: ts ->
      let y, ys = uncons_arbre ts in
      match y with
      |N(_,g,d) -> (g, Un d :: ys)
      |_ -> failwith "??"
        
let uncons lst =
    match lst with 
    |[Un (F x)] -> x,[]
    |_ -> 
        match uncons_arbre lst with
        |F x,ts -> x,ts
        |_ -> failwith "??"