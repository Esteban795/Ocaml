(* somme des entiers de a à b*)
let somme x = 
    let rec aux a b =
        if a = b then a else
        a + aux (a+1) b in aux 1 x



(* sum(k=1,n,1/k) *)
let premier_n x = 
    let rec aux k s = 
        if (1./. float k +. s) >= x then k
        else aux (k+1) (s +. 1./. float k) in aux 1 0.



(*logarithme discret de n*)
let deux_puissance k = 2. ** float k

let log2 n = 
    let rec aux k = 
    if 2. ** float k >= float n then k-1
    else aux (k+1) in aux 0

(* Somme des k² pour k allant de 1 à n*)

let sommes_carres n = 
    let rec aux k s = 
        if k > n then s
        else aux (k+1) (s + (k * k)) in aux 1 0

(* somme des float d'une liste*)

let rec somme_liste liste = 
    match liste with
        | [] -> 0.
        | head :: tail -> head +. somme_liste tail


(* longueur d'une liste*)

let rec longueur liste = 
    match liste with
        | [] -> 0
        | h :: t -> 1 + longueur t


(* moyenne d'une liste de float*)
let moyenne_float liste = somme_liste liste /. float (longueur liste)


(* Croissance d'une liste*)
let rec croissant liste = 
    match liste with
        | [] -> true
        | [x] -> true
        | h :: h2 :: t -> if h < h2 then croissant (h2 :: t) else false


(* concaténer deux listes *)
let rec concat l1 l2 = 
  match l1 with 
  | [] -> l2
  | h :: t -> h :: (concat t l2)

  
(* check if an element is in a list*)

let rec mem elt lst = 
  match lst with 
      | [] -> false
      | h :: t when h = elt -> true
      | h :: t -> mem elt t
  
(* nth element of a list*)
let nth lst n = 
  let rec aux l i = 
      match l with
          | [] -> failwith "Out of range"
          | h :: t when n = i -> h
          | h :: t -> aux t (i+1)
  in aux lst 0


(* nth first elements of a list*)
let rec take n lst =
  match lst with
      | [] when n != 0 -> lst
      | [] -> []
      | h :: t when n - 1 > 0 -> h :: (take (n-1) t)
      | h :: t -> [h]


(* range from a to b-1*)
let rec range a b = 
  if a = (b - 1) then (b-1) :: []
  else a :: (range (a+1) b)

(* miroir naif d'une liste*)

let rec miroir_naif l = 
  match l with 
  | [] -> []
  | h :: t -> concat (miroir_naif t) [h]


(*rev append*)
let rec rev_append l1 l2 = 
  concat (miroir_naif l1) (miroir_naif l2)

(* reverse a list *) 

let rev lst =
  let rec rev_append acc l =
    match l with
    | [] -> acc
    | h::t -> rev_append (h::acc) t in
  rev_append [] lst


(* list.map*)
let rec applique f lst = 
  match lst with
  | [] -> []
  | h :: t -> (f h) :: applique f t

(* somme des carrés d'une liste*)
let liste_carres lst =
  applique (fun x -> x*x) lst


(*check if duplicates in a list*)
let rec sans_doublons_triee lst = 
    match lst with 
    | [] -> true
    | [x] -> true
    | x :: y :: t -> if x == y then false else sans_doublons_triee (y::t)


(*remove duplicates from a list*)
let remove_dupl lst = 
  let seen = Hashtbl.create (List.length lst) in 
      List.filter (fun x -> let tmp = not(Hashtbl.mem seen x) in Hashtbl.replace seen x (); tmp) lst


(* no duplicates next to each other*)
let rec sans_doublons l = 
  match l with
  | [] -> true
  | [x] -> true
  | h :: y :: t -> if h = y then false else sans_doublons (y::t)