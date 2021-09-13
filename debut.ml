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
let concat l1 l2 = 
    match l1 with 
    | [] -> l2
    | h :: t -> h :: (concat l1 l2)

  