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
let concat t u= 
match t with 
| [] -> u
| x :: xs -> x :: (concat xs u)

(*reverse a list*)
let rec miroir l =
    match l with
    | [] -> []
    | h :: t -> concat (miroir t) [h]

  
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

(* no duplicates next to each other*)
let rec sans_doublons l = 
  match l with
  | [] -> true
  | [x] -> true
  | h :: y :: t -> if h = y then false else sans_doublons (y::t)


(* run-length encoding*)
let rec compresse lst =
  match lst with
      | [] -> []
      | h :: t -> match compresse t with
              | [] -> [(h,1)]
              | (y,n) :: tail when h = y -> (y,n+1) :: tail
              | lst -> (h,1) :: lst

(* run-length decoding*)
let rec decompresse lst = 
  let rec add_n_elt n o res = 
      if n = 0 then res
      else add_n_elt (n-1) o (o :: res) in
  match lst with
      | [] -> []
      | (elt,k) :: tail -> add_n_elt k elt (decompresse tail)

  (*remove duplicates from a list*)
  
let rmv_dupl lst = 
    let seen = Hashtbl.create (List.length lst) in
    List.filter (fun x -> let tmp = not (Hashtbl.mem seen x) in
                        Hashtbl.replace seen x ();
                        tmp) lst
  
  
(* index d'un element dans une liste*)
let index k l = 
    let rec aux lst i = 
        match lst with 
            | [] -> failwith "Element not found."
            | h :: t when h = k -> i
            | h :: t -> aux t (i+1)
        in aux l 0
  
  
(* DM début*)
let nb_occs x u = 
    let rec aux c l = 
        match l with 
            | [] -> c 
            | h :: t when h = x -> aux (c+1) t
            | h :: t -> aux c t
    in aux 0 u
  
  
let nb_distincts l = 
    let seen = Hashtbl.create (List.length l) in
    let rec aux lst c = 
        match lst with 
            | [] -> c
            | h :: t when (Hashtbl.mem seen h) -> aux t c
            | h :: t -> Hashtbl.replace seen h 0;aux t (c+1) 
    in aux l 0

let max_occs_triee l = 
    let rec aux lst counter maxi prev_elt = 
        match lst with
            | [] -> maxi
            | h :: t when h = prev_elt -> if (counter + 1) > maxi then aux t (counter + 1) (counter + 1) prev_elt else aux t (counter + 1) maxi prev_elt
            | h :: t -> aux t 1 maxi h
    in aux l 0 0 0

  
let nb_distincts_triee l =
  let rec aux lst prev_elt counter =
      match lst with 
          | [] -> counter 
          | h :: t when h = prev_elt -> aux t prev_elt counter
          | h :: t -> aux t h (counter + 1)
  in aux l 0 0

let rec nb_condition predicat lst = 
  match lst with
      | [] -> 0
      | h :: t when (predicat h) -> 1 + nb_condition predicat t
      | h :: t -> nb_condition predicat t

let somme_par_ligne tbl = 
  let temp = (Array.make (Array.length tbl) 0) in
  for i = 0 to ((Array.length tbl)-1) do
      let counter = ref 0 in
          for j = 0 to ((Array.length tbl.(i))-1) do
              counter := !counter + tbl.(i).(j)
          done;
          temp.(i) <- !counter
      done;
  (temp)


let somme_partielle tbl s e = 
  if s >= e then failwith "IndexError" else 
      let sum = ref 0 in
          for i = (s - 1) to (e - 1) do
              sum := !sum + tbl.(i)
          done;
      (!sum)

let somme_max_rapide tbl = 
  let maxi_ending_here = ref 0 in
  let maxi_so_far = ref 0 in
    for i = 0 to ((Array.length tbl) - 1) do
        maxi_ending_here := maximum (!maxi_ending_here + tbl.(i)) 0;
        maxi_so_far := maximum !maxi_ending_here !maxi_so_far;
    done;
  (!maxi_so_far)