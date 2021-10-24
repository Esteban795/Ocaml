(*Queues are there functional, represented by a couple of lists*)

let ajoute elt queue = 
  let e,s = queue in
  ((elt::e),s)


let rec enleve f = 
  match f with
  |u,x :: xs -> Some (x,(u,xs))
  |[],[] -> None
  |u,[] -> enleve ([],miroir u)


(*adds every element in the queue*)
let somme_file queue = 
  let rec aux s tuple = 
      match tuple with
      | [],[] -> s
      | x :: xs, [] -> aux (s + x) (xs,[])
      | [], y :: ys -> aux (s + y) ([],ys)
      | x :: xs,y :: ys -> aux (s + x + y) (xs,ys)
  in aux 0 queue

(*creates a functional queue from a list (first element of the list will be the first to go out*)
let file_fonct_of_list lst =
  let rec aux queue l = 
      match l with
          |[] -> queue
          |x :: xs -> aux (ajoute x queue) xs
  in aux ([],[]) lst

(*same but way faster*)
let file_fonct_of_list_simple lst = 
  ([],lst)


(*similar to list.map, but it is a list here*)
let rec itere_file f file = 
  match enleve file with
  |None -> ()
  |Some (x,fi) -> f x; itere_file f fi

(*displays the queue*)
let afficher file =
  itere_file (fun x -> print_newline (print_int x)) file;
  print_newline ()



(*imperative and more efficient queue*)
type 'a file_i = 
    {donnees:'a option array;
    mutable entree:int;
    mutable sortie:int;
    mutable cardinal:int}

let new_queue n = {donnees=Array.make n None;entree= 1;sortie= 0;cardinal= n}

let capacite_i q = Array.length q.donnees

let queue_from_list lst = 
  let l = List.length lst in
  let arr = Array.make l (Some 0) in
  let rec aux li i = 
      match li with
      |[] -> {donnees=arr;entree=(l-1);sortie=(l-1);cardinal=l}
      |h :: t -> (arr.(i) <- Some h);aux t (i+1)
  in aux lst 0

let ajoute_i elt q =
  if q.sortie = q.entree
  then failwith "Insertion dans file pleine."
  else
      q.donnees.(q.entree) <- elt;
      q.entree <- (q.entree + 1) mod q.cardinal;

let enleve_i q =
  if q.sortie = q.entree && q.donnees.(q.sortie) = None
  then None
  else
      let temp = q.donnees.(q.sortie) in
      q.donnees.(q.sortie) <- None;
      q.sortie <- (q.sortie + 1) mod q.cardinal;
      (temp)