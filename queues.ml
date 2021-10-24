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