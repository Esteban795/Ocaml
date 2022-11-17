type vertex = int;;
type graph = vertex list array;;

let transpose g = 
  let n = Array.length g in 
  let reversed_g = Array.make n [] in 
  for i = 0 to n - 1 do 
    List.iter (fun x-> reversed_g.(x) <- i :: reversed_g.(x)) g.(i);
  done;
  reversed_g;;

let post_order g = 
  let n = Array.length g in 
  let seen = Array.make n false in 
  let lst = ref [] in 
  let rec explore s = 
    if not seen.(s) then begin 
      seen.(s) <- true;
      List.iter explore g.(s);
      lst := s :: !lst;
    end 
  in explore 0;
  seen.(0) <- true;
  for i = 0 to n - 1 do 
    if not seen.(i) then explore i;
  done;
  !lst;;

let accessibles_lists g lst = 
  let n = Array.length g in 
  let marque = Array.make n false in 
  let successors = ref [] in 
  let accessibles = ref [] in 
  let rec explore s = 
    if not marque.(s) then begin 
      marque.(s) <- true;
      List.iter explore g.(s);
      successors := s :: !successors
    end 
  in 
  let rec parcourir_lst l = 
    match l with
    |[] -> ()
    |x :: xs -> 
      if not marque.(x) then begin
      successors := [];
      explore x;
      accessibles := !successors :: !accessibles;
      end;
      parcourir_lst xs;
  in parcourir_lst lst;
  !accessibles;;


let kosaraju g = 
  let post = post_order g in 
  let reversed_g = transpose g in 
  accessibles_lists reversed_g post;;

let read_graph () = 
  let n,p = Scanf.scanf "%d %d\n" (fun n p ->(n,p)) in
  let g = Array.make n [] in
  for i = 0 to p - 1 do
    let x,y = Scanf.scanf "%d %d\n" (fun x y ->(x,y)) in
    g.(x) <- y :: g.(x);
  done;
  g;;

let _ =
  let g = read_graph () in 
  let cfc = kosaraju g in 
  let nb_cfc = List.length cfc in
  let taille_max = ref 0 in 
  let rec parcourir lst =
    match lst with 
    |[] -> ()
    |l :: l' -> taille_max := max !taille_max (List.length l); parcourir l';
  in 
  parcourir cfc;
  Printf.printf "Nombre de CFC : %d\n" nb_cfc;
  Printf.printf "Taille de la plus grande CFC : %d\n" !taille_max;;




      
      


