type vertex = int

type graph = vertex list array

let miroir g = 
  let n = Array.length g in 
  let mir = Array.make n [] in 
  let rec add_neighbors s neighbors = 
    match neighbors with 
    | [] -> ()
    | x :: xs ->
        mir.(x) <- s :: mir.(x);
        add_neighbors s xs 
  in
  for i = 0 to n - 1 do 
    add_neighbors i g.(i) 
  done;
  mir 

let post_order g = 
  let n = Array.length g in 
  let post = ref [] in 
  let seen = Array.make n false in 
  let rec explore i = 
    if not seen.(i) then 
      begin
        seen.(i) <- true;
        List.iter explore g.(i);
        post := i :: !post
      end
  in
  for i = 0 to n - 1 do 
    explore i 
  done;
  !post


let accessibles_lists g_mir order = 
  let n = Array.length g_mir in 
  let seen = Array.make n false in 
  let scc_list = ref [] in 
  let current_scc = ref [] in 
  let rec explore i = 
    if not seen.(i) then 
      begin 
        seen.(i) <- true;
        List.iter explore g_mir.(i);
        current_scc := i :: !current_scc
      end 
  in
  let process i = 
    if not seen.(i) then 
      begin 
        current_scc := [];
        explore i;
        scc_list := !current_scc :: !scc_list
      end 
  in
  List.iter process order;
  !scc_list

let kosaraju g = 
  let order = post_order g in 
  let mir = miroir g in 
  List.rev (accessibles_lists mir order)


let read_graph () = 
  let n,p = Scanf.scanf "%d %d\n" (fun x y -> (x,y)) in 
  let g = Array.make n [] in 
  for i = 0 to p - 1 do 
    Scanf.scanf "%d %d\n" (fun x y -> g.(x) <- y :: g.(x))
  done;
  g

let main () = 
  let g = read_graph () in 
  let scc = kosaraju g in 
  let nb_scc = List.length scc in 
  let maxi = ref 0 in 
  let rec parcourir lst = 
    match lst with 
    |[] -> ()
    | h :: t -> maxi := max !maxi (List.length h); parcourir t
  in 
  parcourir scc;
  Printf.printf "Nombre de CFC : %d\n" nb_scc;
  Printf.printf "Taille de la plus grande CFC : %d\n" !maxi;;