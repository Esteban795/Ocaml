module PrioQ :
sig
  type t
  val get_min : t -> (int * float)
  val extract_min : t -> (int * float)
  val insert : t -> (int * float) -> unit
  val length : t -> int
  val capacity : t -> int
  val make_empty : int -> t
  val decrease_priority : t -> (int * float) -> unit
  val mem : t -> int -> bool
  val parent : int -> int
  val left : int -> int
  val right : int -> int
  
end = struct

  type t =
    {mutable last : int;
     priorities : float array;
     keys : int array;
     mapping : int array}

  let length q = q.last + 1
  
  let parent i = (i - 1)/2
  let left i = 2 * i + 1
  let right i = 2 * i + 2
  
  let capacity q = Array.length q.keys

  let make_empty n =
    {last = -1;
     priorities = Array.make n nan;
     keys = Array.make n 0;
     mapping = Array.make n (-1)}

  let mem q x =
    q.mapping.(x) >= 0

  let swap t i j =
    let tmp = t.(i) in
    t.(i) <- t.(j);
    t.(j) <- tmp

  let full_swap q i j =
    swap q.keys i j;
    swap q.priorities i j;
    swap q.mapping q.keys.(i) q.keys.(j)

  let get_min q = (q.keys.(0), q.priorities.(0))

  let rec sift_up q i = 
    let sup = parent i in 
    if i > 0  && q.priorities.(i) < q.priorities.(sup) then begin
        full_swap q i sup;
        sift_up q sup;
    end


  let insert q (key,prio) = 
    if length q = capacity q then failwith "plus de place"
    else begin
        let last = q.last + 1 in
        q.keys.(last) <- key;
        q.priorities.(last) <- prio; 
        q.mapping.(key) <- last;
        q.last <- last;
        sift_up q q.last
    end

  let rec sift_down q i =
    let mini = ref i in
    if left i <= q.last && q.priorities.(left i) < q.priorities.(i) then
        mini := left i;
    if right i <= q.last && q.priorities.(right i) < q.priorities.(!mini) then
        mini := right i;
    if !mini <> i then begin
        full_swap q i !mini;
        sift_down q !mini
    end


  let extract_min q = 
    if q.last < 0 then failwith "file vide"
    else begin
        let key = q.keys.(0) in 
        let prio = q.priorities.(0) in
        full_swap q 0 q.last;
        q.last <- q.last - 1;
        sift_down q 0;
        key,prio
    end
    
  let decrease_priority q (x, prio) = 
      let i = q.mapping.(x) in 
      assert (mem q x && prio <= q.priorities.(i));
      q.priorities.(i) <- prio;
      sift_up q i
      
end




let dijkstra g x0 = 
  let n = Array.length g in
  let dist = Array.make n infinity in
  let q = PrioQ.make_empty n in
  PrioQ.insert q (x0,0.);
  dist.(x0) <- 0.;
  while PrioQ.length q <> 0 do 
      let (j,d) = PrioQ.extract_min q in
      let update (k,y) = 
          let new_dist = d +. y in
          if new_dist < dist.(k) then begin
              dist.(k) <- new_dist;
              if PrioQ.mem q k then PrioQ.decrease_priority q (k, new_dist)
              else PrioQ.insert q (k,new_dist)
          end in
      List.iter update g.(j)
  done;
  dist



let dijkstra_tree g x0 =
  let n = Array.length g in
  let tree = Array.make n None in 
  let dist = Array.make n infinity in
  let q = PrioQ.make_empty n in
  PrioQ.insert q (x0,0.);
  dist.(x0) <- 0.;
  tree.(x0) <- Some x0;
  while PrioQ.length q <> 0 do 
      let (j,d) = PrioQ.extract_min q in
      let update (k,y) = 
          let new_dist = d +. y in
          if new_dist < dist.(k) then begin
              dist.(k) <- new_dist;
              tree.(k) <- Some j;
              if PrioQ.mem q k then PrioQ.decrease_priority q (k, new_dist)
              else PrioQ.insert q (k,new_dist)
          end in
      List.iter update g.(j)
  done;
  dist,tree


let reconstruct_path p goal = 
  let rec aux current =
      match p.(current) with
      |None -> failwith "pas de chemin"
      |Some i when i = current -> [current]
      |Some i -> current :: aux i in 
  List.rev (aux goal)


type commune =
{id : int;
  insee : string;
  nom : string;
  pop : int;
  dep : string}


let lire_communes nom_fichier = 
  let f = open_in nom_fichier in
  let lst = ref [] in
  try
      let premiere_ligne = input_line f in
      while true do 
          let line = input_line f in
          let fmt id insee nom pop dep = {id;insee;nom;pop;dep} in
          let obj = Scanf.sscanf line "%d@;%s@;%s@;%d@;%s@" fmt in
          lst := obj :: !lst
      done;
      assert false
  with
  |End_of_file -> close_in f; Array.of_list (List.rev !lst)



let lire_graphe nb_communes fichier_adjacence = 
  let tab_sommets = Array.make nb_communes [] in 
  let file = open_in fichier_adjacence in
  try
      let premiere_ligne = input_line file in 
      while true do 
          let line = input_line file in 
          let fmt source dest = 
              (source,dest) in
          let x,y = Scanf.sscanf line "%d@;%d@" fmt in
          tab_sommets.(x) <- y :: tab_sommets.(x);
          tab_sommets.(y) <- x :: tab_sommets.(y)
      done;
      assert false
  with
  |End_of_file -> close_in file;


let tab_communes = lire_communes "communes.csv"

let g_adj = lire_graphe 35847 "./adjacences.csv"

let affiche chemin =
  let affiche_commune i =
    let c = tab_communes.(i) in
    Printf.printf "%s (%s) : %d\n" c.nom c.dep c.pop in
  List.iter affiche_commune chemin



exception Trouve

let saute_canton init = 
    let graphe = g_adj in
    let communes = tab_communes in
    let n = Array.length graphe in
    let vus = Array.make n false in 
    let tree = Array.make n None in
    let queue = Queue.create () in 
    Queue.add init queue;
    vus.(init) <- true;
    try
        while not(Queue.is_empty queue) do
            let elt = Queue.pop queue in
            if communes.(i).pop >= 50000 then raise (Trouve elt);
            let rec ajouter s = 
                if not vus.(s) then begin
                    vus.(s) <- true;
                    Queue.add j queue;
                    tree.(s) <- Some i end in 
            List.iter ajouter g.(i)
        done;
        []
    with
        |Trouve i -> reconstruct_path tree i



let commune_la_plus_paumee g communes = 
  let max_dist = ref 0 in 
  let longest_path = ref [] in
  let n = Array.length g in 
  for i = 0 to n - 1 do
      let chemin = saute_canton i in 
      let len_chemin = List.length chemin in
      if len_chemin > max_dist then begin
          longest_path := chemin;
          max_dist := len_chemin;
      end
  done;
  affiche communes !longest_path
(*Q19 : sommet x et y reliés, l'arc reliant x et y a le poids de la population de y*)


