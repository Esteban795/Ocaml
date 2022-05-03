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
end = struct

  type t =
    {mutable last : int;
     priorities : float array;
     keys : int array;
     mapping : int array}

  let length q = q.last + 1

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
    if right i <= q.last && q.priorities.(right i) < q.priorities.(i) then
        mini := right i;
    if !mini != i then begin
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


type weighted_graph = (int * float) list array

let g0 : weighted_graph =
  [| [(1, 15.); (2, 16.); (3, 13.); (4, 9.)];
     [(0, 15.); (4, 7.); (5, 1.)];
     [(0, 16.); (3, 5.); (4, 5.); (5, 6.)];
     [(0, 13.); (2, 5.); (4, 3.)];
     [(0, 9.); (1, 7.); (2, 5.); (3, 3.)];
     [(1, 1.); (2, 6.)] |]

let random_graph n avg_outdegree =
  Random.init 0;
  let weight () = Random.float 100. in
  let build_adj i =
    let rec aux j =
      if j = n then
        []
      else if (i <> j) && Random.int (n - 1) < avg_outdegree then
        (j, weight ()) :: aux (j + 1)
      else aux (j + 1) in
    aux 0 in
  Array.init n build_adj

let g1 = random_graph 20 2

let g2 = random_graph 1000 10



let dijkstra g x0 = 
  let n = Array.length g in
  let dist = Array.make n infinity in
  let q = PrioQ.make_empty n in
  PrioQ.insert q (x0,0.);
  dist.(x0) <- 0;
  while PrioQ.length != 0 then begin
      let (j,d) = PrioQ.extract_min q in
      let update (k,y) = 
          let new_dist = d + x in
          if new_dist < dist.(k) then begin
              dist.(k) <- new_dist;
              if PrioQ.mem  k then PrioQ.decrease_priority q (k, new_dist)
              else PrioQ.insert q (k,new_dist)
            end in 
      List.iter updage g.(j)
  done;
  dist