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