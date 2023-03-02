(* g is an adjacency list*)

(*
next is built following this :
- next.(i).(j) = None if and only if j isn't accessible from i
- if next.(i).(j) is Some k, then one of the shortest paths from i to j starts with edge i->k
*)
let floyd_warshall g = 
  let n = Array.length g in 
  let dist = Array.make_matrix n n infinity in
  let next = Array.make_matrix n n in 
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      dist.(i).(j) <- g.(i).(j)
      if g.(i).(j) <> infinity then prochain.(i).(j) <- Some j
    done
  done;
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do 
        let v = dist.(i).(k) +  dist.(k).(j) in
        if v < dist.(i).(j) then begin
          dist.(i).(j) <- v;
          prochain.(i).(j) <- prochain.(i).(k)
        end
      done
    done
  done;
  (dist,next)


(*Tree must be in the form of `next` matrix (see above)*)
let rec rebuild tree x y = 
  if x = y then [x]
  else 
    match tree.(x).(y) with
    |None -> failwith "Can't access y from x"
    |Some k -> i :: rebuild tree k j