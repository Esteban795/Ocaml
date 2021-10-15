let triangle_pascal n =
  let tbl = Array.make (n + 1) [||] in
  for i = 0 to n do
      tbl.(i) <- Array.make (i + 1) 0;
      for j = 0 to i do
          if j = i || j = 0 then tbl.(i).(j) <- 1 else tbl.(i).(j) <- tbl.(i - 1).(j) + tbl.(i - 1).(j - 1)
      done;
  done;
  (tbl)


let binom_it k n =
  if k > n || k < 0 then 0
  else if k = 0 || k = n then 1
  else let temp = triangle n in temp.(n).(k)


let rec pour_tout predicate lst = 
  match lst with
  |[] -> true
  | head :: tail -> if predicate head then pour_tout predicate tail else false


let rec existe predicate lst = 
  match lst with
  |[] -> false
  |head :: tail -> if predicate head then true else existe predicate tail

let filtre predicate lst =
  let rec aux l =
      match l with 
      |[] -> []
      | head :: tail -> if predicate head then head :: (aux tail) else  aux tail
  in aux lst


let appartient elt lst =
  existe (fun x-> x = elt) lst


let inclus u v= 
  pour_tout (fun x -> appartient x v) u
      (* complexité de pour_tout : O(n)
      complexité de appartient : O(n
      *)
let egal u v =
  inclus u v && inclus v u

let rec union_eff g d =
  match g,d with
      |[],_ -> d
      |_,[]-> g
      | x :: xs,y :: ys -> if x = y then x :: union_eff xs ys else if x <= y then x :: union_eff xs d else y :: union_eff g ys

let rec inter_eff g d =
  match g,d with
      |[],_ -> []
      |_,[]-> []
      | x :: xs,y :: ys -> if x = y then x :: (inter_eff xs ys) else if x < y then (inter_eff  xs d) else  (inter_eff  g ys) 

let rec prive_de_eff u v = 
  match u,v with
      |[], _ -> []
      |_,[] -> u
      |x :: xs, y :: ys when x = y -> prive_de xs ys
      |x :: xs, y :: ys when x < y -> x :: prive_de xs v
      |x :: xs, y :: ys -> prive_de u ys


let rec inclus_eff u v = 
  match u,v with
  |_,[] -> false
  |[], _ -> true
  |x :: xs, y :: ys  when x = y -> inclus_eff xs ys
  |x :: xs, y :: ys when x > y -> inclus_eff u ys
  |x :: xs, y :: ys -> inclus_eff xs v


let rec eclate lst = 
  match lst with
      |[] -> ([],[])
      |[x] -> ([x],[])
      |x :: y :: tail -> let a,b = eclate tail in (x :: a, y :: b)
  
  
  let rec fusion u v = 
    match u,v with
        | [],_ -> v
        | _,[] -> u
        |x :: xs,y :: ys -> if x <= y then x :: fusion xs v else y :: fusion u ys

        let rec tri_uniques u = 
          match u with
          |[] -> []
          |[x] -> [x]
          |x :: xs -> let a,b = eclate u in let t1,t2 = tri_uniques a,tri_uniques b in union_eff t1 t2