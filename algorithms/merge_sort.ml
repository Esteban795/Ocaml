(*tri fusion*)
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


let rec tri_fusion t = 
  match t with
      | [] | [_] -> t
      |_ -> let a,b = eclate t in fusion (tri_fusion a) (tri_fusion b)