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


let nb_palindrome tbl length borne = 
  let compteur = ref 0 in
  for k = 0 to borne - length do
      if is_palindromic tbl length k then compteur := !compteur + 1
  done;
  (!compteur)

let is_palindromic tbl l b = 
  let arr1 = Array.make l 0 in
  let arr2 = Array.make l 0 in
  for i = b to b + l - 1 do
      arr1.(i - b) <- tbl.(i);
      arr2.(i - b) <- tbl.(b + l -(i -b) - 1)
  done;
  (arr1 = arr2);

(*118,120,1000*)