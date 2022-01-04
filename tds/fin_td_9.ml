let is_palindromic tbl l b = 
  let arr1 = Array.make l 0 in
  let arr2 = Array.make l 0 in
  for i = b to b + l - 1 do
      arr1.(i - b) <- tbl.(i);
      arr2.(i - b) <- tbl.(b + l -(i -b) - 1)
  done;
  (arr1 =arr2)
  
let nb_palindrome tbl length borne = 
  let compteur = ref 0 in
  for k = 0 to borne - length do
      if is_palindromic tbl length k then compteur := !compteur + 1
  done;
  (!compteur)

(*
l=6, l <= b <= 1000 : 118,
l = 7, l <= b <= 1000 : 120,
l = 20, l <= b <= 1 000 000 : 1000
*)


let init tbl borne =
  let compteur_0 = ref 0 in
  let compteur_1 = ref 0 in
  for i = 0 to borne - 1 do
      if tbl.(i) = 1 then incr compteur_1 else incr compteur_0
  done;
  (compteur_0,compteur_1)

let check ref0 ref1 cpt = 
  if !ref0 = !ref1 then incr cpt
  
let sections_equi tbl l borne = 
  let compteur = ref 0 in
  let compteur_0,compteur_1 = init tbl l in
  check compteur_0 compteur_1 compteur;
  for i = 1 to borne - l do
  begin
      if tbl.(i + l - 1) = 1 then incr compteur_1 else incr compteur_0
  end;
  begin
      if tbl.(i - 1) = 0 then decr compteur_0 else decr compteur_1
  end;
      check compteur_0 compteur_1 compteur;
  done;
  !compteur
  
(*l = 10, l <= b <= 1000 : 204,
l = 100, l <= b <= 1 000 000 : 80041,
l = 100 000, l <= b <= 1 000 000 : 3433*)