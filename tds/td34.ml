(*borne gauche incluse, droite exclue*)

let indice_pic arr = 
  let rec aux deb fin = 
      if fin - deb = 1 then deb (*reste un élément*)
      else if fin - deb = 2 then if arr.(deb) >= arr.(fin - 1) then deb else fin - 1 (*reste que deux éléments*)
      else 
          let mid = ((fin + deb) / 2) in 
          if arr.(mid) >= arr.(mid - 1) && arr.(mid) >= arr.(mid + 1) then mid
          else if arr.(mid) <= arr.(mid - 1) then aux deb mid
          else aux (mid + 1) fin
  in aux 0 (Array.length arr)




(*Nombre de partitions d'un entier n (c'est à dire le nombre de manière de l'écrire avec une somme)*)

let partition n = 
  let arr = Array.make_matrix (n + 1) (n + 1) (-1) in
  let rec aux m k = 
      if m = 0 then 1 (*Une seule manière d'écrire 0*)
      else if m < 0 then 0 (*Aucune manière d'écrire un nombre négatif avec une somme de nombres positifs*)
      else if k = 0 then 0 
      else 
          if arr.(m).(k) = -1 then
              arr.(m).(k) <- aux (m - k) k + aux m (k - 1);
          arr.(m).(k)
  in aux n n





(*

f(k,d) = valeur maximale que l'on peut obtenir si le poids disponible est d et que l'on se limite à choisir des objets dont l'indice est strictement inférieur à k

Cas de base : 
- f(1,d) = 0 si p.(0) > d
- f(1,d) = v0 si p.(0) <= d
- f(0,d) = 0
- f(k,d) = - infini si d < 0

Relation de récurrence :

- f(k + 1,d) = f(k,d) si p.(k) > d
- f(k + 1,d) = max f(k,d) (v.(k) + f(k,d - k))

*)
let p_ex = [|3; 8; 5; 1; 6; 1; 2; 6; 6; 1; 7; 8; 9; 12; 4; 1; 5; 7; 11; 4; 1; 5; 12; 13|]
let v_ex = [|1; 2; 6; 3; 7; 8; 2; 3; 4; 7; 5; 2; 12; 8; 5; 3; 7; 10; 8; 7; 4; 15; 7; 20|]          


(*sac à dos naif avec juste la formule de récurrence*)
let sac p v pmax = 
    let rec aux k d = 
        if d < 0 then min_int
        else if k = 0 then 0
        else 
            let sup = aux (k - 1) d in
            let inf = v.(k - 1) + aux (k - 1) (d - p.(k - 1))
            in max sup inf
    in aux (Array.length p) pmax;


(*on compte le nombre d'appels DISTINCTS faits à aux*)
let sac_instrumente p v pmax = 
  let nb_appels = ref 0 in
  let rec aux k d = 
      incr nb_appels;
      if d < 0 then min_int
      else if k = 0 then 0
      else 
          let sup = aux (k - 1) d in
          let inf = v.(k - 1) + aux (k - 1) (d - p.(k - 1))
          in max sup inf
  in let res = aux (Array.length p) pmax in
  res, !nb_appels


(*version mémoïsée*)
let sac_mem p v pmax =
  let n = Array.length p in
  let table = Array.make_matrix (n + 1) (pmax + 1) None in (*n sur les lignes, p sur les colonnes*)
  let rec aux k d = 
      if d < 0 then min_int
      else if k = 0 then 0
      else 
          match table.(k).(d) with
          |Some v -> v
          |None ->
              let res = 
                  let sup = aux (k - 1) d in
                  let inf = v.(k - 1) + aux (k - 1) (d - p.(k - 1))
                  in max sup inf in
              table.(k).(d) <- Some res;
              res in
      aux n pmax


(*version bottom-up*)
let sac_dyn p v pmax = 
  let n = Array.length p in
  let table = Array.make_matrix (n + 1) (pmax + 1) 0 in
  let score_prec k d = if d < 0 then min_int else table.(k).(d) in
  for k = 1 to n do
      for d = 0 to pmax do
          let sup = score_prec (k - 1) d in
          let inf = v.(k - 1) + score_prec (k - 1) (d - p.(k - 1))
          in table.(k).(d) <- max sup inf 
      done;
  done;
  table.(n).(pmax)


(*version mémoïsée + reconstruit la solution optimale.
On a pris comme approche le fait de conserver la solution optimale de chaque case et de 
rajouter la nouvelle solution optimale par dessus*)
let sac_mem_reconstruit p v pmax =
  let n = Array.length p in
  let table = Array.make_matrix (n + 1) (pmax + 1) None in
  let rec aux k d = 
      if d < 0 then (min_int,[])
      else if k = 0 then (0,[])
      else 
          match table.(k).(d) with
          |Some tuple -> tuple
          |None ->
              let res = 
                  let sup,sol_sup = aux (k - 1) d in
                  let inf,sol_inf = aux (k - 1) (d - p.(k - 1)) in
                  if v.(k - 1) + inf > sup then
                      (v.(k - 1) + inf, (k - 1) :: sol_inf)
                  else
                      (sup,sol_sup) in
              table.(k).(d) <- Some res;
              res in
      aux n pmax