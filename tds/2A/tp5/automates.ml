type etat = int
type lettre = int
type mot = lettre list

type afd =
  {m : int;
   n : int;
   init : etat;
   term : bool array;
   delta : etat array array}


let rec delta_star a q mot =
  match mot with
  | [] -> q
  | x :: xs ->
    let q' = a.delta.(q).(x) in
    if q' = -1 then -1
    else delta_star a q' xs

let accepte a mot =
  let q = delta_star a a.init mot in
  q <> -1 && a.term.(q)

(* On fait un parcours en profondeur à partir de l'état initial. *)

let complet a =
  let flag = ref true in
  for q = 0 to a.n - 1 do
    for x = 0 to a.m - 1 do
      flag := !flag && a.delta.(q).(x) <> -1
    done
  done;
  !flag

let accessibles a =
  let vus = Array.make a.n false in
  let rec dfs q =
    if q <> -1 && not vus.(q) then begin
      vus.(q) <- true;
      Array.iter dfs a.delta.(q)
    end in
  dfs a.init;
  vus

let langage_non_vide a =
  let vus = accessibles a in
  let q = ref 0 in
  while !q < a.n && not (vus.(!q) && a.term.(!q)) do
    incr q
  done;
  !q < a.n

let inverse a =
  let g = Array.make_matrix a.n a.n false in
  for q = 0 to a.n - 1 do
    for x = 0 to a.m - 1 do
      let q' = a.delta.(q).(x) in
      if q' <> -1 then g.(q').(q) <- true
    done
  done;
  g

let coaccessibles a =
  let g_inv = inverse a in
  let vus = Array.make a.n false in
  let rec dfs q =
    if not vus.(q) then begin
      vus.(q) <- true;
      for q' = 0 to a.n - 1 do
        if g_inv.(q).(q') then dfs q'
      done
    end in
  for q = 0 to a.n - 1 do
    if a.term.(q) then dfs q
  done;
  vus

let est_emonde a =
  let tous = Array.make a.n true in
  (accessibles a = tous) && (coaccessibles a = tous)

let complementaire a =
  let n = a.n in
  let m = a.m in
  let delta = Array.init n (fun i -> Array.copy a.delta.(i)) in
  let init = a.init in
  let term = Array.map not a.term in
  {n; m; delta; init; term}

let complete a =
  let term = Array.make (a.n + 1) false in
  for q = 0 to a.n - 1 do
    term.(q) <- a.term.(q)
  done;
  let delta = Array.make_matrix (a.n + 1) a.m a.n in
  for q = 0 to a.n - 1 do
    for x = 0 to a.m - 1 do
      let q' = a.delta.(q).(x) in
      if q' = -1 then delta.(q).(x) <- a.n
      else delta.(q).(x) <- q'
    done
  done;
  {n = a.n + 1; m = a.m; init = a.init; term; delta}

let auto_inter a1 a2 =
  let n = a1.n * a2.n in
  let m = min a1.m a2.m in
  let delta = Array.make_matrix n m (-1) in
  let f i j = i * a2.n + j in
  for i = 0 to a1.n - 1 do
    for j = 0 to a2.n - 1 do
      for x = 0 to m - 1 do
        let q = f i j in
        let q' = f a1.delta.(i).(x) a2.delta.(j).(x) in
        delta.(q).(x) <- q'
      done
    done;
  done;
  let init = f a1.init a2.init in
  let term = Array.make n false in
  for i = 0 to a1.n - 1 do
    for j = 0 to a2.n - 1 do
      term.(f i j) <- a1.term.(i) && a2.term.(j)
    done
  done;
  {n; m; delta; init; term}


let inclus a1 a2 =
  let a2c = complementaire a2 in
  let a1_inter_a2c = auto_inter a1 a2c in
  not (langage_non_vide a1_inter_a2c)

let equivalent a1 a2 =
  let a1_complet = complete a1 in
  let a2_complet = complete a2 in
  inclus a1_complet a2_complet && inclus a2_complet a1_complet


