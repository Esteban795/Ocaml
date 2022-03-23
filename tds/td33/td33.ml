type point = {x : float; y : float}

let distance p1 p2 = 
  sqrt((p1.x -. p2.x)**2. +. (p1.y -. p2.y)**2.)


let nuage nb_points borne = 
  let t = Array.make nb_points {x=0.;y=0.} in
  for i = 0 to nb_points - 1 do
      let x = Random.float borne in
      let y = Random.float borne in
      t.(i) <-{x=x;y=y}
  done;
  t

let tab = nuage 5 100.

let dmin_naif arr = 
  let dist_min = ref infinity in
  let len = Array.length arr in
  for i = 0 to len - 1 do
      for j = i + 1 to len - 1 do
          dist_min := min !dist_min (distance arr.(i) arr.(j))
      done;
  done;
  !dist_min


let separe_moitie lst = 
  let rec aux v l acc =
      match v with
      |[] -> (List.rev acc,[])
      |x :: xs ->
      if l = 0 then List.rev acc,v 
      else aux xs (l - 1) (x :: acc)
  in aux lst ((List.length lst)/2) []


let compare_x p1 p2 = 
  if p1.x < p2.x then  -1
  else if p1.x > p2.x then 1
  else 0

let tri_par_x l = List.sort compare_x l


let dmin_dc_naif points = 
  let rec aux nuage = 
      match nuage with
      |[] | [_] -> infinity
      |[a;b] -> distance a b
      |_ ->
          let gauche,droite = separe_moitie nuage in
          let rec_gauche = aux gauche in
          let rec_droite = aux droite in
          let d_rec_gauche = dmin_gauche_droite gauche droite in
          min d_rec_gauche (min rec_gauche rec_droite)
  in 
  let temp = tri_par_x  points in 
  aux temp
  
let compare_y p1 p2 = 
  if p1.y < p2.y then  -1
  else if p1.y > p2.y then 1
  else 0
  
let tri_par_y l = List.sort compare_y l

let dist_min_7_prochains points_par_y =
  let d_min = ref infinity in
  let len = Array.length points_par_y in
  for i = 0 to len - 1 do
      for j = i + 1 to min (len - 1) (i + 7) do
          d_min := min !d_min  (distance points_par_y.(i) points_par_y.(j))
      done;
  done;
  !d_min


let dmin_dc points = 
  let rec aux nuage = 
      match nuage with
      |[] | [_] -> infinity
      |[a;b] -> distance a b
      |_ ->
          let gauche,droite = separe_moitie nuage in
          let mediane = List.hd droite in
          let dist_gauche = aux gauche in
          let dist_droite = aux droite in
          let d = min dist_gauche dist_droite in
          let filtre_a_garder p = 
              mediane.x -. d <= p.x && p.x <= mediane.x +. d
          in 
          let intervalle = List.filter filtre_a_garder nuage in
          let tri_par_y = Array.of_list (tri_par_y intervalle)
          in min d (dist_min_7_prochains tri_par_y)
  in 
  let temp = tri_par_x  points in 
  aux temp