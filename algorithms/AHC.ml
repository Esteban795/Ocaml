
let dist_squared a b = 
  let x1 = fst a in 
  let y1 = snd a in 
  let x2 = fst b in 
  let y2 = snd b in 
  (x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1)

let delta_simple c1 c2 = 
  let min_dist = ref (dist_squared (List.hd c1) (List.hd c2)) in 
  let rec aux elt lst = 
    match lst with 
    |[] -> ()
    | x :: xs -> 
      let d = dist_squared x elt in 
      if !min_dist > d then (
        min_dist := d;
        aux elt xs
      ) 
      else aux elt xs
  in 
  List.iter (fun i -> aux i c2) c1;
  !min_dist



let proximite (classes : (float * float) list array) = 
  let n = Array.length classes in
  let proximity_m = Array.make_matrix n n 0. in 
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      proximity_m.(i).(j) <- delta_simple classes.(i) classes.(j)
    done;
  done;
  proximity_m

let classes_plus_proches tab =
  let n = Array.length tab in 
  let m = proximite tab in 
  let min_dist = ref m.(0).(0) in 
  let min_c1 = ref 0 in 
  let min_c2 = ref 0 in 
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      if m.(i).(j) < !min_dist then (
        min_dist := m.(i).(j);
        min_c1 := i;
        min_c2 := j;
      )
    done;
  done;
  (!min_c1,!min_c2,!min_dist)


let ahc tab seuil = 
  failwith "a faire"
