type multigraphe = {
  parents : int array;
  degrees : int array;
  adj_m : int array array;
  mutable nb_aretes : int
}


let g_ex =
  [|
   [| 0; 1; 1; 2; 0 |];
   [| 1; 0; 1; 1; 0 |];
   [| 1; 1; 0; 1; 1 |];
   [| 2; 1; 1; 0; 2 |];
   [| 0; 0; 1; 2; 0 |];
  |]


let create_partition n =
  Array.init n (fun i -> i)

let rec find_parent part i = 
  if part.parents.(i) = i then i
  else find_parent part part.parents.(i)

let merge part i j = 
  let ri = find_parent part i in 
  let rj = find_parent part j in 
  if ri <> rj then part.parents.(rj) <- ri

let random_edge g = 
  let r = ref (Random.int g.nb_aretes) in
  let sommet = ref 0 in 
  while g.degrees.(!sommet) < !r do
    r := !r - g.degrees.(!sommet);
    incr sommet;     (*On se décale jusqu'à trouver le bon sommet*)
  done;
  let edge = ref 0 in 
  while g.adj_m.(!sommet).(!edge) <= !r do (* On se décale jusqu'à trouver la bonne arrête *)
    r := !r - g.adj_m.(!sommet).(!edge);
    incr edge
  done;
  (!sommet,!edge)

let contract (g : multigraphe) (s1 : int) (s2 : int) = 
  merge g s1 s2;

  g.degrees.(s1) <- g.degrees.(s1) + g.degrees.(s2);
  g.degrees.(s2) <- 0;

  g.nb_aretes <- g.nb_aretes - 2 * g.adj_m.(s1).(s2); (* Chaque arête était comptée deux fois *)

  g.adj_m.(s1).(s2) <- 0;
  g.adj_m.(s2).(s1) <- 0;

  for i = 0 to Array.length g.adj_m - 1 do 
    g.adj_m.(i).(s1) <- g.adj_m.(i).(s1) + g.adj_m.(i).(s2);
    g.adj_m.(i).(s2) <- 0;
    g.adj_m.(s1).(i) <- g.adj_m.(s1).(i) + g.adj_m.(s2).(i);
    g.adj_m.(s2).(i) <- 0
  done


let graph_to_multigraphe g = 
  let n = Array.length g in
  let adj_m = Array.init n (fun i -> Array.copy g.(i)) in 
  let degrees = Array.make n 0 in 
  let parents = create_partition n in 
  let nb_edges = ref 0 in 
  for s1 = 0 to n - 1 do
    for s2 = 0 to n - 1 do 
      degrees.(s1) <- degrees.(s1) + g.(s1).(s2);
      nb_edges := !nb_edges + g.(s1).(s2)
    done;
  done;
  {
    adj_m = adj_m;
    nb_aretes = !nb_edges;
    degrees = degrees;
    parents = parents;
  }

let krager (g : int array array) = 
  let n = Array.length g in 
  let mg = graph_to_multigraphe g in 
  for i = 0 to n - 3 do 
    let s1,s2 = random_edge mg in 
    contract mg s1 s2
  done;
  mg


let _ = 
  krager g_ex;