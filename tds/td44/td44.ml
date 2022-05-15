type graphe =
  {nb_sommets : int;
   voisins : int -> int list}

exception Trouve of int array


let adj_g0 =
  [|
    [1];
    [0; 2; 7];
    [1; 3; 5];
    [2; 4; 6];
    [3];
    [2; 8];
    [3; 7; 8];
    [1; 6];
    [5; 6]
  |]

let voisins_sommet g x = 
    g.(x)
    
let generateur_graphe adj = 
    {nb_sommets = Array.length adj_g0;voisins = voisins_sommet  adj}
    
let g0 = generateur_graphe adj_g0


let hamiltonien_depuis g x0 = 
  let ordre = Array.make g.nb_sommets (-1) in
  let voisins_libres x = 
      List.filter (fun sommet -> ordre.(sommet) = -1) (g.voisins x ) in
  let rec explorer k x =
      ordre.(x) <- k;
      if k = g.nb_sommets - 1 then raise (Trouve ordre)
      else List.iter (explorer (k + 1)) (voisins_libres x);
      ordre.(x) <- -1
  in
  try
      explorer 0 x0;
      None
  with
  |Trouve arr -> Some arr


let deltas = [|(2,1);(2,-1);(-2,1);(-2,-1);(1,2);(-1,2);(1,-2);(-1,-2)|]

let graphe_cavalier n m =
    let index i j = i * m + j in
    let coordonnees sommet = (sommet / m, sommet mod m) in
    let obtenir_voisins sommet=  
        let i,j = coordonnees sommet in
        let indices_valides x y = 
            0 <= x && x < n && 0 <= y && y < m in
        let lst = ref [] in
        for k = 0 to 7 do
            let dx,dy = deltas.(k) in 
            if indices_valides (i + dx) (j + dy) then lst := (index (i + dx) (j + dy)) :: !lst
        done;
        !lst in
    {nb_sommets = n * m; voisins = obtenir_voisins}

let g = graphe_cavalier 5 6


let affiche_parcours_cavalier n m (x,y) =
  let num_sommet = x * m + y in
  match hamiltonien_depuis (graphe_cavalier n m) num_sommet with
  |None -> Printf.printf "Pas possible"
  |Some ordre -> 
      for i = 0 to n - 1 do
          for j = 0 to m - 1 do
              Printf.printf "%3d " ordre.(i * m + j);
          done;
          print_newline ()
      done


let hamiltonien_opt_depuis g x0 =
  let ordre = Array.make g.nb_sommets (-1) in
  let voisins_libres x = 
      List.filter (fun sommet -> ordre.(sommet) = -1) (g.voisins x ) in
  let nb_voisins_libres x =
      List.length (voisins_libres x) in
  let nb_noeuds_explores = ref 0 in
  let rec explorer k x =
      incr nb_noeuds_explores;
      ordre.(x) <- k;
      if k = g.nb_sommets - 1 then raise (Trouve ordre)
      else let tri_voisins = 
          List.sort (fun k y -> nb_voisins_libres k - nb_voisins_libres y) (voisins_libres x)
      in List.iter (explorer (k + 1)) tri_voisins;
      ordre.(x) <- -1
  in
  try
      explorer 0 x0;
      Printf.printf "%d\n !" !nb_noeuds_explores;
      None
  with
  |Trouve arr -> Printf.printf "%d\n !" !nb_noeuds_explores; Some arr


type 'a reponse = 
|Refus
|Accepte of 'a
|Partiel of 'a

type 'a probleme = 
  {accepte: 'a -> 'a reponse;
  enfants: 'a -> 'a list;
  initiale :'a}

let enumere probleme =
  let rec backtrack candidat = 
      match probleme.accepte candidat with
      |Refus -> []
      |Accepte sol -> [sol]
      |Partiel sol -> 
          let rec aux enfants =
              match enfants with
              |[] -> []
              |c :: cs -> backtrack c @ aux cs in
          aux (probleme.enfants sol) 
  in backtrack probleme.initiale


let occs tab nb_elts = 
  let occs = Array.make nb_elts 0 in
  for i = 0 to nb_elts - 1 do 
      occs.(tab.(i)) <- occs.(tab.(i)) + 1;
  done;
  occs

let enfants n tab = 
    let f i = Array.append tab [|i|] in
    List.init n f
    
let accepte_auto n t =
    if Array.length t = n then
        let occs = occs t n in
        if occs = t then Accepte t
        else Refus
    else Partiel t

let autoreferent_brute n =
    {accepte = accepte_auto n;
    enfants = enfants n ;
    initiale = [| |]}


let g = autoreferent_brute 10