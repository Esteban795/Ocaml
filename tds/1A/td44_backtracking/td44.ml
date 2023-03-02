type graphe =
  {nb_sommets : int;
   voisins : int -> int list}

exception Trouve of int array

let hamiltonien_depuis g x0 =
  let ordre = Array.make g.nb_sommets (-1) in
  let voisins_libres x =
    List.filter (fun y -> ordre.(y) = -1) (g.voisins x) in
  let rec explore k x =
    ordre.(x) <- k;
    if k = g.nb_sommets - 1 then raise (Trouve ordre);
    List.iter (explore (k + 1)) (voisins_libres x);
    ordre.(x) <- -1 in
  try
    explore 0 x0;
    None
  with
  | Trouve _ -> Some ordre


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

let g0 =
  let voisins i = adj_g0.(i) in
  {nb_sommets = Array.length adj_g0; voisins = voisins}

let hamiltonien_opt_depuis g x0 =
  let ordre = Array.make g.nb_sommets (-1) in
  let voisins_libres x =
    List.filter (fun y -> ordre.(y) = -1) (g.voisins x) in
  let nb_voisins_libres x =
    List.length (voisins_libres x) in
  let compteur = ref 0 in
  let rec explore k x =
    incr compteur;
    ordre.(x) <- k;
    if k = g.nb_sommets - 1 then raise (Trouve ordre);
    let voisins_tries =
      List.sort
        (fun y z -> nb_voisins_libres y - nb_voisins_libres z)
        (voisins_libres x) in
    List.iter (explore (k + 1)) voisins_tries;
    ordre.(x) <- -1 in
  try
    explore 0 x0;
    Printf.printf "%d\n%!" !compteur;
    None
  with
  | Trouve _ ->
    Printf.printf "%d\n%!" !compteur;
    Some ordre

let graphe_cavalier n m =
  let indice i j = i * m + j in
  let coord s = (s / m, s mod m) in
  let voisins s =
    let i, j = coord s in
    let ok x y =
      0 <= x && x < n && 0 <= y && y < m in
    let u = ref [] in
    let deltas = [(1, 2); (-1, 2); (1, -2); (-1, -2)] in
    let ajoute (dx, dy) =
      if ok (i + dx) (j + dy) then u := indice (i + dx) (j + dy) :: !u;
      if ok (i + dy) (j + dx) then u := indice (i + dy) (j + dx) :: !u in
    List.iter ajoute deltas;
    !u in
  {nb_sommets = n * m; voisins = voisins}

let affiche_parcours_cavalier n m (x, y) =
  let i = x * m + y in
  match hamiltonien_opt_depuis (graphe_cavalier n m) i with
  | None -> Printf.printf "Pas de parcours.\n"
  | Some ordre ->
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        Printf.printf "%3d " ordre.(i * m + j)
      done;
      print_newline ()
    done

type 'a reponse =
  | Refus
  | Accepte of 'a
  | Partiel of 'a

type 'a probleme =
  {accepte : 'a -> 'a reponse;
   enfants : 'a -> 'a list;
   initiale : 'a}

let enumere probleme =
  let rec backtrack candidat =
    match probleme.accepte candidat with
    | Refus -> []
    | Accepte solution -> [solution]
    | Partiel c' ->
      let rec aux enfants =
        match enfants with
        | [] -> []
        | e :: es -> backtrack e @ aux es in
      aux (probleme.enfants c') in
  backtrack probleme.initiale

let occurrences t n =
  let occs = Array.make n 0 in
  Array.iter (fun x -> occs.(x) <- occs.(x) + 1) t;
  occs

let enfants_auto n t =
  let f i = Array.append t [| i |] in
  List.init n f

(* Première version en pure force brute (pas d'élagage). *)

let accepte_auto n t =
  if Array.length t = n then
    let occs = occurrences t n in
    if occs = t then Accepte t
    else Refus
  else Partiel t

let autoreferent_brute n =
  {accepte = accepte_auto n;
   enfants = enfants_auto n ;
   initiale = [| |]}

let auto_referents n =
  enumere (autoreferent_brute n)


(* Pour accélérer la recherche, il faut élaguer l'arbre (repérer le plus
   rapidement possible qu'on se trouve dans une branche ne pouvant donner
   de solution).
   On utilise les remarques suivantes :
   - à la fin, la somme doit faire n. Si on dépasse n, c'est donc perdu.
   - c'est également perdu si l'on est sûr qu'on dépassera n plus tard.
   - s'il y a déjà plus d'occurrences d'une valeur i que le contenu de
     t.(i), c'est également perdu (le contenu de t.(i) ne changera plus,
     le nombre d'occurrences ne peut que croître).
   - inversement, si par exemple t.(3) vaut 5 et qu'il n'y a qu'un seul
     3 dans le tableau pour l'instant, alors il faut réserver 4 cases
     futures pour y mettre des 3. Si ce n'est pas possible, c'est
     perdu.
   - On pourrait encore clairement améliorer, mais cela suffit pour se
     convaincre qu'il existe une unique solution (sauf pour certaines
     petites valeurs de n). Reste à le prouver...
 *)
let accepte_auto_bis n t =
  let exception Echec in
  let k = Array.length t in
  if n = k then accepte_auto n t
  else
    try
      let somme = Array.fold_left (+) 0 t in
      if somme > n then raise Echec;
      let occs = occurrences t n in
      if k > 0 && somme + (n - k) - (t.(0) - occs.(0)) > n then raise Echec;
      let dispo = ref (n - k) in
      for i = 0 to k - 1 do
        dispo := !dispo - t.(i) + occs.(i);
        if occs.(i) > t.(i) || !dispo < 0 then raise Echec;
      done;
      Partiel t
    with
      Echec -> Refus



let autoreferent_opt n =
  {enfants = enfants_auto n ;
   accepte = accepte_auto_bis n ;
   initiale = [| |]}

let auto_referents_opt n =
  enumere (autoreferent_opt n)