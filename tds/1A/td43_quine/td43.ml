(*Algorithme de Quine*)


type formule =
  | C of bool
  | V of int
  | Et of formule * formule
  | Ou of formule * formule
  | Imp of formule * formule
  | Non of formule

exception Derniere

type decision =
  | Feuille of bool
  | Noeud of int * decision * decision

(* Graphe de Petersen : nombre chromatique égal à 3. *)
let petersen =
  [|
    [4; 5; 6];
    [6; 7; 8];
    [5; 8; 9];
    [4; 7; 9];
    [0; 3; 8];
    [0; 2; 7];
    [0; 1; 9];
    [1; 3; 5];
    [1; 2; 4];
    [2; 3; 6]
  |]

(* Générateur de graphe aléatoire.
 * graphe_alea n p génère un graphe à n sommet
 * dans lequel chaque arête possible a une probabilité p
 * d'être choisie (indépendamment des autres). *)

let graphe_alea n proba_arete =
  let g = Array.make n [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if Random.float 1. <= proba_arete then begin
        g.(i) <- j :: g.(i);
        g.(j) <- i :: g.(j)
      end
    done
  done;
  g

let rec taille formule = 
  match formule with
  |C _ | V _ -> 1
  |Non f -> 1 + taille f
  |Et(f1,f2) | Ou(f1,f2) | Imp(f1,f2) -> 1 + taille f1 + taille f2

let rec var_max formule =
  match formule with
  |C _ -> -1
  |V i -> i
  |Et(f1,f2) | Ou(f1,f2) | Imp(f1,f2) -> max (var_max f1) (var_max f2)
  |Non f -> var_max f


let rec evalue formule valuation = 
  match formule with
  |C b -> b
  |V i -> valuation.(i)
  |Et(f1,f2) -> (evalue f1 valuation) && (evalue f2 valuation)
  |Ou(f1,f2) -> (evalue f1 valuation) || (evalue f2 valuation)
  |Non f1 -> not (evalue f1 valuation)
  |Imp(f1,f2) -> (not (evalue f1 valuation)) || (evalue f2 valuation)


exception Derniere

let incremente_valuation valuation = 
    let i = ref 0 in 
    while !i >= 0 && valuation.(!i) do
        valuation.(!i) <- false;
        decr i;
    done;
    if !i >= 0 then valuation.(!i) <- true
    else raise Derniere


    exception NOT_SAT

let satisfiable_brute formule = 
    let n = taille formule in
    let valuation = Array.make n false in
    try
        while not (evalue formule valuation) do
            incremente_valuation valuation
        done;
        true
    with
    |Derniere -> false


let rec elimine_constantes formule = 
  match formule with
  |Et(f1,f2) -> begin 
                  match elimine_constantes f1, elimine_constantes f2 with
                  | C false, _ | _ , C false -> C false
                  | C true, e | e, C true -> e
                  | f,g -> Et(f,g)
  end
  |Ou(f1,f2) -> begin
                  match elimine_constantes f1, elimine_constantes f2 with
                  | C true, _| _, C true -> C true
                  | C false, e | e, C false -> e
                  |f,g -> Ou(f,g)
              end
  |Imp(f1,f2) -> begin 
                  match elimine_constantes f1,type formule =
                  | C of bool
                  | V of int
                  | Et of formule * formule
                  | Ou of formule * formule
                  | Imp of formule * formule
                  | Non of formule
                
    let rec taille f =
      match f with
      | C _ | V _ -> 1
      | Non f' -> 1 + taille f'
      | Et (f1, f2) | Ou (f1, f2) | Imp (f1, f2) -> 1 + taille f1 + taille f2
    
    let rec var_max f =
      match f with
      | C _ -> -1
      | V i -> i
      | Et (f1, f2) | Ou (f1, f2) | Imp (f1, f2) -> max (var_max f1) (var_max f2)
      | Non f' -> var_max f'
    
    
    let rec evalue formule valuation =
      match formule with
      | C b -> b
      | V i -> valuation.(i)
      | Et (f, g) -> (evalue f valuation) && (evalue g valuation )
      | Ou (f, g) -> (evalue f valuation) || (evalue g valuation)
      | Imp (f, g) -> (not (evalue f valuation)) || (evalue g valuation)
      | Non f -> not (evalue f valuation)
    
    exception Derniere
    
    let incremente_valuation valuation =
      let n = Array.length valuation in
      let i = ref (n - 1) in
      while !i >= 0 && valuation.(!i) do
        valuation.(!i) <- false;
        decr i
      done;
      if !i >= 0 then valuation.(!i) <- true
      else raise Derniere
    
    
    
    let satisfiable_brute formule =
      let n = var_max formule in
      let valuation = Array.make (n + 1) false in
      try
        while not (evalue formule valuation) do
          incremente_valuation valuation
        done;
        true
      with
      | Derniere -> false
    
    
    let rec elimine_constantes = function
      | Et (f, g) ->
        begin match elimine_constantes f, elimine_constantes g with
          | C false, _ | _, C false -> C false
          | C true, h | h, C true -> h
          | f', g' -> Et (f', g')
        end
      | Ou (f, g) ->
        begin match elimine_constantes f, elimine_constantes g with
          | C true, _ | _, C true -> C true
          | C false, h | h, C false -> h
          | f', g' -> Ou (f', g')
        end
      | Imp (f, g) ->
        begin match elimine_constantes f, elimine_constantes g with
          | C false, _ | _, C true -> C true
          | C true, h -> h
          | f', C false -> Non f'
          | f', g' -> Imp (f', g')
        end
      | Non f ->
        begin match elimine_constantes f with
          | C b -> C (not b)
          | f' -> Non f'
        end
      | f -> f
    
    let rec substitue f i g =
      match f with
      | V j when i = j -> g
      | Non f' -> Non (substitue f' i g)
      | Et (f1, f2) -> Et (substitue f1 i g, substitue f2 i g)
      | Ou (f1, f2) -> Ou (substitue f1 i g, substitue f2 i g)
      | Imp (f1, f2) -> Imp (substitue f1 i g, substitue f2 i g)
      | _ -> f
    
    type decision =
      | Feuille of bool
      | Noeud of int * decision * decision
    
    let rec min_var = function
      | C _ -> max_int
      | V i -> i
      | Et (f, g) | Ou (f, g) | Imp (f, g) ->
        min (min_var f) (min_var g)
      | Non f -> min_var f
    
    let rec construire_arbre formule =
      match elimine_constantes formule with
      | C b -> Feuille b
      | f ->
        let i = min_var f in
        let f_bot = substitue f i (C false) in
        let f_top = substitue f i (C true) in
        Noeud (i, construire_arbre f_bot, construire_arbre f_top)
    
    let rec satisfiable_via_arbre f =
      let rec aux = function
      | Feuille b -> b
      | Noeud (_, g, d) -> aux g || aux d in
      aux (construire_arbre f)
    
    
    let range n = List.init n (fun i -> i)
    
    let rec binarise_ou formules =
      match formules with
      | [] -> C false
      | [f] -> f (* pas indispensable *)
      | f :: fs -> Ou (f, binarise_ou fs)
    
    let rec binarise_et formules =
      match formules with
      | [] -> C true
      | [f] -> f (* pas indispensable *)
      | f :: fs -> Et (f, binarise_et fs)
    
    let encode g k =
      let n = Array.length g in
      let var i c = V (i * k + c) in
      let est_colorie i =
        binarise_ou (List.init k (fun c -> var i c)) in
      let contraintes i =
        let contraintes_couleur c =
          let voisins = List.map (fun j -> var j c) g.(i) in
          let autres_couleurs = List.filter (fun x -> x <> c) (range k) in
          let unique = List.map (fun c' -> var i c') autres_couleurs in
          Imp (var i c, Non (binarise_ou (voisins @ unique))) in
        Et (est_colorie i, binarise_et (List.init k contraintes_couleur)) in
      binarise_et (List.init n contraintes)
    
    
    let est_k_coloriable test_satisfiable g k =
      let formule = encode g k in
      Printf.printf "Test %d-coloriable\n%!" k;
      Printf.printf "Nombre de variables : %d\n%!" (1 + var_max formule);
      Printf.printf "Taille : %d\n%!" (taille formule);
      test_satisfiable formule
    
    let degre_max g =
      let rec aux i =
        if i = Array.length g then 0
        else max (List.length g.(i)) (aux (i + 1)) in
      aux 0
    
    let chromatique test_satisfiable g =
      let rec aux i j =
        if i = j then begin
          Printf.printf "\nchi(G) = %d\n%!" i;
          i
        end else begin
          Printf.printf "\n%d <= chi(G) <= %d\n%!" i j;
          let m = (i + j) / 2 in
          if est_k_coloriable test_satisfiable g m then aux i m
          else aux (m + 1) j
        end in
      let n = Array.length g in
      let dmax = degre_max g in
      let p = (Array.fold_left (fun acc u -> acc + List.length u) 0 g) / 2 in
      Printf.printf "%d sommets, %d arêtes, degré max %d\n%!" n p dmax;
      aux 1 (degre_max g + 1)
    
    let petersen =
      [|
        [4; 5; 6];
        [6; 7; 8];
        [5; 8; 9];
        [4; 7; 9];
        [0; 3; 8];
        [0; 2; 7];
        [0; 1; 9];
        [1; 3; 5];
        [1; 2; 4];
        [2; 3; 6]
      |]
    
    let graphe_alea n proba_arete =
      let g = Array.make n [] in
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          if Random.float 1. <= proba_arete then begin
            g.(i) <- j :: g.(i);
            g.(j) <- i :: g.(j)
          end
        done
      done;
      g
    
    
    
    
    let lire_dimacs filename =
      let ic = open_in filename in
      let rec get_next () =
        let s = input_line ic in
        if s.[0] = 'c' then get_next ()
        else s in
      let n, _ = Scanf.sscanf (get_next ()) "p edge %d %d" (fun n p -> (n, p)) in
      let g = Array.make n [] in
      try
        while true do
          let i, j =
            Scanf.sscanf
              (get_next ())
              "e %d %d"
              (fun i j -> (i - 1, j - 1)) in
          if not (List.mem j g.(i)) then g.(i) <- j :: g.(i);
          if not (List.mem i g.(j)) then g.(j) <- i :: g.(j)
        done;
        assert false
      with
      | End_of_file -> g
    
    
    
    
    let rec simplifie x b = function
      | V i when i = x -> C b
      | Et (f, g) ->
        begin match simplifie x b f, simplifie x b g with
          | C true, h | h, C true -> h
          | C false, _ | _, C false -> C false
          | f', g' -> Et (f', g')
        end
      | Ou (f, g) ->
        begin match simplifie x b f, simplifie x b g with
          | C true, _ | _, C true -> C true
          | C false, h | h, C false -> h
          | f', g' -> Ou (f', g')
        end
      | Imp (f, g) ->
        begin match simplifie x b f, simplifie x b g with
          | C false, _ | _, C true -> C true
          | C true, g' -> g'
          | f', C false -> Non f'
          | f', g' -> Imp (f', g')
        end
      | Non f ->
        begin match simplifie x b f with
          | Non f' -> f'
          | C b' -> C (not b')
          | f' -> Non f'
        end
      | f -> f
    
    let rec simplifie2 x b = function
      | V i when i = x -> C b
      | Et (f, g) ->
        begin match simplifie2 x b f with
          | C false -> C false
          | C true -> simplifie2 x b g
          | f' -> begin match simplifie2 x b g with
              | C false -> C false
              | C true -> f'
              | g' -> Et (f', g')
            end
        end
      | Ou (f, g) ->
        begin match simplifie2 x b f with
          | C true -> C true
          | C false -> simplifie2 x b g
          | f' -> begin match simplifie2 x b g with
              | C true -> C true
              | C false -> f'
              | g' -> Ou (f', g')
            end
        end
      | Imp (f, g) ->
        begin match simplifie2 x b f with
          | C false -> C true
          | C true -> simplifie2 x b g
          | f' -> begin match simplifie2 x b g with
              | C true -> C true
              | C false -> Non f'
              | g' -> Imp (f', g')
            end
        end
      | Non f ->
        begin match simplifie2 x b f with
          | Non f' -> f'
          | C b' -> C (not b')
          | f' -> Non f'
        end
      | f -> f
    
    
    
    let rec premiere_var = function
      | V i -> Some i
      | C _ -> None
      | Non f -> premiere_var f
      | Et (f, g) | Ou (f, g) | Imp (f, g) ->
        begin match premiere_var f with
          | Some i -> Some i
          | None -> premiere_var g
        end
    
    
    let satisfiable formule =
      let rec aux f =
        match f, premiere_var f with
        | C b, None -> b
        | _, None -> failwith "impossible"
        | _, Some i ->
          aux (simplifie2 i true f) || aux (simplifie2 i false f) in
      aux (elimine_constantes formule)
                
                
                
                
                let main () =
                  let g =
                    if Array.length Sys.argv > 1 then
                      let filename = Sys.argv.(1) in
                      lire_dimacs filename
                    else
                      graphe_alea 30 0.25 in
                  let c = chromatique satisfiable g in
                  Printf.printf "Le nombre chromatique vaut %d.\n" c
                
                let () =
                  Printexc.record_backtrace true;
                  main ()
                 elimine_constantes f2 with
                  |C false, _ | _, C true -> C true
                  |C true, e -> e
                  |f, C false -> Non f
                  |f,g -> Imp(f,g)
              end
  |Non f1 -> begin 
                  match elimine_constantes f1 with
                  | C a -> C (not a)
                  |f -> Non f end
  |f -> f



let rec substitue f g i = 
  match f with
  |V k when i = k -> g
  |Non f1 -> Non (substitue f1 g i)
  |Et(f1,f2) -> Et(substitue f1 g i, substitue f2 g i)
  |Ou(f1,f2) -> Ou(substitue f1 g i,substitue f2 g i)
  |Imp(f1,f2) -> Imp(substitue f1 g i,substitue f2 g i)
  | _ -> f


type decision = 
|Feuille of bool
|Noeud of int * decision * decision

let rec var_min formule =
  match formule with
  |C _ -> max_int
  |V i -> i
  |Et(f1,f2) | Ou(f1,f2) | Imp(f1,f2) -> max (var_min f1) (var_min f2)
  |Non f -> var_min f

let rec construire_arbre formule = 
  match elimine_constantes formule with
  |C b -> Feuille b
  |f -> 
      let i = var_min f in
      let f_false = substitue f (C false) i in
      let f_true = substitue f (C true) i in
  Noeud (i,construire_arbre f_true,construire_arbre f_false)


let satisfiable_via_arbre formule = 
  let rec aux f = 
      match f with
      |Feuille b -> b
      |Noeud(i,f1,f2) -> (aux f1) || (aux f2)
  in aux (construire_arbre formule)

type graphe = int list array

let rec binarise_et formules = 
  match formules with
  |[] -> C true
  |f :: reste -> Et(f,binarise_et reste)

let rec binarise_ou formules = 
  match formules with
  |[] -> C false
  |f :: reste -> Ou(f,binarise_ou reste)
  
let range i = 
  List.init i (fun k -> k)

let encode g k = 
  let range_k = range k in
  let n = Array.length g in
  let var i c = V (i * k + c) in
  let est_colorie i = binarise_ou (List.init k (fun c -> var i c)) in (*sommet colorié d'au moins une couleur*)
  let appliquer_contraintes i =
      let rec contraintes_sur_couleurs c = 
          let voisins = List.map (fun l -> var l c) g.(i) in (*récupère voisins du sommet*)
          let couleurs_differentes = List.filter (fun x -> x <> c) range_k in (*vérifie si les couleurs des voisins sont toutes différentes*)
          let couleur_unique = List.map (fun c' -> var i c') couleurs_differentes in
          Imp(var i c,Non (binarise_ou (voisins @ couleur_unique))) in
          Et(est_colorie i, binarise_et (List.init k contraintes_sur_couleurs))
  in binarise_et (List.init n appliquer_contraintes)


let est_k_coloriable g k = 
  let formule = encode g k in
  satisfiable_via_arbre formule


let degre_max g =
  let maxi = ref 0 in
  let nb_noeuds = Array.length g in 
  for i = 0 to nb_noeuds - 1 do
      let len = List.length g.(i) in 
      maxi := max !maxi len
  done;
  !maxi
    
let chromatique graphe = 
    let rec aux s e = 
        if s = e then begin Printf.printf "chi(G) = %d" s;
        s
        end 
        else begin 
            let mid = (s + e)/2 in
            if est_k_coloriable graphe mid then aux s mid
            else aux (mid + 1) e  end in 
    let deg_max = degre_max graphe in
    aux 1 (deg_max + 1)


let lire_dimacs filename = 
  let input_file = open_in filename in
  let rec read_next_line () =
      let line = input_line input_file in
      if line.[0] ='c' then read_next_line ()
      else line in
  let nb_v,_ =  Scanf.sscanf (read_next_line ()) "p edge %d %d" (fun n p -> (n,p)) in 
  let g = Array.make nb_v [] in
  try
      while true do
      let i,j = Scanf.sscanf (read_next_line ()) "e %d %d" (fun i j -> (i - 1,j - 1)) in
      if not (List.mem j g.(i)) then g.(i) <- j :: g.(i);
      if not (List.mem i g.(j)) then g.(j) <- i :: g.(j)
  done;
      assert false
  with
      |End_of_file -> g


let rec simplifie i boolean formule =
  match formule with
  |V elt when elt = i -> C boolean
  |Et(f1,f2) -> begin 
                  match simplifie i boolean f1,simplifie i boolean f2 with
                  | C false, _ | _ , C false -> C false
                  | C true, e | e, C true -> e
                  | f,g -> Et(f,g)
  end
  |Ou(f1,f2) -> begin
                  match simplifie i boolean f1,simplifie i boolean f2 with
                  | C true, _| _, C true -> C true
                  | C false, e | e, C false -> e
                  |f,g -> Ou(f,g)
              end
  |Imp(f1,f2) -> begin 
                  match simplifie i boolean f1,simplifie i boolean f2 with
                  |C false, _ | _, C true -> C true
                  |C true, e -> e
                  |f, C false -> Non f
                  |f,g -> Imp(f,g)
              end
  |Non f1 -> begin 
                  match simplifie i boolean f1 with
                  | C a -> C (not a)
                  |f -> Non f end
  |f -> f