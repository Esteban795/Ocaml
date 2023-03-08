(*
COCKE-YOUNGER-KASAMI algorithm

Pseudo-code (Q6) 

---------------------------------------------------------------------------------------------------------------

fonction CYK(g,w)
  si w = mot_vide : renvoyer S -> mot_vide

  n = longueur du mot 
  k = nombre de variables

  t = tableau (n + 1) * n * k init à false 

  pour chaque règle Xi -> a faire 
    pour d = 0 à n - 1 faire
      si w[d] = a alors t[1,d,i] = true 


  pour l = 2 à n faire 
    pour d = 0 à n - l faire
     pour l' = 0 à l - 1 faire 
      pour chaque règle de la forme X_i -> X_jX_k faire 
        t[l,d,i] <- t[l,d,i] || ( t[l',d,i] && t[l - l', d + l',i])
  
  renvoyer t[n,0,g.initial]

--------------------------------------------------------------------------------------------------------------
*)

type regle_unitaire = int * char
type regle_binaire = int * int * int

type cnf = {
  initial :int;
  nb_variables : int;
  unitaires : regle_unitaire list;
  binaires : regle_binaire list;
  mot_vide : bool
}

type arbre = 
  |Unaire of int * char 
  |Binaire of int * arbre * arbre


let g0 = {
 initial = 0;
 nb_variables = 5;
 unitaires = [(0,'b');(1,'a');(2, 'b');(4,'a')];
 binaires = [(0,1,2); (0,2,1); (0,3,1); (1,1,4); (3,1,2)];
 mot_vide = false
}

let cyk_reconnait (g : cnf) (s : string) = 
  if s = "" then g.mot_vide (* don't waste time *)
  else
  let n = String.length s in
  let k = g.nb_variables in 
  let t = Array.make_matrix (n + 1) n [||] in 

  for i = 0 to n do 
    for j = 0 to n - 1 do 
      t.(i).(j) <- Array.make k false
    done;
  done;

  
  for d = 0 to n - 1 do
    List.iter (fun (i, c) -> if c = s.[d] then t.(1).(d).(i) <- true) g.unitaires
  done;

  for l = 2 to n do 
    for d = 0 to n - 1 do
      for l' = 0 to l - 1 do
        if d + l' < n then List.iter (fun (a,b,c) -> t.(l).(d).(a) <- t.(l).(d).(a) || (t.(l').(d).(b) && t.(l - l').(d + l').(c))) g.binaires
      done;
    done;
  done;
  t.(n).(0).(g.initial)

exception No_tree

let cyk_analyse (g : cnf) (s : string) = 
  if s = "" then raise No_tree (* else it breaks the code *)
  else
  let n = String.length s in
  let k = g.nb_variables in 
  let t = Array.make_matrix (n + 1) n [||] in 

  for i = 0 to n do 
    for j = 0 to n - 1 do 
      t.(i).(j) <- Array.make k None;
    done;
  done;

  
  for d = 0 to n - 1 do
    List.iter (fun (i, c) -> if c = s.[d] then t.(1).(d).(i) <- Some (Unaire (i,c))) g.unitaires
  done;

  for l = 2 to n do 
    for d = 0 to n - l do
      for l' = 0 to l - 1 do
        let traiter (a,b,c) = 
          match t.(l).(d).(a), t.(l').(d).(b), t.(l - l').(d + l').(c) with
          | None , Some i, Some j ->
              t.(l).(d).(a) <- Some (Binaire (a,i,j))
          | _ -> ()
        in 
        List.iter traiter g.binaires
      done;
    done;
  done;
  
  match t.(n).(0).(g.initial) with 
  | None -> raise No_tree
  | Some x -> x


let cyk_compte (g : cnf) (s : string) = 
  let n = String.length s in
  let k = g.nb_variables in 
  let t = Array.make_matrix (n + 1) n [||] in 

  for i = 0 to n do 
    for j = 0 to n - 1 do 
      t.(i).(j) <- Array.make k 0;
    done;
  done;

  let traiter_unitaire (i,c) = 
    for j = 0 to n - 1 do 
      if s.[j] = c then t.(1).(j).(i) <- 1
    done;
  in
  List.iter traiter_unitaire g.unitaires;

  for l = 2 to n do 
    for d = 0 to n - l do
      for l' = 0 to l - 1 do
        let traiter_binaire (i,j,k) = 
          t.(l).(d).(i) <- t.(l).(d).(i) + (t.(l').(d).(j) * t.(l - l').(d + l').(k))
        in
        List.iter traiter_binaire g.binaires
      done;
    done;
  done;
  if n = 0 && g.mot_vide then 1
  else  if n = 0 then 0 
  else t.(n).(0).(g.initial)


(*
Objectif : pouvoir mettre n'importe quelle grammaire sous forme normale de Chomsky   
*)


type symbole = 
|T of char
|V of int

type regle = int * symbole list

type grammaire = {
  nb_variables : int;
  regles : regle list;
  initial : int;
}


let g1 = {
  nb_variables = 3;
  initial = 0;
  regles = [(0,[T 'a'; V 0 ; T 'b']); (0,[T 'a'; V 1 ; T 'b']); (1,[V 2; V 1]) ; (1,[]); (2,[T 'a']); (2,[T 'b'])];
}


(*On ajoute juste la règle initiale, sans se préoccuper si elle existe déjà ou pas*)
let start g = {
  nb_variables = g.nb_variables + 1;
  regles = (g.nb_variables,[V g.initial]) :: g.regles;
  initial = g.nb_variables
}
  
let term g =
  let next_available = ref g.nb_variables in (*On sait combien de variables on a en tout*)
  let tab = Array.make 256 (-1) in
  
  
  (*
  On applique la transfo 'term' du cours. 
  On mappe toutes les lettres qui existent dans notre mot sur de nouvelles variables fraîches   
  *)
  let rec recup_variables_fraiches mot =
    match mot with 
    | [] -> []
    | V i :: xs -> (V i) :: recup_variables_fraiches xs
    | T c :: xs ->
      let indice_tab = int_of_char c in
      if tab.(indice_tab) = -1 then begin
        tab.(indice_tab) <- !next_available;
        incr next_available 
      end;
      V tab.(indice_tab) :: recup_variables_fraiches xs
  in 

  (*
    Pour les règles de la forme 
    X -> truc
    
    on crée les variables correspondant aux terminaux présents 
    et on remplace l'occurence de chaque terminal par la variable 
    nouvellement créée.

  *)
  let transforme_regle (v,mot) = 
    if List.length mot <= 1 then (v,mot)
    else (v,recup_variables_fraiches mot)
  in 


  let regles_bis = ref (List.map transforme_regle g.regles) in 
  for i = 0 to 255 do 
    if tab.(i) <> -1 then 
      regles_bis := (tab.(i),[T (char_of_int i)]) :: !regles_bis
  done;

  {
    nb_variables = !next_available;
    regles = !regles_bis;
    initial = g.initial
  }


(*
On applique la transformation 'bin' du cours.

Dans binarise, on transforme toutes les règles de la forme

A -> X_1 ... X_k

par 

A -> X_1 A_1
A_i -> X_i+1 A_i+1

*)
let bin g = 
  let next_available = ref g.nb_variables in (*compter le nombre de variables totales*)

  (*
  On a X -> X_1 ... X_k
  et on renvoie 
  
  X -> X_1 Y_1 
  Y_1 -> X_2 Y_2
  Y_2 -> X_3 Y_3 
  .etc  (Y_i sont des variables qu'on a crée par nous-mêmes)
  *)
  let rec binarise (v,droite) = 
    match droite with 
    |[] | [ _ ] | [_ ; _] -> [(v,droite)] (*on a soit rien, soit une règle qui donne juste A -> X.. On l'éliminera à l'étape UNIT d'après*)
    | a :: reste ->
      let nouvelle_variable = !next_available in 
      let nouvelle_regle = (v,[a ; V nouvelle_variable]) in 
      incr next_available;
      nouvelle_regle :: (binarise (nouvelle_variable, reste))
  in 

  let rec traiter_regles l = 
    match l with 
    | [] -> []
    | r :: ls -> binarise r @ traiter_regles ls
  in 

  let regles' = 
    traiter_regles g.regles in 
    {
      nb_variables = !next_available;
      regles = regles';
      initial = g.initial
    }

(*
On applique la transfo 'del' du cours. 

On commence par calculer les variables annulables
(Une variable est annulable si on a X -> epsilon ou X -> YZ avec Y et Z annulables)


*)
let del (g : grammaire) =

  (* On cherche les variables annulables *)
  let annulables = Array.make g.nb_variables false in
  let switch = ref false in 
  let traite_regle (v,droite) =
    let rec aux dr = 
      match dr with 
      | [] -> switch := true; annulables.(v) <- true; (* On arrive à Epsilon, donc c'est bien annulable *)
      | V x :: reste when annulables.(x) -> aux reste; (* On sait déjà qu'on a une variable annulable, donc il nous reste à vérifier le reste*)
      | _ -> () (* me les brise le linter avec le pattern matching pas exhaustif *)
    in 
    if not annulables.(v) then aux droite 
  in
  while !switch do 
    switch := false;
    List.iter traite_regle g.regles
  done;

  let rec traite_regles regles = 
    match regles with 
    | [] -> [] 
    | (v,[]) :: ls -> traite_regles ls (*on efface toutes les règles de la forme X -> epsilon*)
    | (v,[x]) :: ls -> (v,[x]) :: traite_regles ls (* On touche pas aux règles X -> a, où a est terminal*)
    | (v,[x ; y]) :: ls ->
      let temp = ref [] in 
      let ajouter_autre_si_annulable a b = 
        match a with 
        | V a' when annulables.(a') -> temp := (v,[b]) :: !temp
        | _ -> () (*pattern matching pas exhaustif*)
      in 
      ajouter_autre_si_annulable x y;
      ajouter_autre_si_annulable y x;
      !temp @ (v,[x ;y]) :: traite_regles ls
    | _ -> failwith "binarise abruti"
        
  in
  let regles_bis = 
    if annulables.(g.initial) then (g.initial,[]) :: traite_regles g.regles
    else traite_regles g.regles
  in 
  {
    nb_variables = g.nb_variables;
    regles = regles_bis;
    initial = g.initial
  }

let _ =
  let word = "" in 
  Printf.printf "%s\n" word;
  if cyk_reconnait g0 word then Printf.printf "Oui\n" else Printf.printf "Non\n"
        