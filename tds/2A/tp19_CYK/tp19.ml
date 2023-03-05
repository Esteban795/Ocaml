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
  if s = "" then true (*else it breaks the code *)
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

  let result = ref false in 
  for i = 0 to k - 1 do
    result := !result || t.(n).(0).(i)
  done;
  !result

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


let _ =
  let word = "" in 
  Printf.printf "%s\n" word;
  if cyk_reconnait g0 word then Printf.printf "Oui\n" else Printf.printf "Non\n"
        