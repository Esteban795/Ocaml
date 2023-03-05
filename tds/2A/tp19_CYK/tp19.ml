(*

COCKE-YOUNGER-KASAMI algorithm

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


let _ =
  let word = "" in 
  Printf.printf "%s\n" word;
  if cyk_reconnait g0 word then Printf.printf "Oui\n" else Printf.printf "Non\n"
        