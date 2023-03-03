type regle_unitaire = int * char
type regle_binaire = int * int * int

type cnf = {
  initial :int;
  nb_variables : int;
  unitaires : regle_unitaire list;
  binaires : regle_binaire list;
  mot_vide : bool
}

let g0 = {
 initial = 0;
 nb_variables = 5;
 unitaires = [(0,'b');(1,'a');(2, 'b');(4,'a')];
 binaires = [(0,1,2); (0,2,1); (0,3,1); (1,1,4); (3,1,2)];
 mot_vide = false
}

let cyk_reconnait g s = 
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


let _ =
  let word = "abba" in 
  Printf.printf "%s\n" word;
  if cyk_reconnait g0 word then Printf.printf "Oui\n" else Printf.printf "Non\n"
        