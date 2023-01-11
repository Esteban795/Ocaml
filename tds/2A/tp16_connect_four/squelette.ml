open Printf

let nb_lignes = 6
let nb_colonnes = 7


type position = {
  grille : float array array;
  hauteurs : int array;
  mutable dernier : int;
  mutable nb_joues : int;
  mutable code : int;
}

let creer_initiale () = {
  grille = Array.make_matrix nb_lignes nb_colonnes 0.;
  hauteurs = Array.make nb_colonnes 0;
  nb_joues = 0;
  dernier = -1;
  code = 0;
}

let affiche position =
  let open Printf in
  let bordure = String.make (nb_colonnes + 2) '_' in
  printf " ";
  print_endline bordure;
  for ligne = nb_lignes - 1 downto 0 do
    printf "%d|" ligne;
    for col = 0 to nb_colonnes - 1 do
      let x = position.grille.(ligne).(col) in
      if x = 1. then printf "X"
      else if x = -1. then printf "O"
      else printf " "
    done;
    printf "|\n"
  done;
  printf " ";
  print_endline (String.make (nb_colonnes + 2) '-');
  printf "  ";
  for col = 0 to nb_colonnes - 1 do printf "%d" col done;
  print_newline ()

let joueur_courant position =
  if position.nb_joues mod 2 = 0 then 1.
  else -1.


let en_jeu i j =
  0 <= i && i < nb_lignes && 0 <= j && j < nb_colonnes

let compte_consecutifs position ligne col (di, dj) =
  let nb = ref 0 in
  let g = position.grille in
  let joueur = g.(ligne).(col) in
  let i = ref ligne in
  let j = ref col in
  while en_jeu !i !j && g.(!i).(!j) = joueur do
    incr nb;
    i := !i + di;
    j := !j + dj
  done;
  i := ligne - di;
  j := col - dj;
  while en_jeu !i !j && g.(!i).(!j) = joueur do
    incr nb;
    i := !i - di;
    j := !j - dj
  done;
  !nb

let tab_valeurs =
  [|
    [| 3.; 4.; 5.; 7.; 5.; 4.; 3.|];
    [| 4.; 6.; 7.; 10.; 7.; 6.; 4.|];
    [| 5.; 8.; 11.; 13.; 11.; 8.; 5.|];
    [| 5.; 8.; 11.; 13.; 11.; 8.; 5.|];
    [| 4.; 6.; 7.; 10.; 7.; 6.; 4.|];
    [| 3.; 4.; 5.; 7.; 5.; 4.; 3.|];
  |]

let coup_possibles position = 
  let lst = [] in 
  for i = 0 to nb_colonnes - 1 do 
    if hauteurs.(i) < nb_lignes then i :: lst;
  done;
  lst;

let joue position coup = 
  assert (not (hauteurs.(coup) < nb_lignes));
  let player = joueur_courant position in
  let jeton = if player = 1 then -1 else 1 in
  position.grille.(position.hauteurs.(coup)).(coup) <- jeton;
  position.dernier = coup;
  position.hauteurs.(coup) = position.hauteurs.(coup) + 1;
  position.nb_joues = position.nb_joues + 1;

let restore position coup = 
  let last = position.dernier in 
  position.grille.(position.hauteurs.(last)).(last) <- 0;
  position.dernier <- coup;
  position.hauteurs.(last) <- position.hauteurs.(last) - 1;

let directions = [|(0,1);(1,0);(1,1);(1,-1)|]

let perdant position = 
  let last = position.dernier in 
  let lgn = position.hauteurs.(last) - 1;
  let maxi = ref 0;
  for i = 0 to 3 do
    maxi := max maxi (compte_consecutifs position lgn last directions.(i))
  done;
  maxi >= 4;

let strat_alea pos = 
  let legal_moves = coups_possibles coup;
  let tab = Array.of_list legal_moves in 
  let random = Random.int (Array.length tab) in 
  tab.(random);

let strat_humain () =
  Printf.printf "Bonjour humain, où veux-jouer ?";
  read_int ()

let joue_partie strat1 strat2 =
  let pos = creer_initiale () in
  while not (perdant pos) && List.length (coups_possibles pos) > 0 do 
    let player = joueur_courant pos;
    let coup = if j = 1 then s2 position else s1 position in 
    joue pos coup;
    affiche pos
  done;
  Printf.printf "Le joueur %d est naze" (joueur_courant pos);

let heuristique_basique pos = 
  let player = joueur_courant pos in 
  let sum = ref 0. in 
  for i = 0 to nb_lignes - 1 do
    for j = 0 to nb_colonnes - 1 do 
      sum := !sum + tab_valeurs.(i).(j) * pos.grille.(i).(j) 
    done;
  done;
  player * !sum

let rec negamax heuristique prof pos = 
  let player = joueur_courant position in 
  if perdant position then (-1,-infinity)
  if prof = 0 then (-1,player,heuristique position);
  else 
    let rec eval moves_possibles max_coup max_eval = 
      match moves_possibles with
      |[] -> (max_coup,max_eval)
      |x :: xs ->
        let dernier = position.dernier in
        joue position coup;
        let (a,x) = negamax heuristique (prof - 1) position in 
        let res = -x in
        restore position dernier;
        if res = infinity then (coup,infinity)
        else if res >= max_eval then aux xs max_coup res
        else aux xs max_coup max_eval in
    aux (coups_possibles position) -1 (-infinity)


let main () = 
  Printexc.record_backtrace true;
  printf "à écrire\n"

(* let () = main () *)
