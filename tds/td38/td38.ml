type sommet = int
type graphe = sommet list array

let dfs pre post g i = 
  let len = Array.length g in 
  let vus = Array.make len 0 in
  let rec aux sommet = 
      if vus.(sommet) = 0 then begin
          vus.(sommet) <- 1;
          pre sommet;
          List.iter aux g.(sommet);
          post sommet;
      end in
  aux i
          

let ouvre x = print_newline (Printf.printf "Ouverture %d\n" x)
let ferme x = print_newline (Printf.printf "Fermeture %d\n" x)

let g0 =
  [| [1; 2];
     [2; 3; 4];
     [];
     [0; 5];
     [1; 2];
     [10];
     [1; 9];
     [8];
     [6];
     [7; 10];
     [11];
     [5] |]


let g0_miroir =
  [|[3];
    [6; 4; 0];
    [4; 1; 0];
    [1];
    [1];
    [11; 3];
    [8];
    [9];
    [7];
    [6];
    [9; 5];
    [10]|]

let g1 = 
  [| [1; 4];
     [0; 2; 4; 7];
     [1; 5];
     [6; 8];
     [0; 1];
     [2; 7; 8];
     [3; 8];
     [1; 5; 8];
     [3; 5; 6; 7];
     [] |]

let g2 =
  [| [1; 2];
     [0; 2];
     [0; 1];
     [7];
     [8];
     [8];
     [];
     [3];
     [4; 5] |]

let g3 =
  [| [1; 2; 3];
     [0; 3];
     [0];
     [0; 1] |]


let bfs traitement g s = 
let len = Array.length g in
let vus = Array.make len 0 in 
let ouverts = Queue.create () in
let ajoute sommet = 
    if vus.(sommet) = 0 then begin
        traitement sommet;
        vus.(sommet) <- 1;
        Queue.push sommet ouverts 
    end in
ajoute s;
while not (Queue.is_empty ouverts) do
    let elt = Queue.pop ouverts in
    List.iter ajoute g.(elt)
done;


let bfs_frontiere traitement g s = 
  let len = Array.length g in
  let vus = Array.make len 0 in 
  vus.(s) <- 1;
  let rec ajoute sommets_voisins nouveaux =
      match sommets_voisins with
      |[] -> nouveaux
      |x :: xs ->
          if vus.(x) = 1 then ajoute xs nouveaux
          else begin
              vus.(x) <- 1;
              traitement x;
              ajoute xs (x :: nouveaux)
  end in
  let rec boucle frontiere nouveaux = 
      match frontiere,nouveaux with
      |[],[] -> ()
      |[],_ -> boucle nouveaux []
      |x :: xs, _ -> boucle xs (ajoute g.(x) nouveaux)
  in boucle [s] []


let accessible g x y = 
  let len = Array.length g in
  let vus = Array.make len 0 in 
  let rec explore u =
      if vus.(u) = 0 then begin
          vus.(u) <- 1;
          List.iter explore g.(u)
      end in
  explore x;
  vus.(y)


exception Found

let accessible_fast g x y = 
    let len = Array.length g in
    let vus = Array.make len 0 in 
    let rec explore u =
        if u = y then raise Found;
        if vus.(u) = 0 then begin
            vus.(u) <- 1;
            List.iter explore g.(u)
        end in
    try 
        explore x;
        false
    with
        |Found -> true


let tab_composantes g = 
  let len = Array.length g in
  let tab = Array.make len (-1) in
  let rec placer i sommet = 
      if tab.(sommet) = -1 then begin
          tab.(sommet) <- i;
          List.iter (placer i) g.(sommet)
      end in
  for i = 0 to len - 1 do
      placer i i
  done;
  tab


let liste_composantes g =
  let len = Array.length g in 
  let vus = Array.make len false in 
  let liste_compo = ref [] in
  let compo_actuelle = ref [] in
  let rec placer sommet = 
      if not vus.(sommet) then begin
          vus.(sommet) <- true;
          compo_actuelle := sommet :: !compo_actuelle;
          List.iter placer g.(sommet)
  end in
  for k = 0 to len - 1 do
      if not vus.(k) then begin
      compo_actuelle := [];
      placer k;
      liste_compo := !compo_actuelle :: !liste_compo
      end
  done;
  !liste_compo


let arbre_dfs g x0 = 
  let len = Array.length g in
  let t = Array.make len (-1) in 
  let rec explore i = 
      let verif v = 
          if t.(v) = -1 then (t.(v) <- i;explore v) in
      List.iter verif g.(i)
  in
  t.(x0) <- x0;
  explore x0;
  t


let arbre_bfs g x0 = 
  let len = Array.length g in
  let t = Array.make len (-1) in
  let queue = Queue.create () in 
  let ajoute pere sommet = 
      if t.(sommet) = -1 then begin
          t.(sommet) <- pere;
          Queue.push sommet queue
  end in
  ajoute x0 x0;
  while not (Queue.is_empty queue) do
      let elt = Queue.pop queue in
      List.iter (ajoute elt) g.(elt)
  done;
  t


let chemin parent i = 
  let rec aux i acc = 
      if parent.(i) = i then i :: acc
      else aux parent.(i) (i :: acc) 
      in
  if parent.(i) = -1 then None 
  else Some (aux i [])