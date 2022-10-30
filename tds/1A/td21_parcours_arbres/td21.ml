type ('a, 'b) arbre =
    |Interne of 'a * ('a, 'b) arbre * ('a, 'b) arbre
    | Feuille of 'b

let exemple1 =
  Interne (12,
           Interne (4,
                    Interne (7, Feuille 20, Feuille 30),

                    Interne (14, Feuille 1, Feuille 2)),
           Feuille 20)

val exemple1 arbre =
  Interne (12,
   Interne (4, Interne (7, Feuille 20, Feuille 30),
    Interne (14, Feuille 1, Feuille 2)),
   Feuille 20)

let exemple2 = 
    Interne(4,
        Feuille 0.3,
        Interne(1,
            Interne(8,
                Interne (2,Feuille 2.5,Feuille 3.1),
                Feuille 4.1),
            Feuille 0.2))

val exemple2 arbre =
  Interne (4, Feuille 0.3,
   Interne (1,
    Interne (8, Interne (2, Feuille 2.5, Feuille 3.1), Feuille 4.1),
    Feuille 0.2))

let rec hauteur arbre =
    match arbre with
    | Feuille v -> 0
    | Interne(v,g,d) -> 1 + max (hauteur g) (hauteur d)

let rec taille arbre = 
    match arbre with
    | Feuille v -> 1
    |Interne(v,g,d) -> 1 + taille g + taille d

let rec dernier arbre = 
    match arbre with
    | Feuille v -> v
    | Interne(v,g,d) -> dernier d

let rec affiche_prefixe arbre = 
    match arbre with
    | Feuille v -> print_newline (print_int v)
    |Interne(v,g,d) -> print_newline (print_int v); affiche_prefixe g;affiche_prefixe d

let rec affiche_infixe arbre = 
    match arbre with
    | Feuille v -> print_newline (print_int v)
    |Interne(v,g,d) -> affiche_infixe g; print_newline (print_int v);affiche_infixe d

let rec affiche_postfixe arbre =
    match arbre with
    | Feuille v -> print_newline (print_int v)
    |Interne(v,g,d) -> affiche_postfixe g; affiche_postfixe d;print_newline (print_int v)

type ('a, 'b) token = 
  | N of 'a
  | F of 'b

type ('a, 'b) token = N of 'a | F of 'b

let rec postfixe_naif arbre = 
    match arbre with
    | Feuille v -> [F v]
    | Interne(v,g,d) -> postfixe_naif g @ postfixe_naif d @ [N v]

(*Cette fonction risque d'être inefficace car...

Des concaténations en série de coût N*)

let postfixe arbre = 
    let rec aux arbre valeurs = 
        match arbre with
        | Feuille v -> (F v) :: valeurs
        | Interne(v,g,d) -> aux g (aux d ((N v :: valeurs)))
    in aux arbre []
(*O(n)*)

let prefixe arbre = 
    let rec aux arbre valeurs = 
        match arbre with
        | Feuille v -> (F v) :: valeurs
        | Interne(v,g,d) -> (N v) :: aux g (aux d valeurs)
    in aux arbre []

let infixe arbre = 
    let rec aux arbre valeurs = 
        match arbre with
        | Feuille v -> (F v) :: valeurs
        | Interne(v,g,d) -> aux g ((N v) :: aux d valeurs)
    in aux arbre []

let postfixe_term arbre = 
    let rec aux foret valeurs = 
        match foret with
        | [] -> valeurs
        | Feuille v :: reste_foret -> aux reste_foret (F v :: valeurs)
        | Interne(v,g,d) :: reste_foret -> aux (d :: g :: reste_foret) (N v:: valeurs)
    in aux [arbre] []

(* 

   Le module Queue de la bibliothèque standard fournit des files impératives.

   Les fonctions utiles ici sont :

   - création d'une nouvelle file vide : 

     Queue.create : unit -> 'a Queue.t 

   - tester si une file est vide :

     Queue.is_empty : 'a Queue.t -> bool

   - extraire un élément : 

     Queue.pop : 'a Queue.t -> 'a

   - ajouter un élément :

     Queue.push : 'a -> 'a Queue.t -> unit

*)

let largeur arbre = 
    let queue = Queue.create () in
    Queue.push arbre queue;
    let valeurs = ref [] in
    while not (Queue.is_empty queue) do
        match Queue.pop queue with
        |Feuille v -> valeurs := F v :: !valeurs
        |Interne(v,g,d) -> valeurs := N v :: !valeurs; Queue.push g queue; Queue.push d queue
    done;
    List.rev !valeurs

let rec lire_etiquette adresse arbre =
  match adresse,arbre with
  | [], Interne(v,g,d) -> N v
  |[],Feuille v -> F v
  |adresse :: reste_adresses, Interne(v,g,d) -> 
      if adresse then lire_etiquette reste_adresses d else lire_etiquette reste_adresses g
  |_ -> failwith "mauvaise adresse"

let rec incremente arbre adresse =
  match arbre,adresse with
  |Feuille v, [] -> Feuille (v + 1)
  |Interne(v,g,d),[] -> Interne(v + 1,g,d)
  |Interne(v,g,d), adresse :: reste_adresses -> 
      if adresse then incremente d reste_adresses else incremente g reste_adresses
  |_ -> failwith "mauvaise adresse"

let affiche_avec_adresse (x, adresse) =
  List.iter (fun b -> print_int (if b then 1 else 0)) adresse;
  Printf.printf " : %i\n" x

let tableau_adresses arbre =
  let rec aux temp arbre = 
      match arbre with
      | Feuille v -> affiche_avec_adresse (v,List.rev temp)
      | Interne(v,g,d) ->
          affiche_avec_adresse (v,List.rev temp);
          aux (false :: temp) g;
          aux (true :: temp) d
    in aux [] arbre;
    print_newline ()


let lire_postfixe parcours = 
    let rec aux pile p = 
        match pile,p with
        |_, F x :: xs -> aux (Feuille x :: pile) xs
        |d :: g :: reste_pile, N x :: xs -> aux ( Interne (x,g,d) :: reste_pile) xs
        |[arbre], [] -> arbre
        |_ -> failwith "mauvais parcours"
    in aux [] parcours

let lire_prefixe parcours = 
    let rec aux pile p = 
        match pile,p with
        |_, F x :: xs -> aux (Feuille x :: pile) xs
        |g :: d :: reste_pile, N x :: xs -> aux ( Interne (x,g,d) :: reste_pile) xs
        |[arbre], [] -> arbre
        |_ -> failwith "mauvais parcours"
    in aux [] (List.rev parcours)