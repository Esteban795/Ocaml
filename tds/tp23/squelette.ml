(************)
(* Partie 1 *)
(************)

type dict =
  | V
  | N of char * dict * dict


let mots = ["diane"; "dire"; "diva"; "divan"; "divin"; "do"; "dodo";
            "dodu"; "don"; "donc"; "dont"; "ame"; "ames"; "amen"]

let d_mots =
  N ('a',
     N ('m',
        N ('e',
           N ('n',
              N ('$', V, V),
              N ('s',
                 N ('$', V, V),
                 N ('$', V, V))),
           V),
        V),
     N ('d',
        N ('o',
           N ('n',
              N ('t',
                 N ('$', V, V),
                 N ('c',
                    N ('$', V, V),
                    N ('$', V, V))),
              N ('d',
                 N ('u',
                    N ('$', V, V),
                    N ('o',
                       N ('$', V, V),
                       V)),
                 N ('$', V, V))),
           N ('i',
              N ('v',
                 N ('i',
                    N ('n',
                       N ('$', V, V),
                       V),
                    N ('a',
                       N ('n',
                          N ('$', V, V),
                          N ('$', V, V)),
                       V)),
                 N ('r',
                    N ('e',
                       N ('$', V, V),
                       V),
                    N ('a',
                       N ('n',
                          N ('e',
                             N ('$', V, V),
                             V),
                          V),
                       V))),
              V)),
        V))

let exemple =
  ["ame"; "ames"; "amen"; "amer"; "ami"; "amis";
   "amie"; "amies"; "ane"; "anes"; "annee"; "annees";
   "anti"; "avide"; "mais"; "misa"; "misas"; "mesa";
   "same"; "tian"; "tina"; "nain"; "nina"; "isthme";
   "medisant"; "ultime"; "magique"; "essai"; "est"; "qui"]

(* Exercice 1 *)

let rec est_bien_forme t = 
      match t with
      |V -> true
      |N('$',V,d) -> est_bien_forme d 
      |N(_,V,_) | N('$',_,_) -> false
      |N(c,g,d) -> est_bien_forme g && est_bien_forme d;;


(************)
(* Partie 2 *)
(************)

(* Exercice 3 *)
type mot = char list 

let mot_of_string s = 
   let l = String.length s in
   let rec aux i =
      if i = l then ['$']
      else s.[i] :: aux (i + 1)
   in aux 0

let rec afficher mot = 
   match mot with
   | [] -> print_newline ()
   | '$' :: xs -> afficher xs 
   | x :: xs -> print_char x; afficher xs 



(************)
(* Partie 3 *)
(************)

(* Exercice 4 *)

let rec cardinal t = 
   match t with
   |V -> 0
   |N('$',V,d) -> 1 + cardinal d 
   |N(c,g,d) -> cardinal g + cardinal d

let teste_cardinal () =
  assert (cardinal d_mots = 14);
  print_endline "Test ok"

let rec appartient dict mot = 
   match dict,mot with
   | V,[] -> true
   |N(c,g,d), x :: xs -> if c = x then appartient g xs else appartient d mot 
   |_ -> false

let appartient_string dict s =
  appartient dict (mot_of_string s)




let teste_appartient () =
  let f s = assert (appartient_string d_mots s) in
  let g s = assert (not (appartient_string d_mots s)) in
  List.iter f mots;
  g "amee";
  g "";
  g "am";
  g "amena";
  g "amen$";
  print_endline "Test ok"


(* Exercice 5 *)

let rec ajouter dict mot = 
   match dict,mot with
   |V,[] -> V 
   |V, x :: xs -> N(x,ajouter V xs,V)
   |N(c,g,d), x :: xs -> if c = x then N(c,ajouter g xs,d) else N(c,g,ajouter d mot)
   |_ -> failwith "c'est raté"

let rec dict_of_list u = 
   match u with 
   |[] -> V
   | s :: tl -> ajouter (dict_of_list tl) (mot_of_string s)

let teste_dict_of_list () =
  let d = dict_of_list mots in
  assert (cardinal d = 14);
  List.iter (fun s -> assert (appartient_string d s)) mots;
  let d_ex = dict_of_list exemple in
  assert (cardinal d_ex = 30);
  List.iter (fun s -> assert (appartient_string d_ex s)) exemple;
  print_endline "Test ok"

  let () = 
   teste_cardinal();
   teste_appartient();
   teste_dict_of_list();;

(* Exercice 6 *)

let afficher_mots dict = 
   let rec aux t prefixe = 
      match t with
      |V -> ()
      |N('$',V,d) -> afficher (List.rev prefixe); afficher_mots d prefixe
      |N(c,g,d) -> aux g (c :: prefixe); aux d prefixe in
   aux dict []

let rec longueur_maximale dict = 
   match dict with 
   | V -> -1
   |N(c,g,d) -> 1 + max (longueur_maximale g) (longueur_maximale d)

let afficher_mots_longs dict n = 
   let rec aux t prefixe len = 
      match t with
      |V -> ()
      |N('$',V,d) -> if len >= n then afficher (List.rev prefixe); afficher_mots d prefixe len
      |N(c,g,d) -> aux g (c :: prefixe) (len + 1); aux d prefixe in
   aux dict [] 0 

(************)
(* Partie 4 *)
(************)

(* Exercice 7 *)

let lire_fichier f = 
   let file = open_in f in
   let dict = ref V in
   try 
      while true do
         let line = input_line file in 
         dict := ajouter !dict (mot_of_string line)
      done;
   with
   |End_of_file -> !dict


(************)
(* Partie 5 *)
(************)

(* Exercice 8 *)

let calculer_occurrences s = 
   let t = Array.make 256 0 in
   let len = String.length s in
   for i = 0 to l-1 do
      t.(int_of_char s.[i]) <- t.(int_of_char s.[i]) + 1
   done;
   t

let afficher_mots_contenus dict s = 
   let occs = calculer_occurrences s in
   let rec aux dict prefixe = 
      match dict with
      | V -> ()
      |N('$',g,d) -> afficher (List.rev prefixe); aux d prefixe 
      |N(c,g,d) -> 
         aux d prefixe;
         let i = int_of_char c in
         if occs.(i) > 0 then 
            occs.(i) <- occs.(i) - 1; 
            aux g (c :: prefixe) occs;
            occs.(i) <- occs.(i) + 1; 
   in aux dict []


let check t = 
   let len = Array.length t in
   let i = ref 0 in
   while !i < n && t.(!i) = 0 do
      incr i;
   done;
   !i = n

let afficher_anagrammes dict s = 
   let occs = calculer_occurrences s in
   let rec aux dict prefixe = 
      match dict with
      | V -> ()
      |N('$',g,d) -> 
         if check t then afficher (List.rev prefixe); aux d prefixe 
      |N(c,g,d) -> 
         aux d prefixe;
         let i = int_of_char c in
         if occs.(i) > 0 then 
            occs.(i) <- occs.(i) - 1; 
            aux g (c :: prefixe) occs;
            occs.(i) <- occs.(i) + 1; 
   in aux dict []

(* Exercice 9 *)

let filtrer_mots_contenus dict s = 
   let occs = calculer_occurrences s in
   let rec aux d = 
      match d with
      |V -> V 
      |N('$',V,d) -> N('$',g,aux)


let filtrer_mots_contenant dict s = failwith "à implémenter"

let filtrer_anagrammes dict s = failwith "à implémenter"


(************)
(* Partie 6 *)
(************)

(* Exercice 10 *)

let afficher_decompositions dict mot = failwith "à implémenter"

let decompose_anagrammes dict mot = failwith "à implémenter"

(* Exercice 11 *)

let decompose_anagrammes_unique dict mot = failwith "à implémenter"
