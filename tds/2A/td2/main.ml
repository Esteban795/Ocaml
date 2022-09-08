

(* La fonction parse prend en entrée une string et renvoie la regex
 * correspondante.
 * Epsilon est représenté par le caractère & et Vide par le caractère #.
 * On a les règles de priorité usuelles, et les espaces sont ignorés.
 *)

let parse string =
  let open Printf in
  let s = Stream.of_string string in
  let rec peek () =
    match Stream.peek s with
    | Some ' ' -> Stream.junk s; peek ()
    | Some c -> Some c
    | None -> None in
  let eat x =
    match peek () with
    | Some y when y = x -> Stream.junk s; ()
    | Some y -> failwith (sprintf "expected %c, got %c" x y)
    | None -> failwith "incomplete" in
  let rec regex () =
    let t = term () in
    match peek () with
    | Some '|' -> eat '|'; Somme (t, regex ())
    | _ -> t
  and term () =
    let f = factor () in
    match peek () with
    | None | Some ')' | Some '|' -> f
    | _ -> Produit (f, term ())
 and factor () =
    let rec aux acc =
      match peek () with
      | Some '*' -> eat '*'; aux (Etoile acc)
      | _ -> acc in
    aux (base ())
  and base () =
    match peek () with
    | Some '(' -> eat '('; let r = regex () in eat ')'; r
    | Some '&' -> eat '&'; Epsilon
    | Some '#' -> eat '#'; Vide
    | Some (')' | '|' | '*' as c) -> failwith (sprintf "unexpected '%c'" c)
    | Some c -> eat c; Lettre c
    | None -> failwith "unexpected end of string" in
  let r = regex () in
  try Stream.empty s; r
  with _ -> failwith "trailing ')' ?"

type partie_principale =
  | L of char
  | E of partie_principale
  | S of partie_principale * partie_principale
  | P of partie_principale * partie_principale

type forme_normale =
  | V
  | Eps
  | R of partie_principale
  | PlusEps of partie_principale


type regex =
  | Vide
  | Epsilon
  | Lettre of char
  | Somme of regex * regex
  | Produit of regex * regex
  | Etoile of regex

let rec string_of_regex regex = 
  match regex with
  |Vide -> "#"
  |Epsilon -> "&"
  |Lettre a -> String.make 1 a
  |Somme (s1,s2) -> "(" ^ string_of_regex s1 ^ "|" ^ string_of_regex s2 ^ ")"
  |Produit (f1,f2) -> "(" ^ string_of_regex f1 ^  string_of_regex f2 ^ ")"
  |Etoile e' -> string_of_regex e' ^ "*"


let rec est_vide regex = 
  match regex with
  |Vide -> true
  |Etoile _ | Epsilon | Lettre _ -> false
  |Somme (s1,s2) -> est_vide s1 && est_vide s2
  |Produit (f1,f2) -> est_vide f1 || est_vide f2
  
let rec un_mot e = 
  match e with
  |Vide -> None
  |Epsilon | Etoile _ -> Some ""
  |Lettre l -> Some (String.make 1 l)
  |Somme (s1,s2) -> 
    begin match un_mot s1 with
      |None -> un_mot s2
      |Some str -> Some str
    end
  |Produit (f1,f2) ->
    begin match un_mot f1, un_mot f2 with
      |Some str1,Some str2 -> Some (str1 ^ str2)
      |_ -> None
    end


exception EstVide

    
let rec un_mot_exc reg = 
  match reg with 
  |Vide -> raise EstVide
  |Epsilon | Etoile _ -> ""
  |Lettre l -> String.make 1 l
  |Produit (f1,f2) -> un_mot_exc f1 ^ un_mot_exc f2
  |Somme (s1,s2) ->
    (try 
      un_mot_exc s1
    with
    |EstVide -> un_mot_exc s2)


let rec extrait_vide reg = 
  match reg with
  |Vide -> Vide
  |Epsilon -> Epsilon
  |Lettre c -> Lettre c
  |Etoile e ->
    begin match extrait_vide e with
    |Vide -> Epsilon
    |e' -> Etoile e'
    end
  |Produit (f1,f2) -> 
    begin match extrait_vide f1, extrait_vide f2 with 
    |Vide, _ | _, Vide -> Vide
    |f,f' -> Produit(f,f')
  end
  |Somme (s1,s2) ->
    begin match extrait_vide s1, extrait_vide s2 with
      |Vide,f | f,Vide -> f
      |f,f' -> Somme(f,f')
    end