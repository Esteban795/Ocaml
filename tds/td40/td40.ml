(* On ouvre le module Printf, ce qui permet d'écrire sprintf plutôt
 * que Printf.sprintf *)
 open Printf

 type formule =
   | Const of bool
   | Var of string
   | Et of formule * formule
   | Ou of formule * formule
   | Non of formule
 
 
 let antinomie =
   Et (Non (Var "x"),
       Et (Ou (Et (Var "z", Var "x"),
               Var "y"),
           Et (Non (Var "y"),
               Non (Var "z")
              )
          )
      )
 
 let ex1 =
   Et (Var "x",
       Et (Var "y",
           Et (Non (Var "z"),
               Var "t")
          )
      )
 
 let ex2 =
   Et (
     Et (Var "x",
         Ou (Non (Var "y"),
             Ou (Var "z",
                 Non (Var "x"))
            )
        ),
     Non (Var "t")
   )
 
 let ex3 =
   Et (
     Non (Var "t"),
     Et (Ou (Ou (Var "z",
                 Non (Var "x")),
             Non (Var "y")),
         Var "x")
   )
 
 let gros_ex1 =
   Ou (antinomie,
       Et (ex1,
           Et (ex2, ex2)
          )
      )
 
 let gros_ex2 =
   Ou (Et (Et (ex3, ex1),
           ex2),
       antinomie)
 
 let gros_ex3 =
   Ou (Et (Non ex1, Ou (Var "u", Var "x")), Ou (gros_ex1, gros_ex2))
 
 (* On pourra utiliser sprintf, qui se comporte comme printf sauf
  * qu'elle renvoie une chaîne au lieu de l'afficher. *)
 
 let priorite = function
   | Var _ | Const _ -> max_int
   | Ou _ -> 0
   | Et _ -> 1
   | Non _ -> 2
 
 
 
 (**********************)
 (* Egalite syntaxique *)
 (**********************)
 
 (* Version artisanale *)
 
 
 (* Mise en forme canonique *)
 
 type formule_asso =
   | C of bool
   | V of string
   | EtA of formule_asso list
   | OuA of formule_asso list
   | N of formule_asso
 
 (* Fonctions préliminaires *)
 
 
 
 (* Égalité syntaxique *)

 let rec string_of_formule formule = 
  match formule with
  |Const b -> sprintf "%b" b
  |Var y -> y
  |Et (f1,f2) -> sprintf "(%s et %s)" (string_of_formule f1) (string_of_formule f2)
  |Ou (f1,f2) -> sprintf "(%s ou %s)" (string_of_formule f1) (string_of_formule f2)
  |Non f1-> sprintf "Non (%s)" (string_of_formule f1)



let priorite = function
  |Var _ | Const _ -> max_int
  |Ou _ -> 0
  |Et _ -> 1
  |Non _ -> 2

let rec string_priorite formule = 
  let rec aux f prio_parent = 
      if priorite f >= prio_parent then
      string_priorite f
      else sprintf "(%s)" (string_priorite f)
  in
  match formule with
  |Const b -> sprintf "%b" b
  |Var y -> y
  |Et (f1,f2) -> sprintf "%s et %s" (aux f1 1) (aux f2 1)
  |Ou (f1,f2) -> sprintf "%s ou %s" (aux f1 0) (aux f2 0)
  |Non f1-> sprintf "Non %s" (aux f1 2)
    

let rec egal_com f1 f2 = 
  match f1,f2 with
  |Non x, Non y -> egal_com x y
  |Var x,Var y -> x = y
  |Const x, Const y -> x = y
  |Et(f1,f2), Et(f1',f2') | Ou(f1,f2), Ou(f1',f2') -> egal_com f1 f1' && egal_com f2 f2' || egal_com f1 f2' && egal_com f2 f1'
  |_ -> false


type formule_asso = 
  |C of bool
  |V of string
  |EtA of formule_asso list
  |OuA of formule_asso list
  |N of formule_asso

let rec insere  
  match v with 
  |[] -> [x]
  |h :: t when h < x -> h :: (insere t x)
  |h :: t ->  x :: h :: t

let rec fusionne u v = 
  match u,v with
  | [],_ -> v
  | _,[] -> u
  |x :: xs,y :: ys -> if x <= y then x :: fusionne xs v else y :: fusionne u ys


let rec canonique formule = 
  match formule with
  |Const b -> C b
  |Var x -> V x
  |Non e -> N (canonique e)
  |Et(f1,f2) -> begin
      let f1',f2' = canonique f1, canonique f2 in
      match f1',f2' with
      |EtA enfants_f1', EtA enfants_f2' -> EtA (fusionne enfants_f1' enfants_f2')
      |EtA enfants_f1', _ -> EtA (insere f2' enfants_f1')
      |_, EtA enfants_f2' -> EtA (insere f1' enfants_f2')
      |_,_ -> EtA (fusionne [f1'] [f2']) end
  |Ou(f1,f2) ->
      begin
      let f1',f2' = canonique f1, canonique f2 in
      match f1',f2' with
      |OuA enfants_f1', OuA enfants_f2' -> OuA (fusionne enfants_f1' enfants_f2')
      |OuA enfants_f1', _ -> OuA (insere f2' enfants_f1')
      |_, OuA enfants_f2' -> OuA (insere f1' enfants_f2')
      |_,_ -> OuA (fusionne [f1'] [f2'])
  end


let egal_syntaxe f1 f2 = 
  canonique f1 = canonique f2