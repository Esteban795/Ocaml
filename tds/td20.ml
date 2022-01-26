type op =
  | Plus
  | Fois
  | Moins

type expr =
  | C of int
  | N of op * expr * expr

let exemple = 
  N(Plus,N(Fois,C(4),N(Moins,C(8),C(9))),N(Plus,C(6),C(7)))
    
let exemple2 = N(Plus,C(2),N(Fois,C(3),C(4)))

let exemple3 = N(Fois,N(Plus,C(3),C(2)),C(4))

let applique op x y =
  match op with
  |Plus -> x + y
  |Moins -> x - y
  |Fois -> x * y

let rec eval arbre = 
  match arbre with
  |C(x) -> x
  | N(op,g,d) -> applique op (eval g) (eval d)

type lexeme = PO | PF | Op of op | Val of int

let rec prefixe arbre = 
  match arbre with
  |C(x) -> [Val x]
  |N(op,g,d) -> Op op :: prefixe g @ prefixe d

let rec postfixe arbre = 
  match arbre with
  |C(x) -> [Val x]
  |N(op,g,d) -> postfixe g @ postfixe d @ [Op op]


let rec infixe arbre = 
  match arbre with
  |C(x) -> [Val x]
  |N(op,g,d) -> infixe g @ [Op op] @ infixe d

let eval_post u = 
  let rec aux expre pile = 
      match expre,pile with
      | [],[elt] -> elt
      |Val x :: xs,_ -> aux xs (x :: pile)
      |Op op :: xs, droite :: gauche :: r -> aux xs ((applique op gauche droite) :: r)
      |_,_ -> failwith "raté"
  in aux u []

let arbre_of_post expr = 
  let rec aux expr pile = 
    match expr, pile with
    |[], [x] -> x
    |Val x :: xs, _ -> aux xs (C x :: pile)
    |Op op :: xs, dr :: ga :: reste_pile -> aux xs (N(op, ga, dr) :: reste_pile)
    | _ -> failwith "expression incorrecte" 
  in aux expr []
