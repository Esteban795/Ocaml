type 'a stack = 
  {donnees:'a option array;mutable courant:int}

let capacite s = Array.length s.donnees


let new_stack n = {donnees=Array.make n None;courant=0}

let pop stack = 
  pile.courant <- pile.courant - 1;
  let temp = pile.donnees.(pile.courant + 1) in
  if temp = None then None else temp


let push elt stack = 
  if stack.courant + 1 > capacite stack
  then failwith "Pile pleine"
  else stack.courant <- stack.courant + 1;
      stack.donnees.(stack.courant) <- elt