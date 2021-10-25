type 'a stack = 
  {donnees:'a option array;mutable courant:int}

let capacite s = Array.length s.donnees


let new_stack n = {donnees=Array.make n None;courant=0}

let pop stack = 
    stack.courant <- stack.courant - 1;
    let temp = stack.donnees.(stack.courant) in stack.donnees.(stack.courant) <- None;
    (temp)

let push elt stack =
    if stack.courant >= capacite stack
    then failwith "Pile pleine."
    else stack.donnees.(stack.courant) <- Some elt;stack.courant <- stack.courant + 1