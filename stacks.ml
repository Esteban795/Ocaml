type 'a stack = 
  {donnees:'a option array;mutable courant:int;vide:'a option array}

let capacite s = Array.length s.donnees


let new_stack n = {donnees=Array.make n None;courant=0;vide=Array.make n None}

let pop stack = 
    if stack.courant = 0 then failwith "Queue is empty."
    else
    stack.courant <- stack.courant - 1;
    let temp = stack.donnees.(stack.courant) in stack.donnees.(stack.courant) <- None;
    (temp)

let push elt stack =
    if stack.courant >= capacite stack
    then failwith "Pile pleine."
    else stack.donnees.(stack.courant) <- Some elt;stack.courant <- stack.courant + 1


let peek_1 stack =
  let temp = pop stack in
  match temp with
  |None -> None
  |Some x -> push x stack; temp;


let is_empty stack = 
  stack.donnees.(0) = None || false

let iter_destructif f stack = 
  while not(is_empty stack) do
      let temp = Option.get (pop stack) in
      f temp
  done;

let itere f stack = 
  let nb = stack.courant in
  for i = nb - 1 downto 0 do
      f stack.donnees.(i)
  done;

let egal s1 s2 = (s1.donnees = s2.donnees)

let flush stack =
  stack.donnees <- stack.vide

let egal_destructif s1 s2 =
  while (not(is_empty s1) && not(is_empty s2)) && pop s1 = pop s2 do
      ()
  done;
  is_empty s1 && is_empty s2