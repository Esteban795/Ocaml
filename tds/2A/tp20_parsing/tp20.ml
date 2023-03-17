open Printf

type state = {
  str : string;
  mutable index : int
}

let new_state str = {
  str = str;
  index = 0
}

exception SyntaxError 

let peek s = 
  if s.index < String.length s.str then (Some s.str.[s.index])
  else None

let error s = 
  match peek s with 
  | None ->
    printf "Unexpected end of input\n";
    raise SyntaxError
  |Some c ->
    printf "Unexpected token %c at position %d\n" c s.index;
    raise SyntaxError


let rec expect stream c = 
  match peek stream with
  | Some c' when c = c' ->
    stream.index <- stream.index + 1;
  | _ -> printf "Expected %c\n" c; error stream


let discard stream = 
  if stream.index = String.length stream.str then error stream
  else stream.index <- stream.index + 1

let is_letter c = 
  let num = int_of_char c in 
  num - int_of_char 'A' > 0 && int_of_char 'z' - num > 0



type regex_ast = 
  |Sum of regex_ast * regex_ast
  |Concat of regex_ast * regex_ast
  |Char of char 
  |Star of regex_ast
  |Maybe of regex_ast
  |Any


let rec regex stream = 
  match peek stream with 
  |Some '(' -> 
    begin

    let p = parenthese stream in 
    match peek stream with 
      |Some '+' -> 
        discard stream;
        let temp_p = parenthese stream in (*On process le reste de l'expression*)
        Sum (p,temp_p)
      | Some '*' -> 
        discard stream;
        Star p;
      |Some '?' ->
        discard stream;
        Maybe p;
      |Some '(' ->
        let temp_p = parenthese stream in
        Concat (p,temp_p) 
      |_ -> failwith "pattern matching moment"

    end
  |Some '.' -> 
    discard stream;
    Any
  |Some c when is_letter c ->
    discard stream;
    Char c
  | _ -> error stream

and
parenthese s = 
  match peek s with 
  | Some '(' -> 
    discard s;
    let reste_regex = regex s in (*on parse le reste*)
    expect s ')'; (* on vérifie le bon parenthésage *)
    reste_regex; 
  | _ -> error s (*pattern matching pas exhaustif sinon*)


let parse initial_symbol str =
  let s = new_state str in
  let tree = initial_symbol s in
  match peek s with
  | None -> tree
  | Some c -> printf "Expected input to end\n"; error s

  
let parse_regex str = parse regex str


let rec regex2 (s : state) = 
  let t = term s in 
  match peek s with 
  |Some '+' -> 
    discard s;
    let e = regex2 s in 
    Sum (t,e)
  | _ -> t
  and term s =
    let f = factor s in 
    match peek s with 
    | None | Some ')' | Some '+' -> f 
    | _ -> 
      let t = term s in 
      Concat (f, t)

  and factor s = 
      match peek s with
      | Some '(' -> discard s; 
      let e = regex2 s in
      expect s ')';
      e
      | Some c when is_letter c -> discard s; Char c
      | Some '.' -> discard s; Any
      | _ -> error s


let parse_regex_2 str = 
  parse regex2 str


let rec regex3 s = 
  let t = term s in 
  match peek s with 
  |Some '+' -> 
    discard s;
    let e = regex3 s in 
    Sum (t,e)
  | _ -> t 

  and term s = 
      let f = factor s in 
      match peek s with 
      |None | Some ')' | Some '+' -> f
      | _ ->
        let t = term s in 
        Concat(f,t)
  and factor s = 
      let l = lettre s in 
      quantificateur s l
  
  and quantificateur s a = 
      match peek s with 
      | Some '*' -> discard s; quantificateur s (Star a)
      | Some '?' -> discard s; quantificateur s (Maybe a)
      | _ -> a
  
  and lettre s = 
      match peek s with 
      |Some '(' -> 
        discard s;
        let e = regex3 s in 
        expect s ')';
        e
      |Some c when is_letter c -> discard s; Char c
      |Some '.' -> discard s; Any
      | _ -> error s

let parse_regex_3 str = parse regex3 str


let rec size expr = 
  match expr with 
  | Char _ | Any -> 1
  |Star e | Maybe e -> 1 + size e
  |Concat (e,f) | Sum(e,f) -> 1 + size e + size f

let postfix expr = 
  let len = size expr in 
  let b = Bytes.make len '/' in
  let index = ref 0 in 
  let put c = 
    Bytes.set b !index c;
    incr index;
  in 
  let rec explore_expr ex = 
    match ex with
    | Char c -> put c
    | Any -> put '.'
    | Star e -> explore_expr e; put '*'
    | Maybe e -> explore_expr e; put '?'
    | Concat (e,f) -> explore_expr e; explore_expr f; put '@'
    | Sum (e,f) -> explore_expr e; explore_expr f; put '|' 
  in explore_expr expr;
  Bytes.to_string b
