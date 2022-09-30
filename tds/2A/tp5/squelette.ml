type state = int

type nfa =
  {delta : state list array array;
  accepting : bool array}

let graphviz_nfa a filename =
  let open Printf in
  let n = Array.length a.delta in
  let m = Array.length a.delta.(0) in
  let out = open_out filename in
  fprintf out "digraph a {\nrankdir = LR;\n";
  (* noms des états *)
  let lettre i = String.make 1 (char_of_int (i + int_of_char 'a')) in
  (* etats *)
  for q = 0 to n - 1 do
    let shape = if a.accepting.(q) then "doublecircle" else "circle" in
    fprintf out "node [shape = %s, label = %d] %d;\n" shape q q
  done;
  (* etat initial *)
  fprintf out "node [shape = point]; I\n";
  fprintf out "I -> %i;\n" 0;
  (* transitions *)
    let labels = Array.make_matrix n n [] in
  for q = 0 to n - 1 do
    for x = m - 1 downto 0 do
      let ajoute q' = labels.(q).(q') <- lettre x :: labels.(q).(q') in
      List.iter ajoute a.delta.(q).(x)
    done
  done;
  for q = 0 to n - 1 do
    for q' = 0 to n - 1 do
      let s = String.concat "," labels.(q).(q') in
      if s <> "" then
        fprintf out "%i -> %i [ label = \"%s\" ];\n" q q' s
    done
  done;
  fprintf out "}\n";
  close_out out

let genere_pdf input_file output_file =
  Sys.command (Printf.sprintf "dot -Tpdf %s -o %s" input_file output_file)


type 'a regex =
  | Empty
  | Eps
  | Letter of 'a
  | Sum of 'a regex * 'a regex
  | Concat of 'a regex * 'a regex
  | Star of 'a regex


(* Parses a string into an int regex.
 * The alphabet is assumed to be a subset of a..z, and is converted
 * to [0..25] (a -> 0, b -> 1...),
 * Charcater '&' stands for "epsilon", and character '#' for "empty".
 * Spaces are ignored, and the usual priority rules apply.
 *)

let parse string =
  let open Printf in
  let to_int c =
    assert ('a' <= c && c <= 'z');
    int_of_char c - int_of_char 'a' in
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
    | Some '|' -> eat '|'; Sum (t, regex ())
    | _ -> t
  and term () =
    let f = factor () in
    match peek () with
    | None | Some ')' | Some '|' -> f
    | _ -> Concat (f, term ())
 and factor () =
    let rec aux acc =
      match peek () with
      | Some '*' -> eat '*'; aux (Star acc)
      | _ -> acc in
    aux (base ())
  and base () =
    match peek () with
    | Some '(' -> eat '('; let r = regex () in eat ')'; r
    | Some '&' -> eat '&'; Eps
    | Some '#' -> eat '#'; Empty
    | Some (')' | '|' | '*' as c) -> failwith (sprintf "unexpected '%c'" c)
    | Some c -> eat c; Letter (to_int c)
    | None -> failwith "unexpected end of string" in
  let r = regex () in
  try Stream.empty s; r
  with _ -> failwith "trailing ')' ?"


let rec string_of_regex e =
  let open Printf in
  let to_char i =
    char_of_int (i + int_of_char 'a') in
  let priorite = function
    | Sum (_, _) -> 1
    | Concat (_, _) -> 2
    | Star _ -> 3
    | _ -> 4 in
  let parenthese expr parent =
    if priorite expr < priorite parent then
      sprintf "(%s)" (string_of_regex expr)
    else string_of_regex expr in
  match e with
  | Empty -> "#"
  | Eps -> "&"
  | Letter x -> sprintf "%c" (to_char x)
  | Sum (f, f') -> sprintf "%s|%s" (parenthese f e) (parenthese f' e)
  | Concat (f, f') -> sprintf "%s%s" (parenthese f e) (parenthese f' e)
  | Star f -> sprintf "%s*" (parenthese f e)


type dfa =
  {delta_d : state array array;
  accepting_d : bool array}

let to_nfa a =
  let n = Array.length a.delta_d in
  let m = Array.length a.delta_d.(0) in
  let delta = Array.make_matrix n m [] in
  for q = 0 to n - 1 do
    for x = 0 to m - 1 do
      delta.(q).(x) <- [a.delta_d.(q).(x)]
    done
  done;
  {delta = delta; accepting = a.accepting_d}

let graphviz_dfa a = graphviz_nfa (to_nfa a)

let rec merge u v = 
  match u,v with
  | [], ys -> ys
  | xs, [] -> xs
  | x :: xs, y :: ys -> 
    if x = y then x ::  (merge xs ys)
    else 
      if x < y then x :: (merge xs v)
      else y :: (merge u ys)


let rec is_empty reg = 
  match reg with 
  | Empty -> true
  | Sum(x,y) -> is_empty x && is_empty y
  |Concat(x,y) -> is_empty x || is_empty y
  |_ -> false


let rec contains_epsilon reg = 
  match reg with
  |Empty | Letter _ -> false
  |Eps | Star _ -> true
  |Sum(x,y) -> contains_epsilon x || contains_epsilon y
  |Concat(x,y) -> contains_epsilon x && contains_epsilon y

let rec prefix e = 
  match e with 
  | Eps | Empty -> []
  |Letter x -> [x]
  |Sum(x,y) -> merge (prefix x) (prefix y)
  |Concat(x,y) -> 
    if is_empty x || is_empty y then []
    else if contains_epsilon x then merge (prefix x) (prefix y)
    else prefix x
  |Star x -> prefix x


let rec suffix e = 
  match e with
  |Eps | Empty -> []
  |Letter x -> [x]
  |Sum(x,y) -> merge (suffix x) (suffix y)
  |Concat(x,y) -> 
    if is_empty x || is_empty y then []
    else if contains_epsilon y then merge (prefix x) (prefix y)
    else prefix y
  |Star x -> suffix x


let rec combine u v = 
  match u with
  |[] -> []
  |x :: xs -> 
    let prod_avec_x = List.map (fun y -> (x,y)) v in 
    prod_avec_x @ combine xs v

let rec factor reg = 
  match reg with 
  |Empty | Eps | Letter _-> []
  |Sum(x,y) -> 
    merge (factor x) (factor y)
  |Concat(x,y) -> 
    if is_empty x || is_empty y then []
    else merge (merge (factor x) (factor y)) (combine (suffix x) (prefix y))
  |Star x -> merge (factor x) (combine (suffix x) (prefix x))



let rec number_of_letters reg = 
  match reg with
  |Eps | Empty -> 0
  |Letter _ -> 1
  |Star x -> number_of_letters x
  |Concat(x,y) | Sum(x,y) -> number_of_letters x + number_of_letters y



let linearize reg = 
  let i = ref 0 in 
  let rec aux t = 
    match t with
    |Empty -> Empty
    |Eps -> Eps
    |Letter x -> incr i; Letter (x, !i)
    |Sum(x,y) -> let x' = aux x in Sum(x', aux y)
    |Concat(x,y) -> let x' = aux x in Concat(x', aux y)
    |Star x -> Star (aux x) in 
  aux reg


let rec max_letter reg = 
  match reg with 
  |Empty | Eps -> -1
  |Letter i -> i 
  |Star x -> max_letter x
  |Sum(x,y) | Concat(x,y) -> max (max_letter x) (max_letter y)


let glushkov reg = 
  let linearized = linearize reg in 
  let pre = prefix linearized in 
  let suf = suffix linearized in 
  let fac = factor linearized in 
  let n = number_of_letters linearized in 
  let m = max_letter reg in
  let delta = Array.make_matrix (n + 1) (m + 1) [] in 
  let accepting = Array.make (n + 1) false in 

  let f i x j = delta.(i).(x) <- j :: delta.(i).(x) in 
  List.iter( fun (x,i) -> f 0 x i) pre;
  List.iter (fun ((x,i),(y,j)) -> f i y j) fac;
  List.iter (fun (x,i) -> accepting.(i) <- true) suf;

  if contains_epsilon reg then accepting.(0) <- true;
  {delta = delta; accepting = accepting}

  
let delta_set automate set letter = 
  let n = Array.length set in 
  let new_set = Array.make n false in
  let transition s = new_set.(s) <- true in 
  for state = 0 to n - 1 do
    if set.(state) then List.iter transition automate.delta.(state).(letter)
  done;
  new_set

let has_accepting_state automate set = 
  let final = automate.final in 
  let n = Array.length set in 
  for i = 0 to n - 1 do
    if final.(i) && automate.(i) then true
  done;

let nfa_accept automate lst = 
  let n = Array.length automate.final in 
  let rec app_delta_star set word = 
    match word with 
    | [] -> set
    | x :: xs ->
        let new_set = delta_set automate set x in 
        app_delta_star new_set xs in 
  let init = Array.make n false in
  in init.(0) <- true;
  let final = app_delta_star init lst in 
  has_accepting_state automate final

  

let build_set nfa lst letter = 
  let n = Array.length nfa.delta in 
  let next_states_present = Array.make n false in 
  let rec aux app_delta_to_states states = (*On applique la fonction de transfert sur chaque état*)
    match states with 
    |[] -> ()
    | q :: qs -> 
      next_states_present.(a.delta.(q).(letter)) <- true; (*si l'état est présent quand on fait letter, on ajoute l'état aux états qu'on aura ensuite*)
      aux qs
  in aux lst
  let next_set = ref [] in 
  for state = 0 to 0 do
    if next_states_present.(i) then next_set := i :: !next_set
  done;
  !next_set
