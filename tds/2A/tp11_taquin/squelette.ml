let n = 4

type state = {
  grid : int array array;
  mutable i : int;
  mutable j : int;
  mutable h : int;
}

let print_state state =
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if i = state.i && j = state.j then print_string "   "
      else Printf.printf "%2d " state.grid.(i).(j)
    done;
    print_newline ()
  done

type direction = U | D | L | R | No_move

let delta = function
  | U -> (-1, 0)
  | D -> (1, 0)
  | L -> (0, -1)
  | R -> (0, 1)
  | No_move -> assert false

let string_of_direction = function
  | U -> "Up"
  | D -> "Down"
  | L -> "Left"
  | R -> "Right"
  | No_move -> "No move"


(* Part 1 *)

let possible_moves state =
  let legal_moves = ref [] in  
  if state.i > 0 then legal_moves := U :: !legal_moves;
  if state.i < n - 1 then legal_moves := D :: !legal_moves;
  if state.j > 0 then legal_moves := L :: !legal_moves;
  if state.j < n - 1 then legal_moves := R :: !legal_moves;
  !legal_moves

let distance i j value =
  let i_target = value / n in
  let j_target = value mod n in
  abs (i - i_target) + abs (j - j_target)

let compute_h state =
  let s = ref 0 in 
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do  
      if state.i <> i || j <> state.j then s := !s + distance i j state.grid.(i).(j)
    done;
  done;
  state.h <- !s
    

let delta_h state move =
  let dxi,dyj = delta move in 
  let i = state.i in 
  let j = state.j in 
  let di = i + dxi in 
  let dj = j + dyj in 
  let elt = state.grid.(di).(dj) in 
  distance i j elt - distance (di) (dj) elt



let apply state move = 
  let di,dj = delta move in 
  let elt = state.grid.(state.i + di).(state.j + dj) in 
  state.h <- state.h + delta_h state move;
  state.grid.(state.i).(state.j) <- elt;
  state.i <- state.i + di;
  state.j <- state.j + dj;;


let copy state =
  let copy_grid = Array.init (Array.length state.grid) (fun i -> Array.copy state.grid.(i)) in
  {grid = copy_grid; i = state.i; j = state.j; h= state.h};;


(* A few examples *)

(* the goal state *)
let final =
  let m = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      m.(i).(j) <- i * n + j
    done
  done;
  {grid = m; i = n - 1; j = n - 1; h = 0}

(* Generates a state by making nb_moves random moves from *)
(* the final state. Returns a state s such that *)
(*  d(initial, s) <= nb_moves (obviously). *)
let random_state nb_moves =
  let state = copy final in
  for i = 0 to nb_moves - 1 do
    let moves = possible_moves state in
    let n = List.length moves in
    apply state (List.nth moves (Random.int n))
  done;
  state

(* distance 10 *)
let ten =
  let moves = [U; U; L; L; U; R; D; D; L; L] in
  let state = copy final in
  List.iter (apply state) moves;
  state

(* distance 20 *)
let twenty =
  {grid =
    [| [|0; 1; 2; 3|];
      [|12; 4; 5; 6|];
      [|8; 4; 10; 11|];
      [|13; 14; 7; 9|] |];
   i = 1; j = 1; h = 14}

(* distance 30 *)
let thirty =
  {grid =
     [| [|8; 0; 3; 1|];
       [|8; 5; 2; 13|];
       [|6; 4; 11; 7|];
       [|12; 10; 9; 14|] |];
   i = 0; j = 0; h = 22}

(* distance 40 *)
let forty =
  {grid =
     [| [|7; 6; 0; 10|];
       [|1; 12; 11; 3|];
       [|8; 4; 2; 5|];
       [|8; 9; 13; 14|] |];
   i = 2; j = 0; h = 30}

(* distance 50 *)
let fifty =
  let s =
    {grid =
       [| [| 2; 3; 1; 6 |];
          [| 14; 5; 8; 4 |];
          [| 15; 12; 7; 9 |];
          [| 10; 13; 11; 0|] |];
     i = 2;
     j = 3;
     h = 0} in
  compute_h s;
  s

(* distance 64 *)
let sixty_four =
  let s =
    {grid =
       [| [| 15; 14; 11; 7|];
          [| 5; 9; 12; 4|];
          [| 3; 10; 13; 8|];
          [| 2; 6; 0; 1|] |];
     i = 0;
     j = 0;
     h = 0} in
  compute_h s;
  s


(* Part 2 *)


let successors state =
  let moves = possible_moves state in
  let succ = ref [] in
  let rec create_succ_state coords = 
    match coords with
    |[] -> !succ
    |d :: reste -> 
      let s_copy  = copy state in 
      apply s_copy d;
      succ := s_copy :: !succ;
      create_succ_state reste;
  in create_succ_state moves




let reconstruct parents x =
  let rec aux s chemin = 
    let parent = Hashtbl.find parents s in 
    if parent = s then chemin
    else aux parent (s :: chemin)
  in aux x []

exception No_path

let astar initial =
  let dist = Hashtbl.create 1000 in 
  let parents = Hashtbl.create 1000 in 
  let q = Heap.create () in 
  Heap.insert q (initial,initial.h);
  Hashtbl.add dist initial 0;
  Hashtbl.add parents initial initial;
  let rec loop () = 
    match Heap.extract_min q with (*renvoie un Some couple*)
    |None -> raise No_path 
    |Some (s,p) ->
      if s.h = 0 then reconstruct parents s 
      else 
        let succs = successors s in 
        let dist_s = Hashtbl.find dist s in
        let process_succ v = 
          let dist_v = dist_s + 1 in
          match Hashtbl.find_opt dist v with
          |Some d when d <= dist_v -> ()
          |_ ->
              Hashtbl.replace dist v dist_v;
              Hashtbl.replace parents v s;
              Heap.insert_or_decrease q (v, dist_v + v.h)
        in List.iter process_succ succs;
        loop () 
  in loop ()


(* Part 3 *)

exception Found
(*
let idastar_length initial =
  let s = copy initial in 
  let minimum = ref max_int in
  let rec dfs maximum prof = 
    let c = prof + s.h in 
    if c > maximum then c
    else 
      if s.h = 0 then raise (Found prof)
    else begin
      minimum := max_int;
      let bouger move = 
        apply s move;
        minimum := min !minimum (dfs (prof + 1) maximum);
      in List.iter bouger (possible_moves s);
      !minimum  end in
  let rec loop maxi = 
    let m = dfs maxi 0 in 
    if m = max_int then None
    else loop m in 
  try 
    loop s.h 
  with
  |Found-> Some d*)

let opposite_move move =
  match move with 
  |U -> D 
  |D -> U
  |L -> R 
  |R -> L
  |No_move -> No_move

let idastar initial =
  let chemin = Vector.create () in
  let s = copy initial in 
  let rec dfs prof maximum last_move = 
    let c = prof + s.h in 
    if c > maximum then c
    else if s.h = 0 then raise Found
    else 
      let minimum = ref max_int in
      let bouger move = 
        if move <> opposite_move last_move then (
          Vector.push chemin move;
          apply s move;
          minimum := min !minimum (dfs (prof + 1) maximum move);
          apply s (opposite_move move);
          ignore (Vector.pop chemin))
      in List.iter bouger (possible_moves s);
      !minimum
  in 
  let rec loop maxi = 
    let m = dfs 0 maxi No_move in 
    if m = max_int then None
    else loop m in 
  try
    loop s.h 
  with 
  |Found  -> Some chemin

let print_direction_vector t =
  for i = 0 to Vector.length t - 1 do
    Printf.printf "%s " (string_of_direction (Vector.get t i))
  done;
  print_newline ()

let print_idastar state =
  match idastar state with
  | None -> print_endline "No path"
  | Some t ->
    Printf.printf "Length %d\n" (Vector.length t);
    print_direction_vector t


let main () =
  print_idastar fifty;
  Printexc.record_backtrace true

let () = main ()
(*ocamlopt -g -o taquin vector.ml heap.ml squelette.ml*)