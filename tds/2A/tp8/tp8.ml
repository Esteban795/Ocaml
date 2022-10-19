let u_tab = Array.make 1000000 0;;

let calc_u = 
  let u0 = 42 in
  let multiplieur = 19999999 in 
  let modulo = 19999981 in
  u_tab.(0) <- u0;
  for i = 1 to 1000000 - 1 do 
    u_tab.(i) <- (multiplieur * u_tab.(i - 1)) mod modulo
  done;
  u_tab;;

calc_u;;
print_int (u_tab.(123) mod 1000);;
print_newline ();;
print_int (u_tab.(456) mod 1000);;
print_newline ();;
print_int (u_tab.(789) mod 1000);;

let vt_value i p = if (u_tab.(i) mod 10000) < p then 1 else 0;;

let calc_v_tab n p = 
  Array.init (n*n) (fun i -> vt_value i p);;

let count_edges n p = 
  let v_tab = calc_v_tab n p  in 
  let counter = ref 0 in 
  for i = 0 to n * (n-1)/2 do 
    if v_tab.(i) = 1 then incr counter
    done;
  !counter;;


let explicit_graph n p =
  let tab = Array.make n [] in 
  let v_tab = calc_v_tab n p in
  let k = ref 0 in 
  for i = 0 to n - 1 do 
    for j = i + 1 to n - 1 do 
      if v_tab.(!k) = 1 then
        begin 
          tab.(i) <- j  ::  tab.(i); 
          tab.(j) <- i :: tab.(j)
        end;
      incr k
    done;
  done;
  tab;;

let connex_to_0 n p = 
  let g = explicit_graph n p in 
  let seen = Array.make n false in 
  let counter = ref 0 in 
  let rec explore s = 
    if not seen.(s) then begin 
      seen.(s) <- true;
      incr counter;
      List.iter explore g.(s)
    end;
  in explore 0;
  !counter;;


let count_connex_parts n p = 
  let g = explicit_graph n p in 
  let seen = Array.make n false in 
  let count = ref 0 in 
  let rec explore s = 
    if not seen.(s) then begin 
      seen.(s) <- true;
      List.iter (fun s' -> explore s') g.(s)
    end;
  in explore 0;
  for i = 1 to n - 1 do 
    if not seen.(i) then begin
      incr count;
      explore i
    end;
  done;
  !count;;

let explicit_graph_v2 n p k = 
  let tab = Array.make n [] in 
  let v_tab = calc_v_tab n p in
  let k = ref 0 in 
  for i = 0 to n - 1 do 
    if i < n - 1 then begin
      tab.(i) <- (i + 1) :: tab.(i);
      tab.(i + 1) <- i :: tab.(i + 1);
    end;
    for j = i + 1 to n - 1 do 
      if v_tab.(!k) = 1 then
        begin 
          tab.(i) <- j  ::  tab.(i); 
          tab.(j) <- i :: tab.(j)
        end;
      incr k
    done;
  done;
  tab;;


let nb_moves n p = 
  let g = explicit_graph_v2 n p in 
  

print_newline ();;
print_newline ();;
print_newline ();;
print_int (count_edges 10 654);;
print_newline ();;
print_int (count_edges 100 543);;
print_newline ();;
print_int (count_edges 1000 12);;
print_newline ();;
print_newline ();;
print_newline ();;
print_int (connex_to_0 10 654);;
print_newline ();;
print_int (connex_to_0 100 543);;
print_newline ();;
print_int (connex_to_0 1000 12);;
print_newline ();;
