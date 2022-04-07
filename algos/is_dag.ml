type state = Unknown | Opened | Closed
exception Cycle


(* g is an adjacency list *)
let dfs g =
  let n = Array.length g in
  let seen = Array.make n Unknown in 
  let rec explore v = 
      if seen.(v) = Opened then raise Cycle
      else if seen.(v) = Unknown then begin
        seen.(v) <- Opened;
        List.iter explore g.(x);
        seen.(v) <- Closed;
      end
  in 
  try
    for i = 0 to n - 1 do
      explore i
    done;
    true
  with
  |Cycle -> false