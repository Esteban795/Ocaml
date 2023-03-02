(*sac à dos naif*)

let p_ex = [|3; 8; 5; 1; 6; 1; 2; 6; 6; 1; 7; 8; 9; 12; 4; 1; 5; 7; 11; 4; 1; 5; 12; 13|]
let v_ex = [|1; 2; 6; 3; 7; 8; 2; 3; 4; 7; 5; 2; 12; 8; 5; 3; 7; 10; 8; 7; 4; 15; 7; 20|]          


let sac p v pmax = 
    let rec aux k d = 
        if d < 0 then min_int
        else if k = 0 then 0
        else 
            let sup = aux (k - 1) d in
            let inf = v.(k - 1) + aux (k - 1) (d - p.(k - 1))
            in max sup inf
    in aux (Array.length p) pmax;

let sac_mem p v pmax =
  let n = Array.length p in
  let table = Array.make_matrix (n + 1) (pmax + 1) None in
  let rec aux k d = 
      if d < 0 then min_int
      else if k = 0 then 0
      else 
          match table.(k).(d) with
          | Some v -> v
          |None ->
              let res = 
                  let sup = aux (k - 1) d in
                  let inf = v.(k - 1) + aux (k - 1) (d - p.(k - 1))
                  in max sup inf in
              table.(k).(d) <- Some res;
              res in
      aux n pmax


let sac_dyn p v pmax = 
  let n = Array.length p in
  let table = Array.make_matrix (n + 1) (pmax + 1) 0 in
  let score_prec k d = if d < 0 then min_int else table.(k).(d) in
  for k = 1 to n do
      for d = 0 to pmax do
          let sup = score_prec (k - 1) d in
          let inf = v.(k - 1) + score_prec (k - 1) (d - p.(k - 1))
          in table.(k).(d) <- max sup inf 
      done;
  done;
  table.(n).(pmax)

let sac_mem_reconstruit p v pmax =
  let n = Array.length p in
  let table = Array.make_matrix (n + 1) (pmax + 1) None in
  let rec aux k d = 
      if d < 0 then (min_int,[])
      else if k = 0 then (0,[])
      else 
          match table.(k).(d) with
          |Some tuple -> tuple
          |None ->
              let res = 
                  let sup,sol_sup = aux (k - 1) d in
                  let inf,sol_inf = aux (k - 1) (d - p.(k - 1)) in
                  if v.(k - 1) + inf > sup then
                      (v.(k - 1) + inf, (k - 1) :: sol_inf)
                  else
                      (sup,sol_sup) in
              table.(k).(d) <- Some res;
              res in
      aux n pmax