let triangle_pascal n =
  let tbl = Array.make (n + 1) [||] in
  for i = 0 to n do
      tbl.(i) <- Array.make (i + 1) 0;
      for j = 0 to i do
          if j = i || j = 0 then tbl.(i).(j) <- 1 else tbl.(i).(j) <- tbl.(i - 1).(j) + tbl.(i - 1).(j - 1)
      done;
  done;
  (tbl)


let binom_it k n =
  if k > n || k < 0 then 0
  else if k = 0 || k = n then 1
  else let temp = triangle n in temp.(n).(k)


