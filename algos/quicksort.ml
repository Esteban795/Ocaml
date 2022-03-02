let echange arr i j = 
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- arr.(i)

let partition arr s e index_piv = 
  let v_piv = arr.(index_piv) in
  echange arr ipiv (e - 1);
  let i = ref s in 
  for j = s to e - 1 do
    if t.(j) <= v_piv then begin
      echange arr i j;
      i := !i + 1
    end;
    !i - 1


let quicksort arr = 
  let rec aux s e = 
    if e - s >= 2 then begin
      let k = partition arr s e s 
    in aux s k;
    aux (k + 1) e
  end in 
  aux 0 (Array.length arr)