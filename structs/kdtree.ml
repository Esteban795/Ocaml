type kdnode = 
  | Leaf of int * int
  | CutNode of int * int * kdnode * kdnode

type kdtree = {
  points : float array array;    (*Each point is a d-dimensional array*)
  root : kdnode
}


let swap arr i j = 
  let temp = arr.(i) in 
  arr.(i) <- arr.(j);
  arr.(j) <- temp


let partition points lo hi dim = 
  let index = Random.int (hi - lo) + lo in 
  let piv = points.(index) in 
  swap points (hi - 1) index;
  let j = ref 0 in 
  for i = lo to hi - 2 do 
    if piv.(dim) >= points.(i).(dim) then (
      swap points i !j;
      incr j
    )
  done;
  swap points index !j;
  !j

let rec quickselect arr lo hi dim k = 
  let index = partition arr lo hi dim in 
  if k = index then ()
  else if k < index then quickselect arr lo index dim k
  else if k > index then quickselect arr (index + 1) hi dim k

let build_kdtree data leaf_size = 
  let arr = Array.copy data in 
  let nb_dim = Array.length data.(0) in 
  let rec aux lo hi dim = 
    if hi - lo <= leaf_size then Leaf (lo,hi)
    else 
      begin 
        let avg = (hi + lo) / 2 in 
        quickselect arr lo hi dim avg;
        let dim' = (dim + 1) mod nb_dim in 
        let left = aux lo avg dim' in 
        let right = aux (avg + 1) hi dim' in 
        CutNode (dim, avg, left, right)
      end 
    in
    let root = aux 0 (Array.length data) 0 in 
    {
      root = root;
      points = arr
    }
