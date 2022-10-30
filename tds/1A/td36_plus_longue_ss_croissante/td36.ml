let aux_dyn s =
  let n = Array.length s in
  let arr = Array.make n 0 in
  for k = 0 to n - 1 do
      let maxi = ref 1 in
      for i = 0 to k - 1 do
          if s.(i) <= s.(k) then maxi := max !maxi (1 +  arr.(i))
      done;
      arr.(k) <- !maxi
  done;
  arr


let l_seq_dyn s = 
  let arr = aux_dyn s in
  let maxi = ref 1 in 
  for i = 1 to Array.length s - 1 do
      maxi := max !maxi arr.(i)
  done;
  !maxi


let indice_max s =
  let len = Array.length s in
  let indice_maxi = ref 0 in
  for i = 1 to len - 1 do
      if s.(i) > s.(!indice_maxi) then indice_maxi := i
  done;
  s.(!indice_maxi), !indice_maxi


let sous_sequence_dyn s = 
  let arr = aux_dyn s in
  let maximum,indice_max = indice_max arr in
  let sortie = Array.make maximum s.(indice_max) in
  let k = ref (indice_max - 1) in
  let i = ref (maximum - 1) in
  while !i > 0 do
      if arr.(!k) = !i && s.(!k) <= sortie.(!i) then begin
          decr i;
          sortie.(!i) <- s.(!k)
      end;
      decr k
  done;
  sortie


  let patience lst = 
  let res = Array.make (List.length lst) [] in
  let rec stack cards current = 
      match cards, res.(current) with
      | [],_ -> res
      |x :: xs, y :: ys when x >= y -> stack cards (current + 1)
      |x :: xs, r -> res.(current) <- x :: r; stack xs 0 
  in stack lst 0


let patience_opt s = 
  let n = List.length s in
  let res = Array.make (List.length s) [] in
    let rec bs deb fin carte  =
        if deb = fin then fin
        else
            let milieu = ((deb+fin)/2) in
            match res.(milieu) with
            | x :: xs when carte >= x -> bs (milieu + 1) fin carte
            |_ -> bs deb milieu carte
  in 
  let rec stack cards = 
      match cards with
      |[] -> res
      |x :: xs -> 
          let num_pile = bs 0 (n - 1) x in res.(num_pile) <- (x :: res.(num_pile));
      stack xs 
  in stack s