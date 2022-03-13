(*Calculer le coût optimal d'une multiplication de matrices (via associativité)

- l.(i) = nombre de lignes de la matrice i
- c.(i) = nombre de colonnes de la matrice i 
*)

(*Approche top down*)


let cout_optimal l c = 
    let n = Array.length l in
    let t = Array.make_matrix (n + 1) (n + 1) (-1) in 
    for i = 0 to n - 1 do
      t.(i).(j) <- 0 (*init*)
    done;
    let rec aux i j = 
      if t.(i).(j) = -1 then begin
        let m = ref max_int in
        for k = i + 1 to j - 1 do
          m := min !m (aux i k + aux k j + l.(i) * l.(k) * c.(j - 1)) (*relation de récurrence*)
        done;
        t.(i).(j) <- !m
      end;
      t.(i).(j) in (*on renvoie la valeur dans tous les cas*)
    aux 0 n




(*Approche bottom-up*)

let cout_optimal l c = 
  let n = Array.length l in
  let t = Array.make_matrix (n + 1) (n + 1) (-1) in 
  for delta = 2 to n do 
    for i = 0 to n - delta do 
      let m = ref max_int in 
        for k = i + 1 to i + delta - 1 do
          m := min !m (t.(i).(k) + t.(k).(i + delta) + l.(i) * l.(k) * c.(i + delta))
        done;
      t.(i).(i + delta) <- !m 
    done;
  done;
  t.(0).(n) 