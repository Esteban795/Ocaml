(*borne gauche incluse, droite exclue*)

let indice_pic arr = 
  let rec aux deb fin = 
      if fin - deb = 1 then deb (*reste un élément*)
      else if fin - deb = 2 then if arr.(deb) >= arr.(fin - 1) then deb else fin - 1 (*reste que deux éléments*)
      else 
          let mid = ((fin + deb) / 2) in 
          if arr.(mid) >= arr.(mid - 1) && arr.(mid) >= arr.(mid + 1) then mid
          else if arr.(mid) <= arr.(mid - 1) then aux deb mid
          else aux (mid + 1) fin
  in aux 0 (Array.length arr)



let partition n = 
  let arr = Array.make_matrix (n + 1) (n + 1) (-1) in
  let rec aux m k = 
      if m = 0 then 1
      else if m < 0 then 0
      else if k = 0 then 0 
      else 
          if arr.(m).(k) = -1 then
              arr.(m).(k) <- aux (m - k) k; + aux m (k - 1);
          arr.(m).(k)
  in aux n n