let binary_search t x =
  let rec aux deb fin =
      if deb >= fin then (Array.length t)
      else
          let milieu = ((deb+fin)/2) + in
              if t.(milieu) = x then milieu
              else if t.(milieu) < x then aux (milieu + 1) fin
              else aux deb milieu
      in aux 0 (Array.length t)