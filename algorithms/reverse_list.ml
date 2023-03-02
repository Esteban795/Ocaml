let miroir lst =
  let rec aux acc l = 
      match l with
      | [] -> acc
      | h :: t -> aux (h :: acc) t
  in aux [] lst