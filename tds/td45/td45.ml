type arbre = 
    |Vide
    |Feuille of char
    |Noeud of arbre * arbre


let rec bien_forme tree = 
  match tree with
  |Vide -> true
  |Feuille w -> true
  |Noeud(Vide,Vide) -> false
  |Noeud(a,b) -> bien_forme a && bien_forme b

type arbre_code = 
  |F of char
  |N of arbre_code * arbre_code

type bitstream = bool list

let rec decode_caractere tree u = 
  match tree, u with
  |F x, _ -> (x,u)
  |N(ga,dr), false :: us -> decode_caractere ga us
  |N(ga,dr), true :: us -> decode_caractere dr us
  |_ -> failwith "what da code doin'"

let string_of_char_list u = String.of_seq (List.to_seq u)

let decode_texte tree bits = 
  let rec aux bits_temp = 
      match bits_temp with
      |[] -> []
      |_ -> let (s,rest) = decode_caractere tree bits_temp in
              s :: aux rest
  in string_of_char_list (aux bits)

type table_code = bitstream array

let cree_table tree = 
  let table = Array.make 256 [] in 
  let rec aux parc_prefixe noeud =
      match noeud with
      |F carac -> table.(int_of_char carac) <- List.rev parc_prefixe
      |N(ga,dr) -> aux (false :: parc_prefixe) ga;
                  aux (true :: parc_prefixe) dr in
  aux [] tree;
  table

let encode (tab : table_code) s = 
  let len = String.length s in
  let rec aux l =
      if l = len then []
      else tab.(int_of_char s.[l]) @ aux (l + 1)
  in aux 0

let occurences s = 
  let t = Array.make 256 0 in 
  let string_length = String.length s in
  for i = 0 to string_length - 1 do
      t.(int_of_char s.[i]) <- 1 + t.(int_of_char s.[i])
  done;
  t

let foret s = 
  let tab_occs = occurences s in
  let lst_temp = ref [] in
  for i = 0 to 255 do
      if tab_occs.(i) <> 0 then lst_temp := (F (char_of_int i),tab_occs.(i)) :: !lst_temp
  done;
  List.rev !lst_temp


let huffman s = 
  let prioq = PrioQ.of_list (foret s) in (*techniquement, il faudrait réadapter la PrioQ du td42, mais j'ai la flemme*)
  while PrioQ.length prioq > 1 do
      let (c,occs) = PrioQ.extract_min prioq in
      let (c',occs') = PrioQ.extract_min prioq in
      PrioQ.insert prioq (N(c,c'),occs + occs')
  done;
  fst (PrioQ.extract_min prioq)

let compresse s =
  let arbre = huffman s in
  let table = cree_table arbre in 
  (arbre,encode table)