type instance = int * int list

type box = {
  lst : int list;
  capacity : int;
}

let create_new_box capacity = 
  {lst=[];capacity=capacity} 

let add_to_box box elt = 
  {lst= elt :: box.lst;capacity= box.capacity - elt}
  
let fits_in box elt = 
  box.capacity > elt


let next_fit inst = 
  let lst = ref [] in
  let capacity,data_lst = inst in 
  let first_box = create_new_box capacity in 
  let rec aux box = 
    match data_lst with
    |[] -> !lst
    |x :: xs -> 
      if fits_in box x then begin 
        aux (add_to_box box x);
      end 
      else begin 
        lst := box :: !lst;
        x :: data_lst;
        aux (create_new_box capacity)
      end
  in aux first_box

(*
let add_first_box lst_box elt capacity = 
  match lst_box with
  |[] -> let new_box = create_new_box capacity in 
          [add_to_box new_box elt]
  |b :: bs when not fits_in box elt-> b :: aux bs elt
  |b :: bs -> let nb = add_to_box b elt;
              nb :: bs
*)
(*
let first_fit inst = 
  let capacity,data_lst = inst in 
  let lst_boxes = [create_new_box capacity] in
  let rec aux lst_b = 
    match data_lst with 
    |[] -> lst_boxes
    |x :: xs -> aux (add_first_box lst_boxes capacity)
  in aux lst_boxes


let first_fit_decreasing inst = 
  let capacity, data_lst = inst in 
  first_fit (capacity, List.sort (fun (x,y) -> y - x) data_lst)
*)