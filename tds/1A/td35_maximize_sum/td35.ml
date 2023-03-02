(*Objectif : 
- somme maximale en prenant uniquement un élément de chaque ligne et de chaque colonne.
Si on prend un élément à la ligne j et à la colonne i, on a pas le droit de prendre le prochain élément
sur la ligne i et dans la colonne j

Relation de récurrence 
*)

let m1 = [|
 [|7;  53; 183; 439; 863 |];
 [|497; 383; 563;  79; 973 |];
 [|287;  63; 343; 169; 583 |];
 [|627; 343; 773; 959; 943 |];
 [|767; 473; 103; 699; 303 |]
|]

let m2 = [|
 [|  7; 53;183;439;863;497;383;563; 79;973;287; 63;343;169;583|];
 [|627;343;773;959;943;767;473;103;699;303;957;703;583;639;913|];
 [|447;283;463; 29; 23;487;463;993;119;883;327;493;423;159;743|];
 [|217;623;  3;399;853;407;103;983; 89;463;290;516;212;462;350|];
 [|960;376;682;962;300;780;486;502;912;800;250;346;172;812;350|];
 [|870;456;192;162;593;473;915; 45;989;873;823;965;425;329;803|];
 [|973;965;905;919;133;673;665;235;509;613;673;815;165;992;326|];
 [|322;148;972;962;286;255;941;541;265;323;925;281;601; 95;973|];
 [|445;721; 11;525;473; 65;511;164;138;672; 18;428;154;448;848|];
 [|414;456;310;312;798;104;566;520;302;248;694;976;430;392;198|];
 [|184;829;373;181;631;101;969;613;840;740;778;458;284;760;390|];
 [|821;461;843;513; 17;901;711;993;293;157;274; 94;192;156;574|];
 [| 34;124;  4;878;450;476;712;914;838;669;875;299;823;329;699|];
 [|815;559;813;459;522;788;168;586;966;232;308;833;251;631;107|];
 [|813;883;451;509;615; 77;281;613;459;205;380;274;302; 35;805|];
|]

let rec range a b =   (*Generates [a, a + 1... b - 1]*)
  if a >= b then []
  else a :: range (a + 1) b

let rec prive_de lst elt =  (*Removes first occurence of elt from arr*)
  match lst with
  |[] -> []
  |h :: t -> if h = elt then t else h :: (prive_de t elt)


let rec max_liste lst = 
  match lst with 
  |[] -> min_int 
  |x :: xs -> max x (max_liste xs)


(*Solution naïve*)
let rec eval m s = 
  let j = List.length s in
  if j = 0 then 0
  else
      let f i = m.(j - 1).(i) + eval m (prive_de s i) in
      let liste_evals = List.map f s in
      max_liste liste_evals

let s_naif m = 
  eval m (range 0 (Array.length m))



let s_mem m = 
  let n = Array.length m in
  let t = Hashtbl.create (1 lsl n) in
  Hashtbl.add t [] 0;
  let rec aux s = 
      if Hashtbl.mem t s then Hashtbl.find t s
      else begin
          let k = List.length s in
          let f i = m.(k - 1).(i) + aux (prive_de s i) in
          let liste_evals = List.map f s in
          let res = max_liste liste_evals in
          Hashtbl.add t s res;
          res
      end in
  aux (range 0 n)

  
let eval_opt2 mat = 
  let n = Array.length mat in
  let table = Array.make (1 lsl n) 0 in 
  let courants = ref [0] in 
  for k = 0 to n - 1 do
    let nouveaux = ref [] in
    (* Pour chaque x, on regarde toutes les colonnes non utilisees.
        On rajoute la colonne, on obtient un y, que l'on rajoute a nouveaux
        si c'est la premiere fois qu'on le voit et l'on met 
        si besoin a jour la case correspondante dans table. *)
    let traite x = 
      for i = 0 to n - 1 do
        if (x lsr i) land 1 = 0 then
          let y = x + (1 lsl i) in 
          if table.(y) = 0 then 
            nouveaux := y :: !nouveaux;
            table.(y) <- max (table.(x) + mat.(k).(i)) table.(y)
      done in
    List.iter traite !courants;
    courants := !nouveaux
  done;
  table.(1 lsl n -1)