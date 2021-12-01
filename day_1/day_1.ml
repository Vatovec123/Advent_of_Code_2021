let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := int_of_string (input_line chan) :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
(* Povezava do spletne strani, iz katere je prepisana zgornja koda: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
  
(*--------------------------------------------------------------------------------------------------------------------------------*)
let ali_je_stevilo_vecje sez =
  let rec aux acc sez'=
    match sez' with
    | [] -> List.rev acc
    | x:: [] -> List.rev acc
    | x :: y :: ys -> if y > x then aux (y :: acc) (y :: ys) else aux acc (y :: ys)
   in
  aux [] sez

let seznam_vecjih  =  ali_je_stevilo_vecje 

let dolzina_seznama sez = 
  let rec aux acc sez' =
  match sez' with
    | [] -> acc
    | x :: xs -> aux (1+ acc) xs
  in
  aux 0 sez

let koliko_stevil_je_vecjih sez   = dolzina_seznama ( seznam_vecjih sez )

(*--------------------------------------------------------------------------------------------------------------------------------*)


let rec seznam_vsot_zaporednih_treh_stevil sez =
  let rec aux acc' sez' =
    match sez' with
    | [] -> List.rev acc'
    | x :: [] ->List.rev  acc'
    | x :: y :: [] -> List.rev  acc'
    | x :: y :: z :: zs -> 
      let vsota = x + y + z in
      aux (vsota :: acc') (y::z:: zs)
    in
    aux [] sez


let rec razdeli k sez =
  let rec dolzina_seznama = List.length sez in
  match sez with
  | [] -> ([], [])  
  | x :: xs -> if k <= 0 || dolzina_seznama < k then ( sez,[]) else   
    let (prvi, drugi) = razdeli (k-1) xs in  
    (x :: prvi, drugi)                   
                                             

  

let seznam_vsot sez  = seznam_vsot_zaporednih_treh_stevil sez

let seznam_vecjih_vsot = ali_je_stevilo_vecje

let koliko_vsot_je_vecjih sez   =
    let rec aux acc sez' =
      match sez' with
      |[] -> acc  
      | x :: xs ->    
      let prvih_deset = fst (razdeli 10 sez') in
      let rep_brez_prvih_deset = snd (razdeli 10 sez') in
      aux (dolzina_seznama ( seznam_vecjih (seznam_vsot prvih_deset) ) + acc) rep_brez_prvih_deset
  in
  aux 0 sez




let naloga1 vsebina_datoteke =  koliko_stevil_je_vecjih ( vsebina_datoteke) 
let naloga2 vsebina_datoteke = koliko_vsot_je_vecjih (vsebina_datoteke)

let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "day_1/day_1.in" in
  let odgovor1 = string_of_int(naloga1 vsebina_datoteke)
  and odgovor2 = string_of_int(naloga2 vsebina_datoteke)
  in
  izpisi_datoteko "day_1/day_1_1.out" odgovor1;
  izpisi_datoteko "day_1/day_1_2.out" odgovor2
  
 

(*
[199; 200; 208; 210; 200; 207; 240; 269; 260; 263]
*)

(*let primer = fst ( razdeli 10 [199; 200; 208; 210; 200; 207; 240; 269; 260; 263; 198; 201; 208; 210; 200; 207; 240; 269; 260; 263] );;
seznam_vsot [198; 201; 208; 210; 200; 207; 240; 269; 260; 263];;*)