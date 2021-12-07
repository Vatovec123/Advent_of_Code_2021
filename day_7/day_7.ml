let read vsebina =List.map (int_of_string) (String.split_on_char ','(vsebina))


(* Funkcija, ki poračuna skupno količino goriva, če se premaknemo v pozicijo x ; 1 step = 1 fuel*)
let cena1 poz seznam =
  let rec aux acc sez =
    match sez with
    | [] -> acc
    | x :: xs ->aux (abs(x - poz) + acc) xs
  in
  aux 0 seznam


let rec minimum1 poz1 poz2 min sez =
  let gorivo = min in 
  let gorivo' = cena1 poz1 sez
  and k = poz1 + 1 in
  if poz2 = poz1 then gorivo  else (
    if  gorivo <= gorivo' then (minimum1 k poz2 gorivo sez) else (minimum1 k poz2 gorivo' sez)
  ) 

(* Cena, če gremo v nič. *)
let y0 sez = cena1 0 sez

(*--------------------------------------------------------------------------------------------------------*)

(* Funkcija , ki določi stevilo premikov.*)

let seznam_premikov poz seznam =
  let rec aux acc p sez =
    match sez with
    | [] -> List.rev acc
    | x :: xs -> aux ((abs(x - p)) :: acc) p xs
  in
  aux [] poz seznam


(* Funkcija, ki določi ceno. *)
let cena poz sez =
  let x = seznam_premikov poz sez in
  let rec aux acc x' =
    match x' with
    | [] -> acc
    | y :: ys -> aux ( ((y * (y + 1)) / 2) + acc) ys
  in
  aux 0 x


let rec minimum2 poz1 poz2 min sez =
  let gorivo = min in 
  let gorivo' = cena poz1 sez
  and k = poz1 + 1 in
  if poz2 = poz1 then gorivo  else (
    if  gorivo <= gorivo' then (minimum2 k poz2 gorivo sez) else (minimum2 k poz2 gorivo' sez)
  ) 
  
(* Cena, če gremo v nič. *)
let y1 sez = cena 0 sez
  

let naloga1 vsebina_datoteke =string_of_int (minimum1 0 1000 (y0 vsebina_datoteke) vsebina_datoteke)
let naloga2 vsebina_datoteke =string_of_int (minimum2 0 1000 (y1 vsebina_datoteke) vsebina_datoteke)




let _ =
    let preberi_datoteko ime_datoteke =
    let chan = open_in_bin ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
    let chan = open_out_bin ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = read (preberi_datoteko ("day_7/day_7.in")) in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke 
    in
    izpisi_datoteko "day_7/day_7_1.out"  odgovor1;
    izpisi_datoteko "day_7/day_7_2.out"  odgovor2;
  
  


 (* let sez = [16;1;2;0;4;2;7;1;2;14] *)