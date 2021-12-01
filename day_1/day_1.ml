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
  

let ali_je_stevilo_vecje sez =
  let rec aux acc sez'=
    match sez' with
    | [] -> acc
    | x:: [] -> acc
    | x :: y :: ys -> if y > x then aux (y :: acc) (y :: ys) else aux acc (y :: ys)
   in
  aux [] sez

let seznam_vecjih  = ali_je_stevilo_vecje 

let dolzina_seznama sez = 
  let rec aux acc s =
    match s with
    | [] -> acc
    | x :: xs -> aux (1+ acc) xs
  in
  aux 0 sez

let koliko_stevil_je_vecjih sez   = dolzina_seznama ( seznam_vecjih sez )

let naloga1 vsebina_datoteke =  koliko_stevil_je_vecjih ( vsebina_datoteke) 

let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "day_1/day_1.in" in
  let odgovor1 = string_of_int(naloga1 vsebina_datoteke)  in
  izpisi_datoteko "day_1/day_1_1.out" odgovor1
 

