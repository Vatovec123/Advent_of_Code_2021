let read vsebina_datoteke = List.map (int_of_string) (String.split_on_char ',' (vsebina_datoteke))

let stevilo_nicel seznam = List.length ( List.find_all (function x -> x =0) seznam)

let rec dodaj_elemente x i seznam = if i = 0 then seznam else dodaj_elemente x (i-1) (x :: seznam )

let spremeni stanje =
  let rec aux acc sez =
    match sez with
    | []-> acc
    | x :: xs -> if x = 0 then aux  (8 :: 6 :: acc) xs else aux ((x-1) :: acc) xs
  in
  aux [] stanje
    


let rec po_i_dnevih stanje i =  if i = 0 then stanje else po_i_dnevih (spremeni stanje ) (i-1)
 
let naloga1 vsebina_datoteke = string_of_int (List.length (po_i_dnevih vsebina_datoteke 80))


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
  let vsebina_datoteke = read (preberi_datoteko ("day_6/day_6.in")) in
  let odgovor1 = naloga1 vsebina_datoteke 
  in
  izpisi_datoteko "day_6/day_6_1.out"  odgovor1;



