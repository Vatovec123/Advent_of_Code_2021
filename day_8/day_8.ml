let rec loci_vzorce_in_izhodne_vrednosti sez1 sez2 sez =
  match sez with 
  | [] -> List.rev (sez2 ::sez1)
  | "|" :: xs -> loci_vzorce_in_izhodne_vrednosti  (sez2 :: sez1) [] xs
  | x :: xs -> loci_vzorce_in_izhodne_vrednosti sez1 (x :: sez2) xs


let read vsebina =
  vsebina
  |> String.split_on_char '\n'
  |> List.map  (String.split_on_char ' ')
  |> List.map (loci_vzorce_in_izhodne_vrednosti [] [])

let je_enostavna x = String.length x =  2 || String.length x= 3 || String.length x= 4 || String.length x= 7
let rec poisci x sez = 
  match sez with
  | [] -> false
  | y :: ys -> if String.length x =String.length y then true else poisci x ys

let rec prestej seznam_dveh_seznamov =
  let vzorci = List.hd seznam_dveh_seznamov 
  and izhodne_vrednosti = List.hd (List.tl seznam_dveh_seznamov) in
  let rec aux acc izhodne_vrednosti' =
  match izhodne_vrednosti' with
  | [] -> acc
  | x :: xs -> if ( (je_enostavna x) && (poisci x vzorci))  then aux (1 + acc) xs else aux acc xs
  in
  aux 0 izhodne_vrednosti


let rec skupaj sez =
  let rec aux  acc sez' =
    match sez' with
    | [] -> acc
    | x :: xs -> aux ((prestej x) + acc) xs
  in
  aux 0 sez

let naloga1 vsebina = string_of_int (skupaj (vsebina))





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
  let vsebina_datoteke = read (preberi_datoteko ("day_8/day_8.in")) in
  let odgovor1 = naloga1 vsebina_datoteke
  in
  izpisi_datoteko "day_8/day_8_1.out" odgovor1;

