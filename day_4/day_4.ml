(* Funkcija, ki vrne seznam iger*)
let rec seznam_iger sez_iger igra seznam =
	match seznam with
	| [] -> List.rev sez_iger
	| "" :: xs -> seznam_iger (igra :: sez_iger) [] xs (* Če element enak "" potem dodamo igro v seznam iger in pripravimo prazen seznam za novo igro*)
	| vrstica :: xs -> seznam_iger sez_iger (vrstica :: igra) xs (* Če x ni enak "" pa dodamo vrstico v igro*)

(* Pomozna funkcija, ki odstrani elemente enake ""*)
let odstrani x = x <> ""


let read vsebina_datoteke =
    let dobitna_stevila :: igre = String.split_on_char '\n' (vsebina_datoteke ^"\n") in
    let igre' = List.map (List.map (String.split_on_char ' ')) (seznam_iger [] [] igre) in
    let igre'' =  List.map ( List.map (List.filter (odstrani) )) igre' in
    let igre''' = List.tl (List.map (List.map (List.map (int_of_string))) igre'' ) in
    let dobitna = List.map (int_of_string) (String.split_on_char ',' dobitna_stevila) in
    (dobitna , igre''')
     


(*Funkcija,ki  transponira matriko. Z njo bom dobila stolpce igre*)
let rec transponiraj matrika =
    let rec aux  acc mat =
      match mat with
      |  [] -> []
      | x :: xs -> if x = [] then acc else  List.map (List.hd) matrika :: (transponiraj (List.map List.tl matrika))
    in aux [] matrika

(*Funkcija, ki preveri ali smo v igri našli stolpec ali vrstico dobitnih števil. *)   
let resitev_igre igra =
 let rec poglej_vrstice igra =
        match igra with
        | [] -> false
        | x :: xs-> if x = [-1; -1; -1; -1; -1] then true else poglej_vrstice xs
      in
      let rec poglej_stolpce igra =
        let stolpci = transponiraj igra in
        match stolpci with
        | [] -> false
        | x :: xs -> if x = [-1; -1; -1; -1; -1]  then true else poglej_vrstice xs
    in
    poglej_vrstice igra || poglej_stolpce igra

(* Funkcija, ki vrne igro s številom -1 na mestu, kjer število ustreza dobitnemu stevilu *)

let dobitna_stevilka_v_igri dobitno_stevilo sez =
    List.map (List.map (List.map (function x -> if x = dobitno_stevilo then -1 else x))) sez
  
(* Funkcija, ki vrne vsoto igre *)
let vsota_igre igra =
    let igra' =  List.map (List.filter (function x -> x <> -1)) igra (* Odstranimo elemente enake -1*)
    in  
    List.fold_left (+) 0 ((List.map (List.fold_left (+) 0)) igra')  (* Naredimo vsoto tistega, kar je ostalo*)
  
(* Če igra ima resitev vrno vsoto sicer pa pogledamo drugo *)
let rec vrni_vsoto_igre igre =
	match igre with
	| [] -> 0
	| a :: b -> if resitev_igre a then vsota_igre a else vrni_vsoto_igre b

let rec rezultat dobitna_stevila igre  =
	match dobitna_stevila with
	| [] -> 0
	| a :: b -> let x = vrni_vsoto_igre (dobitna_stevilka_v_igri a igre) in
		if x <> 0 then a * x else rezultat  b (dobitna_stevilka_v_igri a igre) (* Zadnje stevilo, ki da resitev pomnožimo z vsoto*)

 
let naloga1 vsebina_datoteke =
     let (dobitna_stevila, igre) = vsebina_datoteke in 
     string_of_int (rezultat dobitna_stevila igre)




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
    let vsebina_datoteke = read (preberi_datoteko ("day_4/day_4.in")) in
    let odgovor1 = naloga1 vsebina_datoteke in
    izpisi_datoteko ("day_4/day_4_1.out") odgovor1

(* Vir: pomoč iz repozitorija: https://github.com/L3435/PROG1-Bonus*)