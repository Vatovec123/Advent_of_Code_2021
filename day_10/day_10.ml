let read vsebina =String.split_on_char '\n' (vsebina)

let niz_v_seznam niz =
  let d = String.length niz in
  let rec aux i seznam =
    if i < 0 then seznam else aux  (i-1) (niz.[i] :: seznam)
  in
  aux (d - 1) []

let rec najdi_napako_ali_pa_seznam_oklepajev_brez_zaklepajev seznam  acc=
  match seznam with
  | [] -> acc
  | x :: xs -> if x = '(' || x = '[' || x = '{' || x = '<' then najdi_napako_ali_pa_seznam_oklepajev_brez_zaklepajev xs (x :: acc) else (
    if ( x = ')' && ( List.hd acc = '(')) || (x = ']' && (List.hd acc = '[') )|| (x = '}' && (List.hd acc = '{') )|| (x = '>' && (List.hd acc = '<') )
    then najdi_napako_ali_pa_seznam_oklepajev_brez_zaklepajev xs (List.tl acc) else if x = '\r' then najdi_napako_ali_pa_seznam_oklepajev_brez_zaklepajev xs acc else [x]
)
  
let preveri_in_izracunaj1 seznam acc = if seznam = [')'] then (3 + acc) else if seznam = [']'] then (57 + acc) else
   if seznam = ['}'] then ( 1197+ acc) else if seznam = ['>'] then (25137 + acc) else 0

let doloci_vrednost_za_posamezno_vrstico niz = preveri_in_izracunaj1 (najdi_napako_ali_pa_seznam_oklepajev_brez_zaklepajev (niz_v_seznam niz) []) 0

let skupaj seznam_nizov =
  let rec aux acc sez =
    match sez with
    | [] -> acc
    | x :: xs -> aux (( doloci_vrednost_za_posamezno_vrstico x) + acc) xs
  in
  aux 0 seznam_nizov

(*---------------------------------------------------------------------------------------------------------------------------------------------*)
let preveri_in_izracunaj2 seznam  =
  let rec aux acc sez =
    match sez with
  | [] -> acc
  | x :: xs ->
    let y = (5 * acc) in
    (if x = '(' then aux (1 + y) xs else if x = '[' then aux (2 + y) xs else
    if x = '{' then aux (3 + y) xs else if x = '<' then aux (4 + y) xs else aux acc xs)
  in
  aux 0 seznam

let izracun_vrednosti_zaklepajev niz = preveri_in_izracunaj2 (najdi_napako_ali_pa_seznam_oklepajev_brez_zaklepajev (niz_v_seznam niz) [])

let skupaj2 seznam_nizov =
  let rec aux acc sez =
      match sez with
      | [] -> List.rev acc
      | x :: xs ->if izracun_vrednosti_zaklepajev x = 0 then ( aux acc xs) else aux (( izracun_vrednosti_zaklepajev x) :: acc) xs
    in
    aux [] seznam_nizov


let uredi seznam = List.sort compare seznam

let srednja_vrednost_seznama sez = 
  let srednja_vrednost = (List.length sez + 1) /2 in
  List.nth sez (srednja_vrednost -1)
 
let rezultat2 seznam  = srednja_vrednost_seznama ( uredi (skupaj2 seznam))

let naloga1 vsebina = string_of_int (skupaj vsebina)
let naloga2 vsebina = string_of_int (rezultat2 vsebina)


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
  let vsebina_datoteke = read (preberi_datoteko ("day_10/day_10.in")) in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_10/day_10_1.out" odgovor1;
  izpisi_datoteko "day_10/day_10_2.out" odgovor2;

(*
["{([(<{}[<>[]}>{[]{[(<()>\r"; "[[<[([]))<([[{}[[()]]]\r";"[{[{({}]{}}([{[{{{}}([]\r"; "[<(<(<(<{}))><([]([]()\r";"<{([([[(<>()){}]>(<<{{\r"]
*)
(*["[({(<(())[]>[[{[]{<()<>>\r";"[(()[<>])]({[<{<<[]>>(\r";      "(((({<>}<{<{<>}{[]{[]{}\r";"{<[[]]>}<{[{[{[]{()[[[]"; "<{([{{}}[<[[[<>{}]]]>[]]"]

*)

