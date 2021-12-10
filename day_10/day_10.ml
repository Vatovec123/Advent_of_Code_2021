let read vsebina =String.split_on_char '\n' (vsebina)

let niz_v_seznam niz =
  let d = String.length niz in
  let rec aux i seznam =
    if i < 0 then seznam else aux  (i-1) (niz.[i] :: seznam)
  in
  aux (d - 1) []

let rec najdi seznam  acc=
  match seznam with
  | [] -> acc
  | x :: xs -> if x = '(' || x = '[' || x = '{' || x = '<' then najdi xs (x :: acc) else (
    if ( x = ')' && ( List.hd acc = '(')) || (x = ']' && (List.hd acc = '[') )|| (x = '}' && (List.hd acc = '{') )|| (x = '>' && (List.hd acc = '<') )
    then najdi xs (List.tl acc) else [x]
)
  
let preveri_in_izracunaj sez acc = if sez = [')'] then (3 + acc) else if sez = [']'] then (57 + acc) else
   if sez = ['}'] then ( 1197+ acc) else if sez = ['>'] then (25137 + acc) else 0
 
let doloci_vrednost_za_posamezno_vrstico niz = preveri_in_izracunaj (najdi (niz_v_seznam niz) []) 0

let skupaj seznam_nizov =
  let rec aux acc sez =
    match sez with
    | [] -> acc
    | x :: xs -> aux (( doloci_vrednost_za_posamezno_vrstico x) + acc) xs
  in
  aux 0 seznam_nizov


let naloga1 vsebina = string_of_int (skupaj vsebina)


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
  in
  izpisi_datoteko "day_10/day_10_1.out" odgovor1;

(*
["{([(<{}[<>[]}>{[]{[(<()>\r"; "[[<[([]))<([[{}[[()]]]\r";"[{[{({}]{}}([{[{{{}}([]\r"; "[<(<(<(<{}))><([]([]()\r";"<{([([[(<>()){}]>(<<{{\r"]
*)