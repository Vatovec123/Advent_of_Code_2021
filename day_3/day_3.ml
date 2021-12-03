let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
(* Povezava do spletne strani, iz katere je prepisana zgornja koda: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)   

(*----------------------------------------------------------------------------------------------------------------------------------------*)
let niz_v_seznam_stevk niz =
    let rec aux acc i  = if i < 0 then acc else aux (niz.[i] :: acc) (i -1) in
    aux [] (String.length  niz -1)

let seznam_seznamov_stevk sez = 
  let rec aux acc sez' =
    match sez' with
    | [] -> acc
    | x :: xs -> aux(( niz_v_seznam_stevk x) :: acc) xs 
  in
  aux [] sez

(* ["00100"; "11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"] 
let primer = seznam_seznamov_stevk ["00100"; "11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"];;
let vsote = seznam_vsot primer;;
let rate = epsilon_gamma_rate vsote;;
*)

let vsota_stolpca sez i =
  let rec aux acc sez' =
    match sez' with
    | []-> acc
    | x :: xs -> if (List.nth x i) = '1' then  aux (1 + acc) xs else aux acc xs
  in
  aux 0 sez

let seznam_vsot' sez =
  let rec aux acc i = if i < 0 then acc else aux (( vsota_stolpca sez i) :: acc) (i-1) 
  in
  aux [] (List.length (List.hd sez) -1) 

let seznam_vsot sez  = seznam_vsot'( seznam_seznamov_stevk sez)

let epsilon_gamma sez =
  let rec aux acc1 acc2 sez' =
    match sez' with
   | [] -> (List.rev acc1,List.rev acc2)
   | x :: xs -> if (x > 500) then (aux ( 1 :: acc1)  (0 :: acc2) xs) else aux (0 :: acc1 )  (1:: acc2) xs
  in
  aux [] [] sez


let gamma_v_decimalnem_zapisu (x, y) =
  let rec aux acc i sez' =
    match sez' with
    | []-> acc
    | x :: xs -> if (i < 0.) then acc else aux  ((x *( int_of_float( 2. ** i))) + acc)  (i -. 1.) xs
  in
  aux 0 (float_of_int(List.length x )-. 1.) x

let epsilon_v_decimalnem_zapisu (x, y) =
  let rec aux acc i sez' =
    match sez' with
    | []-> acc
    | y :: ys -> if (i < 0.) then acc else aux  ((y *( int_of_float( 2. ** i))) + acc)  (i -. 1.) ys
  in
  aux 0 (float_of_int(List.length x )-. 1.) y

let zmnozek_gamme_in_epsilona'  (x, y) = epsilon_v_decimalnem_zapisu (x, y) * gamma_v_decimalnem_zapisu (x, y)

let zmnozek_gamme_in_epsilona sez  = zmnozek_gamme_in_epsilona' (epsilon_gamma (seznam_vsot sez))


let naloga1 vsebina_datoteke =  zmnozek_gamme_in_epsilona (vsebina_datoteke) 

let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
    output_string chan vsebina;
    close_out chan
    in
    let vsebina_datoteke = read_file "day_3/day_3.in" in
    let odgovor1 = string_of_int (naloga1 vsebina_datoteke)
    in
    izpisi_datoteko "day_3/day_3_1.out" odgovor1;


