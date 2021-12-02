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


(*-------------------------------------------------------------------------------------------------------------------------------------------*)

let loci_premik_in_stevilo_premikov sez =
  let rec aux acc sez' =
    match sez' with
    | [] -> List. rev acc
    | x :: xs ->  aux  ( (List.hd (String.split_on_char ' ' x),(int_of_string (List.hd (List.rev (String.split_on_char ' ' x))))) ::  acc) xs
  in
  aux [] sez

(*["forward 3"; "up 2"]*)



(*
["forward 4"; "down 7"; "down 4"; "forward 2"; "down 4"; "down 9"; "down 1"; "forward 1"; "down 4"; "forward 5"]
*)


let poisci_koncno_pozicijo sez =
  let rec aux acc1 acc2 sez' =
    match sez' with
    | [] -> (acc1,acc2)
    |(x, y) :: xs ->
      match x with
      | "forward" -> aux (y + acc1) acc2 xs
      | "up" -> aux acc1 (acc2 - y) xs
      | "down" -> aux acc1 (acc2 + y) xs
      | _ -> aux acc1 acc2 xs
    in
    aux 0 0 sez

let zmnozek (x, y) = x * y

let koncni_zmnozek_pozicije sez  = zmnozek (poisci_koncno_pozicijo( loci_premik_in_stevilo_premikov sez))



let naloga1 vsebina_datoteke =  koncni_zmnozek_pozicije( vsebina_datoteke) 

    
let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
    output_string chan vsebina;
    close_out chan
    in
    let vsebina_datoteke = read_file "day_2/day_2.in" in
    let odgovor1 = string_of_int (naloga1 vsebina_datoteke)
    in
    izpisi_datoteko "day_2/day_2_1.out" odgovor1 
