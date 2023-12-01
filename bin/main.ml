let explode_string s = List.init (String.length s) (String.get s);;

let get_digit c =
    let i0 = int_of_char '0' in
    match c with
    | '0'..'9' -> Some (int_of_char c - i0)
    | _ -> None

let match_no_written _ = None;;

let match_written_digit input =
    match input with
    | 'o' :: 'n' :: 'e' ::  _ -> Some 1
    | 't' :: 'w' :: 'o' ::  _ -> Some 2
    | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> Some 3
    | 'f' :: 'o' :: 'u' :: 'r' ::  _ -> Some 4
    | 'f' :: 'i' :: 'v' :: 'e' ::  _ -> Some 5
    | 's' :: 'i' :: 'x' ::  _ -> Some 6
    | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> Some 7
    | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> Some 8
    | 'n' :: 'i' :: 'n' :: 'e' :: _ -> Some 9
    | 'z' :: 'e' :: 'r' :: 'o' :: _ -> Some 0
    | _ -> None
;;

let match_written_digit_rev input =
    match input with
    | 'e' :: 'n' :: 'o' ::  _ -> Some 1
    | 'o' :: 'w' :: 't' ::  _ -> Some 2
    | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: _ -> Some 3
    | 'r' :: 'u' :: 'o' :: 'f' ::  _ -> Some 4
    | 'e' :: 'v' :: 'i' :: 'f' ::  _ -> Some 5
    | 'x' :: 'i' :: 's' ::  _ -> Some 6
    | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: _ -> Some 7
    | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: _ -> Some 8
    | 'e' :: 'n' :: 'i' :: 'n' :: _ -> Some 9
    | 'o' :: 'r' :: 'e' :: 'z' :: _ -> Some 0
    | _ -> None
;;

let rec first_from_left ?(written_digit = match_no_written) input =
    match written_digit input with
    | Some i -> i
    | None ->
        match input with
        | [] -> 0
        | x :: rest -> match get_digit x with
            | None -> first_from_left rest ~written_digit:written_digit
            | Some x -> x
;;

(* let rec day1a carry = *)
(*     let input = read_line () in *)
(*     match input with *)
(*     | "" -> carry *)
(*     | _ -> *)
(*         let input_exploded = explode_string input in *)
(*         let ffl = first_from_left input_exploded in *)
(*         let ffr = first_from_left (List.rev input_exploded) in *)
(*         day1a (carry + 10 * ffl + ffr) *)
(* ;; *)

let rec day1b carry =
    let input = read_line () in
    match input with
    | "" -> carry
    | _ ->
        let input_exploded = explode_string input in
        let ffl = first_from_left input_exploded ~written_digit:match_written_digit in
        let ffr = first_from_left (List.rev input_exploded) ~written_digit:match_written_digit_rev in
        day1b (carry + 10 * ffl + ffr)
;;

let () =
    print_int (day1b 0);
    print_endline "";
