let rec parse_input f =
  try
    match String.split_on_char ',' (input_line f) with
    | [ "" ] | [] -> []
    | l ->
      List.concat
        [ List.map (fun x -> List.init (String.length x) (String.get x)) l
        ; parse_input f
        ]
  with
  | End_of_file -> []
;;

let rec consume h l =
  match l with
  | [] -> h
  | x :: xs ->
    let h = h + int_of_char x in
    consume (h * 17 mod 256) xs
;;

let hash str = List.init (String.length str) (String.get str) |> consume 0

let part1 () =
  let f = open_in "data/input15.txt" in
  let inputs = parse_input f in
  List.map (consume 0) inputs |> Utils.sum
;;

exception PIError

type instruction =
  | Add of int
  | Remove

let parse_instruction str =
  let len = String.length str in
  let c = String.get str (len - 1) in
  match c with
  | '-' -> String.sub str 0 (len - 1), Remove
  | '1' .. '9' -> String.sub str 0 (len - 2), Add (int_of_char c - int_of_char '0')
  | _ -> raise PIError
;;

let rec parse_input2 f =
  try
    match String.split_on_char ',' (input_line f) with
    | [ "" ] | [] -> []
    | l -> List.concat [ List.map parse_instruction l; parse_input2 f ]
  with
  | End_of_file -> []
;;

let apply_instruction state (label, instr) =
  let h = hash label in
  let rec add fl lst =
    match lst with
    | (l, _) :: xs when String.equal l label -> (l, fl) :: xs
    | x :: xs -> x :: add fl xs
    | [] -> [ label, fl ]
  in
  let func =
    match instr with
    | Remove ->
        Printf.printf "Remove %s %d\n" label h;
        List.filter (fun (l, _) -> not (String.equal l label))
    | Add f ->
        Printf.printf "Add %s %d %d\n" label h f;
        add f
  in
  List.mapi
    (fun i x ->
      match i, x with
      | i, x when i == h -> func x
      | _ -> x)
    state
;;

let compute_box lst =
  let c, _ = List.fold_left (fun (c, i) (_, f) -> c + (f * i), i + 1) (0, 1) lst in
  c
;;

let rec compute_score state i =
  match state with
  | x :: xs -> 
      (* Printf.printf "Box %d: %d\n" i (List.length x); *)
      (i * compute_box x) + compute_score xs (i + 1)
  | [] -> 0
;;

let part2 () =
  let f = open_in "data/input15.txt" in
  let inputs = parse_input2 f in
  let state = List.init 256 (fun _ -> []) in
  let state = List.fold_left apply_instruction state inputs in
  compute_score state 1
;;
