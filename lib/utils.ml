let print_list l =
  print_endline
    (match l with
     | [] -> ""
     | [ a ] -> string_of_int a
     | a :: rest ->
       List.fold_left (fun a b -> a ^ ", " ^ string_of_int b) (string_of_int a) rest)
;;

let print_lists l =
  print_endline
    (match l with
     | [] -> ""
     | [ a ] -> a
     | a :: rest -> List.fold_left (fun a b -> a ^ ", " ^ b) a rest)
;;

let sprint_tuple (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let print_list2 l =
  print_endline
    (match l with
     | [] -> ""
     | [ a ] -> sprint_tuple a
     | a :: rest ->
       List.fold_left (fun a b -> a ^ ", " ^ sprint_tuple b) (sprint_tuple a) rest)
;;

let rec parse_matrix f =
  match read_line () with
  | "" -> []
  | x -> (List.init (String.length x) (String.get x) |> List.map f) :: parse_matrix f
;;

let list_equal x y = List.combine x y |> List.for_all (fun (a, b) -> a == b)

let rec transpose = function
  | [] | [] :: _ -> []
  | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)
;;

let sum = List.fold_left (+) 0
