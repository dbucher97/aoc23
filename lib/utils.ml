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
     | a :: rest ->
       List.fold_left (fun a b -> a ^ ", " ^ b) a rest)
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
