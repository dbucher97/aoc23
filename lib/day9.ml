let rec diff a =
  match a with
  | a :: b :: rest -> (a - b) :: diff (b :: rest)
  | _ -> []
;;

let extrapolate ls =
  let rec extrapolate' ls carry =
    match ls with
    | st :: res -> extrapolate' res (carry + List.hd st)
    | [] -> carry
  in
  extrapolate' ls 0
;;

let rec difftree ls =
  if List.fold_left (fun c x -> c && x == 0) true (List.hd ls)
  then ls
  else difftree (diff (List.hd ls) :: ls)
;;

let rec parse_list l =
  match read_line () with
  | "" -> l
  | x ->
    let l2 = String.split_on_char ' ' x |> List.map int_of_string |> List.rev in
    parse_list (l2 :: l)
;;

let extrapolate_single l = extrapolate (difftree [ l ])
let extrapolate_single_left l = extrapolate (difftree [ List.rev l ])

let part1 () =
  let lists = parse_list [] in
  let res = List.map extrapolate_single lists in
  Utils.print_list res;
  List.fold_left (+) 0 res
;;

let part2 () =
  let lists = parse_list [] in
  let res = List.map extrapolate_single_left lists in
  Utils.print_list res;
  List.fold_left (+) 0 res
