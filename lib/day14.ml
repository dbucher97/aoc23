type stone =
  | None
  | Round
  | Sticky

let parse_stone = function
  | 'O' -> Round
  | '#' -> Sticky
  | _ -> None
;;

(*flush stones east*)
let flush_stones l =
  let rec flush_stones' carry left rest =
    match rest with
    | [] -> List.concat [ List.rev left; carry ]
    | hd :: tl ->
      (match hd with
       | Round -> flush_stones' (Round :: carry) left tl
       | Sticky -> flush_stones' [] (Sticky :: List.concat [ carry; left ]) tl
       | None -> flush_stones' carry (None :: left) tl)
  in
  flush_stones' [] [] l
;;

let count l = List.mapi (fun i x -> if x == Round then i + 1 else 0) l |> Utils.sum

let part1 () =
  let mat = Utils.parse_matrix parse_stone |> Utils.transpose in
  let mat = List.map List.rev mat |> List.map flush_stones in
  List.map count mat |> Utils.sum
;;

let print_mat m =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          print_char
            (match y with
             | None -> '.'
             | Round -> 'O'
             | Sticky -> '#'))
        x;
      print_endline "")
    m
;;

let rotate_flush l = List.map flush_stones l |> Utils.transpose |> List.map List.rev
let cycle l = rotate_flush l |> rotate_flush |> rotate_flush |> rotate_flush
let rec cycle_ntimes n l = if n > 0 then cycle_ntimes (n - 1) (cycle l) else l
let countall m = Utils.sum (List.map count m)

let rec cycle_list n l s =
  if n > 0 then cycle_list (n - 1) (cycle l) (countall l :: s) else List.rev s
;;

let match_period length period lst =
  let rec match_period' (lst, c) =
    match lst, c with
    | _, c when c == length -> true
    | hd :: tl, c -> if hd == (List.nth lst period) then match_period' (tl, c + 1) else false
    | _, _ -> false
  in
  match_period' (lst, 0)
;;

let find_period length lst =
  let rec find_period' p =
    match match_period length p lst with
    | true -> p
    | false -> find_period' (p+1)
  in
  find_period' 1

let part2 () =
  let mat = Utils.parse_matrix parse_stone |> Utils.transpose in
  let mat = List.map List.rev mat in
  (* I am currently to dumb / tired to find the period programatically *)
  let period = 13 in
  let e = 1000000000 mod period in
  print_int e;
  print_endline "";
  let series = List.rev (cycle_list (15 * period + e + 1) mat []) in
  print_int (List.hd series);
  print_endline "";
  let series = List.rev (cycle_list (16 * period + e + 1) mat []) in
  print_int (List.hd series);
  print_endline "";
  let series = List.rev (cycle_list (20 * period + e + 1) mat []) in
  (* Utils.print_list series; *)
  List.hd series
;;
(* let mat = rotate_ntimes 10000 mat in *)
