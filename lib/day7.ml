let card_to_score c =
  match c with
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | '2' .. '9' -> int_of_char c - int_of_char '0'
  | _ -> 0

type card_type = Joker | Card of int | None

module IntSet = Set.Make (Int)

let count l v =
  List.fold_left (fun c x -> match x with _ when v == x -> c + 1 | _ -> c) 0 l

let occs s l = List.map (fun x -> count l x) s

let extend_hand_score hand =
  let set = IntSet.of_list hand in
  let set = IntSet.to_seq set |> List.of_seq in
  let occs = occs set hand |> List.sort compare |> List.rev in
  let score =
    match occs with
    | [ 5 ] -> 6
    | [ 4; 1 ] -> 5
    | [ 3; 2 ] -> 4
    | [ 3; 1; 1 ] -> 3
    | [ 2; 2; 1 ] -> 2
    | [ 2; 1; 1; 1 ] -> 1
    | _ -> 0
  in
  (* Utils.print_list (score :: hand); *)
  score :: hand

let parse_cards cards =
  List.init (String.length cards) (String.get cards) |> List.map card_to_score

let rec compare_hands h1 h2 =
  match (h1, h2) with
  | s1 :: r1, s2 :: r2 -> (
      match compare s1 s2 with x when x == 0 -> compare_hands r1 r2 | x -> x)
  | _ -> 0

let rec parse_players () =
  match String.split_on_char ' ' (read_line ()) with
  | [ cards; amount ] ->
      (parse_cards cards, int_of_string amount) :: parse_players ()
  | _ -> []

let part1 () =
  let players = parse_players () in
  let players = List.map (fun (x, y) -> (extend_hand_score x, y)) players in

  let players = List.sort (fun (x, _) (y, _) -> compare_hands x y) players in
  (* let _, y = List.split players in
     Utils.print_list y; *)
  let a, _ =
    List.fold_left (fun (c, r) (_, s) -> (c + (s * r), r + 1)) (0, 1) players
  in
  a

let extend_hand_score2 hand =
  let clean_hand =
    List.filter_map (fun x -> match x with 11 -> None | x -> Some x) hand
  in
  let num_jokers =
    List.fold_left (fun c x -> match x with 11 -> c + 1 | _ -> c) 0 hand
  in
  let set = IntSet.of_list clean_hand in
  let set = IntSet.to_seq set |> List.of_seq in
  let occs = occs set hand |> List.sort compare |> List.rev in
  let occs =
    match occs with s :: rest -> (s + num_jokers) :: rest | _ -> []
  in
  let score =
    match occs with
    | [ 5 ] -> 6
    | [ 4; 1 ] -> 5
    | [ 3; 2 ] -> 4
    | [ 3; 1; 1 ] -> 3
    | [ 2; 2; 1 ] -> 2
    | [ 2; 1; 1; 1 ] -> 1
    | [] -> 6
    | _ -> 0
  in
  let ret = score :: (List.map (fun x -> if x == 11 then 1 else x) hand) in
  ret

let part2 () =
  let players = parse_players () in
  let players = List.map (fun (x, y) -> (extend_hand_score2 x, y)) players in

  let players = List.sort (fun (x, _) (y, _) -> compare_hands x y) players in
  let x, _ = List.split players in
  List.iter Utils.print_list x;
  let a, _ =
    List.fold_left (fun (c, r) (_, s) -> (c + (s * r), r + 1)) (0, 1) players
  in
  a
