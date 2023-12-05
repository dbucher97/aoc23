exception Invalid_card

module IntSet = Set.Make (Int)

let parse_list str =
  String.split_on_char ' ' str
  |> List.filter_map (fun x ->
         match x with "" -> None | x -> Some (int_of_string x))

let inter_list a b = IntSet.inter (IntSet.of_list a) (IntSet.of_list b)

let parse_line line =
  let res =
    match String.split_on_char ':' line with
    | [ _; tail ] ->
        let inner =
          match String.split_on_char '|' tail with
          | [ winning; got ] ->
              IntSet.cardinal (inter_list (parse_list winning) (parse_list got))
          | _ -> raise Invalid_card
        in
        inner
    | _ -> raise Invalid_card
  in
  ((match res with 0 -> 0 | x -> Int.shift_left 1 (x - 1)), res)

let rec parse_cards score =
  match read_line () with
  | "" -> score
  | x ->
      let x, _ = parse_line x in
      parse_cards (score + x)

let extend_multipliers a b multi =
  let rec extend_multipliers' a b =
    if b == 0 then a
    else
      match a with
      | start :: rest -> (start + 1 + multi) :: extend_multipliers' rest (b - 1)
      | [] -> (1 + multi) :: extend_multipliers' [] (b - 1)
  in
  extend_multipliers' a b

let rec parse_cards_with_multi carry multipliers =
  let multi, rest =
    match multipliers with x :: rest -> (x, rest) | _ -> (0, [])
  in
  match read_line () with
  | "" -> carry
  | x ->
      let _, num = parse_line x in
      let multipliers = extend_multipliers rest num multi in
      parse_cards_with_multi (carry + multi + 1) multipliers

let part1 () = parse_cards 0
let part2 () = parse_cards_with_multi 0 []
