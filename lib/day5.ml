exception ParseSeedsError
exception ParseMapError

let parse_seeds () =
  match String.split_on_char ' ' (read_line ()) with
  | _ :: rest -> List.map int_of_string rest
  | _ -> []

type map_item = { f : int; t : int; range : int }

let rec parse_map items =
  match String.split_on_char ' ' (read_line ()) with
  | [ a; b; c ] ->
      parse_map
        ({ f = int_of_string a; t = int_of_string b; range = int_of_string c }
        :: items)
  | _ -> items

let parse_maps () =
  let rec parse_maps' maps =
    match String.split_on_char ' ' (read_line ()) with
    | [ _; "map:" ] -> parse_maps' (parse_map [] :: maps)
    | _ -> maps
  in
  List.rev (parse_maps' [])

let print_map map =
  List.iter
    (fun { f; t; range } ->
      print_endline (Printf.sprintf "f: %d, t: %d, r: %d" f t range))
    map

let rec follow i map =
  match map with
  | { f; t; range } :: rest -> (
      match i - t with
      | x when x >= 0 && x < range -> x + f
      | _ -> follow i rest)
  | [] -> i

let rec follow_all maps i =
  (* Printf.printf "%d\n" i; *)
  match maps with [] -> i | map :: rest -> follow_all rest (follow i map)

let part1 () =
  let seeds = parse_seeds () in
  let _ = read_line () in

  let maps = parse_maps () in

  let locs = List.map (follow_all maps) seeds in

  List.fold_left min (List.nth locs 0) locs

let parse_seed_ranges () =
  let seeds = parse_seeds () in

  let rec parse_seed_ranges' l =
    match l with a :: b :: rest -> (a, b) :: parse_seed_ranges' rest | _ -> []
  in

  parse_seed_ranges' seeds

let is_sub x1 w1 x2 w2 = x1 >= x2 && x1 + w1 <= x2 + w2
let is_left x1 w1 x2 w2 = x1 < x2 && x1 + w1 <= x2 + w2 && x1 + w1 >= x2
let is_right x1 w1 x2 w2 = x1 >= x2 && x1 + w1 > x2 + w2 && x1 <= x2 + w2

let rec range_follow map (start, n) =
  match map with
  | { f; t; range } :: rest -> (
      match start - t with
      | x when is_sub start n t range -> [ (x + f, n) ]
      | x when is_left start n t range ->
          (f, n + x) :: range_follow rest (start, -x)
      | x when is_right start n t range ->
          (x + f, range - x) :: range_follow rest (t + range, x + n - range)
      | _ -> range_follow rest (start, n))
  | [] -> [ (start, n) ]

let range_follow_multi sources map =
  List.concat (List.map (fun s -> range_follow map s) sources)

let range_follow_all maps sources =
  List.fold_left range_follow_multi sources maps

let part2 () =
  let seeds = parse_seed_ranges () in
  let _ = read_line () in
  let maps = parse_maps () in

  let resp = range_follow_all maps seeds in
  (* Utils.print_list2 resp; *)
  let a, _ = List.split resp in

  List.fold_left (min) 100000000000000000 a
