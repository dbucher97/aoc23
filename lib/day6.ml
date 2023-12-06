exception Invalid_race

let calc_range total i = i * (total - i)
let distances t = List.init t (calc_range t)

let parse_race () =
  let fmap s = match s with "" -> None | x -> Some (int_of_string x) in
  let times =
    match String.split_on_char ' ' (read_line ()) with
    | "Time:" :: rest -> List.filter_map fmap rest
    | _ -> raise Invalid_race
  in
  let distances =
    match String.split_on_char ' ' (read_line ()) with
    | "Distance:" :: rest -> List.filter_map fmap rest
    | _ -> raise Invalid_race
  in
  List.combine times distances

let parse_race2 () =
  let race = parse_race () in
  let a, b = List.split race in
  let time =
    List.map string_of_int a |> List.fold_left ( ^ ) "" |> int_of_string
  in
  let distance =
    List.map string_of_int b |> List.fold_left ( ^ ) "" |> int_of_string
  in
  time, distance

let options (t, distance) =
  let l = distances t in
  let l = List.filter (fun x -> x > distance) l in
  List.length l

let part1 () =
  let race = parse_race () in
  let all_options = List.map options race in
  List.fold_left Int.mul 1 all_options

let part2 () =
  let time, distance = parse_race2 () in
  options (time, distance)
