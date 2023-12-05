type set = { r : int; g : int; b : int }

(* let show_set {r; g; b} =
    Printf.sprintf "{r: %d, g: %d; b: %d}" r g b *)

let max_set = { r = 12; g = 13; b = 14 }

let parse_set set_str =
  let rec parse_set' set_list carry =
    match set_list with
    | x :: rest ->
        let new_ret =
          match String.split_on_char ' ' x with
          | _ :: amount :: color :: _ ->
              let new_ret =
                match color with
                | "red" ->
                    { r = int_of_string amount; b = carry.b; g = carry.g }
                | "green" ->
                    { g = int_of_string amount; b = carry.b; r = carry.r }
                | "blue" ->
                    { b = int_of_string amount; g = carry.g; r = carry.r }
                | _ -> carry
              in
              new_ret
          | _ -> carry
        in
        parse_set' rest new_ret
    | _ -> carry
  in
  parse_set' (String.split_on_char ',' set_str) { r = 0; b = 0; g = 0 }

let set_ok carry set =
  carry && set.g <= max_set.g && set.r <= max_set.r && set.b <= max_set.b

let max_set carry set =
  { r = max carry.r set.r; b = max carry.b set.b; g = max carry.g set.g }

let check_line line =
  match String.split_on_char ':' line with
  | [ game; rest ] ->
      let id =
        match String.split_on_char ' ' game with
        | [ _; id_str ] -> int_of_string id_str
        | _ -> 0
      in
      let sets = List.map parse_set (String.split_on_char ';' rest) in
      let sets_ok = List.fold_left set_ok true in
      let resp = match sets_ok sets with true -> Some id | false -> None in
      resp
  | _ -> None

let power_set_line line =
  match String.split_on_char ':' line with
  | [ _; rest ] ->
      let sets = List.map parse_set (String.split_on_char ';' rest) in
      let max_set = List.fold_left max_set { r = 0; b = 0; g = 0 } sets in
      max_set.r * max_set.g * max_set.b
  | _ -> 0

let part1 () =
  let rec part1' carry =
    match read_line () with
    | "" -> carry
    | x -> (
        match check_line x with
        | None -> part1' carry
        | Some x -> part1' (carry + x))
  in
  part1' 0

let part2 () =
  let rec part2' carry =
    match read_line () with
    | "" -> carry
    | x -> part2' (carry + power_set_line x)
  in
  part2' 0
