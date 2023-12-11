exception InputError
exception TooManySteps

module Dict = Map.Make (String)

type dir =
  | Left
  | Right

let parse_line () =
  let line = read_line () in
  match String.split_on_char ' ' line with
  | [ n; "="; a; b ] -> Some (n, String.sub a 1 3, String.sub b 0 3)
  | _ -> None
;;

let parse_map () =
  let rec parse_map' l =
    match parse_line () with
    | Some (n, a, b) -> parse_map' (Dict.add n (a, b) l)
    | None -> l
  in
  parse_map' Dict.empty |> Dict.to_seq |> Hashtbl.of_seq
;;

let follow_dir map ins current =
  let l, r = Hashtbl.find map current in
  match ins with
  | Left -> l
  | Right -> r
;;

type state =
  { current : string
  ; goal : string
  ; map : (string, string * string) Hashtbl.t
  ; dirs : dir list
  ; step : int
  }

let rec follow_dirs state =
  let rec follow_dirs' state =
    if String.equal state.current state.goal
    then state
    else (
      match state.dirs with
      | d :: rest ->
        let next = follow_dir state.map d state.current in
        follow_dirs' { state with current = next; step = state.step + 1; dirs = rest }
      | [] -> state)
  in
  match follow_dirs' state with
  | { step; _ } when step > 100000 -> raise TooManySteps
  | { current; goal; step; _ } when String.equal current goal -> step
  | { current; step; _ } -> follow_dirs { state with current; step }
;;

let parse_dirs () =
  let line = read_line () in
  List.init (String.length line) (String.get line)
  |> List.map (fun x ->
    match x with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> raise InputError)
;;

let part1 () =
  let dirs = parse_dirs () in
  let _ = read_line () in
  let map = parse_map () in
  follow_dirs { current = "AAA"; goal = "ZZZ"; map; dirs; step = 0 }
;;

type state2 =
  { current : string list
  ; map : (string, string * string) Hashtbl.t
  ; dirs : dir list
  ; step : int
  }

let rec follow_dirs2 state =
  let is_ok current =
    List.fold_left (fun c x -> c && String.ends_with x ~suffix:"Z") true current
  in
  let rec follow_dirs2' state =
    if is_ok state.current
    then state
    else (
      match state.dirs with
      | d :: rest ->
        let next = List.map (follow_dir state.map d) state.current in
        follow_dirs2' { state with current = next; step = state.step + 1; dirs = rest }
      | [] -> state)
  in
  match follow_dirs2' state with
  | { step; _ } when step > 100000000 -> raise TooManySteps
  | { current; step; _ } when is_ok current -> step
  | { current; step; _ } -> follow_dirs2 { state with current; step }
;;

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a * b / gcd a b

type state3 =
  { current : string
  ; map : (string, string * string) Hashtbl.t
  ; dirs : dir Seq.t
  ; step : int
  }

let follow_dirs3 state =
  let is_ok = String.ends_with ~suffix:"Z" in
  let rec follow_dirs3' state =
    if is_ok state.current
    then state.step
    else (
      match Seq.uncons state.dirs with
      | Some (dir, rest) ->
        let next = follow_dir state.map dir state.current in
        follow_dirs3' { state with current = next; dirs = rest; step = state.step + 1 }
      | None -> state.step)
  in
  follow_dirs3' state
;;

(* let get_min_step periods = *)
(*   while  *)

let part2 () =
  let dirs = parse_dirs () in
  let _ = read_line () in
  let map = parse_map () in
  (* Utils.print_lists starts; *)
  let starts = List.of_seq (Hashtbl.to_seq_keys map) in
  let starts = List.filter (fun x -> String.ends_with x ~suffix:"A") starts in
  let dirs = Seq.cycle (List.to_seq dirs) in
  let period start = follow_dirs3 { current = start; map; dirs; step = 0 } in
  List.map period (List.tl starts)
  |> List.fold_left (fun a b -> lcm a b) (period (List.hd starts))
;;
