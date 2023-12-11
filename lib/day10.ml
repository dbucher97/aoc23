type pipe =
  | Vertical
  | Horizontal
  | Bend_NE
  | Bend_NW
  | Bend_SE
  | Bend_SW
  | None
  | Start

type direction =
  | Left
  | Right
  | Down
  | Up

let parse_pipe c =
  match c with
  | '|' -> Vertical
  | '-' -> Horizontal
  | 'L' -> Bend_NE
  | 'J' -> Bend_NW
  | '7' -> Bend_SW
  | 'F' -> Bend_SE
  | 'S' -> Start
  | _ -> None
;;

let parse_ground () =
  let rec parse_ground' f =
    match read_line () with
    | "" -> f
    | s ->
      let l = List.init (String.length s) (String.get s) in
      parse_ground' (List.map parse_pipe l :: f)
  in
  parse_ground' [] |> List.map Array.of_list |> List.rev |> Array.of_list
;;

exception StartNotFound

let find_start ground =
  let ijmap = Array.mapi (fun i x -> Array.mapi (fun j y -> i, j, y) x) ground in
  let ijmap = Array.to_list ijmap in
  match List.filter_map (Array.find_opt (fun (_, _, x) -> x == Start)) ijmap with
  | [ (y, x, _) ] -> y, x
  | _ -> raise StartNotFound
;;

let pos_equal (a1, a2) (b1, b2) = a1 == a2 && b1 == b2
let get_pipe ground (i, j) = Array.get (Array.get ground i) j

let turn_direction_cw dir =
  match dir with
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
;;

let turn_direction_acw dir =
  match dir with
  | Up -> Left
  | Left -> Down
  | Down -> Right
  | Right -> Up
;;

type state =
  { count : int
  ; direction : direction
  ; pos : int * int
  ; pipe : pipe
  }

exception CannotAdvance

let advance_state ground state =
  let y, x = state.pos in
  let pos =
    match state.direction with
    | Up -> y - 1, x
    | Down -> y + 1, x
    | Left -> y, x - 1
    | Right -> y, x + 1
  in
  (* let yi, xi = pos in *)
  (* Printf.printf "(%d, %d) -> (%d, %d)\n" y x yi xi; *)
  let pipe = get_pipe ground pos in
  let direction =
    match state.direction, pipe with
    | _, Vertical | _, Horizontal | _, Start -> state.direction
    | Left, Bend_NE | Down, Bend_NW | Up, Bend_SE | Right, Bend_SW ->
      turn_direction_cw state.direction
    | Down, Bend_NE | Right, Bend_NW | Left, Bend_SE | Up, Bend_SW ->
      turn_direction_acw state.direction
    | _ -> raise CannotAdvance
  in
  { count = state.count + 1; direction; pos; pipe }
;;

let count_loop_length ground start direction =
  let state = { count = 0; direction; pos = start; pipe = get_pipe ground start } in
  let rec count_loop_length' ground state =
    match state with
    | { count; pos; _ } when get_pipe ground pos == Start -> count
    | _ -> count_loop_length' ground (advance_state ground state)
  in
  count_loop_length' ground (advance_state ground state)
;;

let part1 () =
  let ground = parse_ground () in
  let start = find_start ground in
  let total_loop = count_loop_length ground start Right in
  print_int total_loop;
  print_endline "";
  total_loop / 2
;;

let get_loop ground start direction =
  let state = { count = 0; direction; pos = start; pipe = get_pipe ground start } in
  let rec count_loop' ground state loop =
    if get_pipe ground state.pos == Start
    then loop
    else count_loop' ground (advance_state ground state) (state.pos :: loop)
  in
  count_loop' ground (advance_state ground state) [ start ]
;;

type line_state =
  | FromAbove
  | FromBelow
  | Not

type scan_state =
  { crossing : int
  ; line : line_state
  ; on_line : bool
  }

exception ScanFailed

let advance_scan scan item is_loop =
  if not is_loop
  then { scan with on_line = false }
  else (
    match scan, item with
    | { crossing; line = Not; _ }, Vertical ->
      { crossing = crossing + 1; line = Not; on_line = true }
    | { crossing; line = Not; _ }, Bend_NE ->
      { crossing; line = FromAbove; on_line = true }
    | { crossing; line = Not; _ }, Bend_SE | { crossing; line = Not; _ }, Start ->
      { crossing; line = FromBelow; on_line = true }
    | { crossing; line = FromAbove; _ }, Horizontal ->
      { crossing; line = FromAbove; on_line = true }
    | { crossing; line = FromBelow; _ }, Horizontal ->
      { crossing; line = FromBelow; on_line = true }
    | { crossing; line = FromAbove; _ }, Bend_NW ->
      { crossing; line = Not; on_line = true }
    | { crossing; line = FromBelow; _ }, Bend_NW ->
      { crossing = crossing + 1; line = Not; on_line = true }
    | { crossing; line = FromBelow; _ }, Bend_SW ->
      { crossing; line = Not; on_line = true }
    | { crossing; line = FromAbove; _ }, Bend_SW ->
      { crossing = crossing + 1; line = Not; on_line = true }
    | _ -> raise ScanFailed)
;;

let inside_loop loop ground (la, lb) =
  let is_loop (y, x) = List.exists (fun (a, b) -> a == y && b == x) loop in
  let rec fill_line y x scan =
    if x == lb
    then []
    else (
      let scan = advance_scan scan (get_pipe ground (y, x)) (is_loop (y, x)) in
      let isodd =
        (match scan with
         | { crossing; line = Not; on_line = false } -> crossing mod 2
         | _ -> 0)
        == 1
      in
      isodd :: fill_line y (x + 1) scan)
  in
  let rec lines y =
    if y == la
    then []
    else fill_line y 0 { crossing = 0; line = Not; on_line = false } :: lines (y + 1)
  in
  lines 0
;;

let part2 () =
  let ground = parse_ground () in
  let start = find_start ground in
  let loop = get_loop ground start Right in
  let inside_loop =
    inside_loop loop ground (Array.length ground, Array.length (Array.get ground 0))
  in
  let pr_line l =
    List.map (fun x -> if x then "I" else " ") l |> List.fold_left ( ^ ) ""
  in
  List.iter (fun x -> print_endline (pr_line x)) inside_loop;
  let inside_flattened = List.concat inside_loop in
  List.fold_left (fun c x -> if x then c + 1 else c) 0 inside_flattened
;;
