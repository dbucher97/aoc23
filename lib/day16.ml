type dir =
  | North
  | West
  | South
  | East

type mirror =
  | NWSE
  | NESW

type splitter =
  | Horizontal
  | Vertical

type block =
  | Empty
  | Splitter of splitter
  | Mirror of mirror

let parse_char c =
  match c with
  | '/' -> Mirror NWSE
  | '\\' -> Mirror NESW
  | '-' -> Splitter Horizontal
  | '|' -> Splitter Vertical
  | _ -> Empty
;;

let advance_beam (x, y, direction) =
  match direction with
  | North -> x, y - 1, direction
  | East -> x + 1, y, direction
  | West -> x - 1, y, direction
  | South -> x, y + 1, direction
;;

let get_xy field (x, y) =
  match x, y with
  | x, y when x < 0 || y < 0 -> None
  | _ -> Option.bind (List.nth_opt field y) (fun l -> List.nth_opt l x)
;;

let mirror d direction =
  match d with
  | NESW ->
    (match direction with
     | North -> West
     | West -> North
     | South -> East
     | East -> South)
  | NWSE ->
    (match direction with
     | North -> East
     | West -> South
     | South -> West
     | East -> North)
;;

let rec follow_beam field (x, y, direction) beam =
  let x, y, direction = advance_beam (x, y, direction) in
  (* Printf.printf "%d, %d \n" x y; *)
  match
    List.find_opt (fun (ax, ay, ad) -> ax == x && ay == y && ad == direction) beam
  with
  | None ->
    let op = get_xy field (x, y) in
    (match op with
     | None -> beam
     | Some k ->
       (match k, direction with
        | Mirror d, _ ->
          follow_beam field (x, y, mirror d direction) ((x, y, direction) :: beam)
        | Splitter Vertical, East | Splitter Vertical, West ->
          follow_beam
            field
            (x, y, North)
            (follow_beam field (x, y, South) ((x, y, direction) :: beam))
        | Splitter Horizontal, North | Splitter Horizontal, South ->
          follow_beam
            field
            (x, y, East)
            (follow_beam field (x, y, West) ((x, y, direction) :: beam))
        | _ -> follow_beam field (x, y, direction) ((x, y, direction) :: beam)))
  | Some _ -> beam
;;

let rec follow_beam_memo field tbl pos beam =
  let follow_beam_inner field (x, y, direction) beam =
    let x, y, direction = advance_beam (x, y, direction) in
    (* Printf.printf "%d, %d \n" x y; *)
    match
      List.find_opt (fun (ax, ay, ad) -> ax == x && ay == y && ad == direction) beam
    with
    | None ->
      let op = get_xy field (x, y) in
      (match op with
       | None -> beam
       | Some k ->
         (match k, direction with
          | Mirror d, _ ->
            follow_beam_memo
              field
              tbl
              (x, y, mirror d direction)
              ((x, y, direction) :: beam)
          | Splitter Vertical, East | Splitter Vertical, West ->
            follow_beam_memo
              field
              tbl
              (x, y, North)
              (follow_beam_memo field tbl (x, y, South) ((x, y, direction) :: beam))
          | Splitter Horizontal, North | Splitter Horizontal, South ->
            follow_beam_memo
              field
              tbl
              (x, y, East)
              (follow_beam_memo field tbl (x, y, West) ((x, y, direction) :: beam))
          | _ -> follow_beam_memo field tbl (x, y, direction) ((x, y, direction) :: beam)))
    | Some _ -> beam
  in
  match Hashtbl.find_opt tbl pos with
  | None ->
    let inner = follow_beam_inner field pos beam in
    Hashtbl.add tbl pos inner;
    inner
  | Some x -> x
;;

module IntTuple = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with
    | 0 -> compare y0 y1
    | c -> c
  ;;
end

module TupleSet = Set.Make (IntTuple)

let part1 () =
  let field = Utils.parse_matrix parse_char in
  let l = follow_beam field (-1, 0, East) [] in
  let s = List.map (fun (x, y, _) -> x, y) l in
  TupleSet.of_list s |> TupleSet.cardinal
;;

let get_energized field tbl initial =
  let l = follow_beam_memo field tbl initial [] in
  let s = List.map (fun (x, y, _) -> x, y) l in
  TupleSet.of_list s |> TupleSet.cardinal
;;

let part2 () =
  let field = Utils.parse_matrix parse_char in
  let rows = List.length field in
  let cols = List.length (List.hd field) in
  let initials =
    List.concat
      [ List.init rows (fun i -> -1, i, East)
      ; List.init rows (fun i -> cols, i, West)
      ; List.init cols (fun i -> i, -1, South)
      ; List.init cols (fun i -> i, rows, North)
      ]
  in
  let tbl = Hashtbl.create 1024 in
  List.map (get_energized field tbl) initials
  |> List.fold_left (fun c x -> if x > c then x else c) 0
;;
