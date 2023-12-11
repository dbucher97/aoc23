let rec transpose = function
  | [] | [] :: _ -> []
  | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)
;;

type univ =
  | Empty
  | Galaxy

let rec double_empty x =
  match x with
  | [] -> []
  | x :: rest ->
    if List.for_all (fun x -> x == Empty) x
    then x :: x :: double_empty rest
    else x :: double_empty rest
;;

let parse_univ c =
  match c with
  | '#' -> Galaxy
  | _ -> Empty
;;

let print_univ c =
  match c with
  | Galaxy -> print_char '#'
  | _ -> print_char '.'
;;

let extract_pos l =
  let pos =
    List.mapi
      (fun i x ->
        List.mapi
          (fun j y ->
            match y with
            | Galaxy -> Some (i, j)
            | Empty -> None)
          x)
      l
  in
  List.concat pos |> List.filter_map (fun x -> x)
;;

let combinations l =
  let rec combinations' l =
    match l with
    | [] -> []
    | hd :: tl -> List.map (fun x -> hd, x) tl :: combinations' tl
  in
  List.concat (combinations' l)
;;

let part1 () =
  let field = Utils.parse_matrix parse_univ |> double_empty in
  let field = transpose field |> double_empty |> transpose in
  (* List.iter *)
  (*   (fun x -> *)
  (*     List.iter print_univ x; *)
  (*     print_endline "") *)
  (*   field; *)
  let pos = extract_pos field in
  let comb = combinations pos in
  let lengths =
    List.map (fun ((x1, y1), (x2, y2)) -> Int.abs (x1 - x2) + Int.abs (y1 - y2)) comb
  in
  (* Utils.print_list lengths; *)
  List.fold_left ( + ) 0 lengths
;;

let rec find_empty_rows x i =
  match x with
  | [] -> []
  | x :: rest ->
    if List.for_all (fun x -> x == Empty) x
    then i :: find_empty_rows rest (i + 1)
    else find_empty_rows rest (i + 1)
;;

let count_list l a b =
  let xa = min a b in
  let xb = max a b in
  List.filter (fun x -> x > xa) l |> List.filter (fun x -> x < xb) |> List.length
;;

let count_empty_rows rows (x1, _) (x2, _) = count_list rows x1 x2
let count_empty_cols cols (_, y1) (_, y2) = count_list cols y1 y2

let part2 () =
  let field = Utils.parse_matrix parse_univ in
  let rows = find_empty_rows field 0 in
  let cols = find_empty_rows (transpose field) 0 in
  let mult = 1000000 in
  let pos = extract_pos field in
  let comb = combinations pos in
  let lengths =
    List.map (fun ((x1, y1), (x2, y2)) -> Int.abs (x1 - x2) + Int.abs (y1 - y2)) comb
  in
  let empty_crossings =
    List.map (fun (a, b) -> count_empty_cols cols a b + count_empty_rows rows a b) comb
  in
  let lengths =
    List.map (fun (a, b) -> a + ((mult - 1) * b)) (List.combine lengths empty_crossings)
  in
  List.fold_left ( + ) 0 lengths
;;
