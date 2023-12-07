type block_kind =
  | Empty
  | Symbol of char
  | Digit of int

type info_digit =
  { value : int
  ; is_first : bool
  ; surrounded_by : bool
  }

type block_kind_info =
  | None
  | InfoDigit of info_digit

let rec parse_grid grid =
  let line = read_line () in
  let parse c =
    match c with
    | '0' .. '9' -> Digit (int_of_char c - int_of_char '0')
    | '.' -> Empty
    | c -> Symbol c
  in
  match line with
  | "" -> grid
  | _ ->
    let grid_line = Array.of_seq (Seq.map parse (String.to_seq line)) in
    parse_grid (grid_line :: grid)
;;

let print_grid grid =
  let b2c b =
    match b with
    | Empty -> ' '
    | Symbol _ -> '*'
    | Digit v -> char_of_int (v + int_of_char '0')
  in
  let render_line line =
    let chars = Array.map b2c line in
    String.of_seq (Array.to_seq chars)
  in
  Array.iter (fun x -> print_endline (render_line x)) grid
;;

let surroundings = [ 0, -1; -1, -1; -1, 0; -1, 1; 0, 1; 1, 1; 1, 0; 1, -1 ]

let get_ij grid i j =
  try Array.get (Array.get grid i) j with
  | Invalid_argument _ -> Empty
;;

let get_info_grid grid =
  let check_first j line =
    try
      match Array.get line (j - 1) with
      | Digit _ -> false
      | _ -> true
    with
    | Invalid_argument _ -> true
  in
  let check_surrounded_by i j =
    let inner carry (x, y) =
      carry
      ||
      match get_ij grid (i + x) (j + y) with
      | Symbol _ -> true
      | _ -> false
    in
    List.fold_left inner false surroundings
  in
  let info_digit i j item line =
    match item with
    | Empty | Symbol _ -> None
    | Digit value ->
      let is_first = check_first j line in
      let surrounded_by = check_surrounded_by i j in
      InfoDigit { value; is_first; surrounded_by }
  in
  Array.mapi (fun i line -> Array.mapi (fun j v -> info_digit i j v line) line) grid
;;

let extract_info_grid grid =
  let flattened = Array.to_list (Array.concat (Array.to_list grid)) in
  let digits =
    List.filter
      (fun x ->
        match x with
        | None -> false
        | _ -> true)
      flattened
  in
  let rec scan inner acc =
    match inner with
    | InfoDigit v :: rest -> if v.is_first then acc else scan rest (v :: acc)
    | _ -> acc
  in
  let rec scan_digits digits acc =
    match digits with
    | InfoDigit v :: rest ->
      if v.is_first
      then scan_digits rest (scan rest [ v ] :: acc)
      else scan_digits rest acc
    | _ -> acc
  in
  let extracted = scan_digits digits [] in
  let valid =
    List.filter
      (fun x -> List.fold_left (fun c y -> c || y.surrounded_by) false x)
      extracted
  in
  let res =
    List.map
      (fun x -> List.fold_left (fun (i, y) x -> i * 10, y + (i * x.value)) (1, 0) x)
      valid
  in
  List.fold_left ( + ) 0 (List.map (fun (_, y) -> y) res)
;;

let print_info_grid grid =
  let b2c b =
    match b with
    | None -> ' '
    | InfoDigit { value; is_first; surrounded_by } ->
      if is_first then 'F' else if surrounded_by then 'S' else char_of_int value
  in
  let render_line line = String.of_seq (Array.to_seq (Array.map b2c line)) in
  Array.iter (fun x -> print_endline (render_line x)) grid
;;

let part1 () =
  let grid = Array.of_list (parse_grid []) in
  let grid2 = get_info_grid grid in
  extract_info_grid grid2
;;

let get_surrounding grid i j =
  List.map (fun (x, y) -> i + x, j + y, get_ij grid (i + x) (j + y)) surroundings
;;

let find_connections grid =
  Array.mapi
    (fun i line ->
      Array.mapi
        (fun j x ->
          match x with
          | Symbol '*' -> Some (get_surrounding grid i j)
          | _ -> None)
        line)
    grid
;;

let rec scan_left grid i j =
  match get_ij grid i (j - 1) with
  | Digit _ -> scan_left grid i (j - 1)
  | _ -> i, j
;;

let scan_right grid i j =
  let rec scan_right' grid i j =
    match get_ij grid i (j + 1) with
    | Digit x -> x :: scan_right' grid i (j + 1)
    | _ -> []
  in
  match get_ij grid i j with
  | Digit x -> x :: scan_right' grid i j
  | _ -> []
;;

let rec filter_duplicates l res =
  match l with
  | [] -> res
  | (bi, bj) :: rem ->
    (match res with
     | [] -> filter_duplicates rem [ bi, bj ]
     | (ai, aj) :: _ ->
       if ai == bi && aj == bj
       then filter_duplicates rem res
       else filter_duplicates rem ((bi, bj) :: res))
;;

let process_conns conns grid =
  let flattened = Array.to_list (Array.concat (Array.to_list conns)) in
  let filtered = List.filter_map (fun x -> x) flattened in
  let firsts =
    List.map
      (fun x ->
        List.filter_map
          (fun (i, j, y) ->
            match y with
            | Digit _ -> Some (scan_left grid i j)
            | _ -> None)
          x)
      filtered
  in
  let firsts = List.map (fun x -> filter_duplicates x []) firsts in
  let firsts = List.filter (fun x -> 2 == List.length x) firsts in
  let digits =
    List.map (fun x -> List.map (fun (i, j) -> scan_right grid i j) x) firsts
  in
  let digits =
    List.map
      (fun x ->
        List.map
          (fun y ->
            String.of_seq
              (List.to_seq (List.map (fun x -> char_of_int (x + int_of_char '0')) y)))
          x)
      digits
  in
  let digits = List.map (fun x -> List.map (fun y -> int_of_string y) x) digits in
  let digits = List.map (fun x -> List.fold_left Int.mul 1 x) digits in
  List.fold_left ( + ) 0 digits
;;

let part2 () =
  let grid = Array.of_list (parse_grid []) in
  let conns = find_connections grid in
  process_conns conns grid
;;
