type spring =
  | Operational
  | Damaged
  | Unknown

type spring_array =
  { springs : spring list
  ; nums : int list
  }

let parse_state = function
  | '#' -> Damaged
  | '.' -> Operational
  | _ -> Unknown
;;

exception ParseError

let parse_spring_array l =
  match String.split_on_char ' ' l with
  | [ a; b ] ->
    { springs = List.init (String.length a) (String.get a) |> List.map parse_state
    ; nums = String.split_on_char ',' b |> List.map int_of_string
    }
  | _ -> raise ParseError
;;

let rec parse_input () =
  match read_line () with
  | "" -> []
  | l -> parse_spring_array l :: parse_input ()
;;

exception SomeKindOfError

let count_blobs lst =
  let rec count_blobs' lst count =
    match lst with
    | [] -> if count > 0 then [ count ] else []
    | hd :: tl ->
      (match hd, count with
       | Operational, 0 -> count_blobs' tl count
       | Operational, c -> c :: count_blobs' tl 0
       | Damaged, c -> count_blobs' tl (c + 1)
       | _ -> raise SomeKindOfError)
  in
  count_blobs' lst 0
;;

let check_blobs l1 l2 =
  let blobs = count_blobs l1 in
  if List.length blobs == List.length l2
  then List.for_all (fun (a, b) -> a == b) (List.combine blobs l2)
  else false
;;

let count_with_unknown { springs; nums } =
  let rec count_with_unknown' left todo =
    match todo with
    | [] -> if check_blobs (List.rev left) nums then 1 else 0
    | hd :: tl ->
      (match hd with
       | Unknown ->
         count_with_unknown' (Operational :: left) tl
         + count_with_unknown' (Damaged :: left) tl
       | _ -> count_with_unknown' (hd :: left) tl)
  in
  count_with_unknown' [] springs
;;

let part1 () =
  let input = parse_input () in
  List.map count_with_unknown input |> List.fold_left ( + ) 0
;;

let rec print_arr l =
  match l with
  | [] -> print_endline ""
  | st :: tl ->
    print_char
      (match st with
       | Unknown -> '?'
       | Operational -> '.'
       | Damaged -> '#');
    print_arr tl
;;

let sprint_arr l =
  let cl =
    List.map
      (function
       | Unknown -> '?'
       | Operational -> '.'
       | Damaged -> '#')
      l
  in
  String.of_seq (List.to_seq cl)
;;

let get_opt lst count =
  let rec get_opt' lst c o =
    match lst with
    | [] -> o, []
    | Operational :: tl -> if c == 0 then get_opt' tl 0 [] else c :: o, tl
    | Damaged :: tl -> get_opt' tl (c + 1) o
    | Unknown :: tl -> get_opt' tl (c + 1) (c :: o)
  in
  get_opt' lst count []
;;

let check_possible lst num count =
  (* print_arr lst; *)
  Printf.printf "count: %d, num: %d, lst: " count num;
  print_arr lst;
  let opt, _ = get_opt lst count in
  Utils.print_list opt;
  match List.find_opt (fun x -> x == num) opt with
  | Some _ -> true
  | None ->
    (match List.find_opt (fun x -> x == 0) opt with
     | Some _ -> true
     | None -> false)
;;

let count_with_unknown2 { springs; nums } =
  let rec count_with_unknown' left count todo nums =
    match todo with
    | [] -> 1
    | hd :: tl ->
      print_arr (List.concat [ List.rev left; todo ]);
      print_int (List.length todo);
      let psb = check_possible todo (List.hd nums) count in
      Printf.printf "%b\n" psb;
      if not psb
      then 0
      else (
        match hd with
        | Unknown ->
          count_with_unknown'
            (Operational :: left)
            0
            tl
            (if count > 0 then List.tl nums else nums)
          + count_with_unknown' (Damaged :: left) (count + 1) tl nums
        | Operational ->
          count_with_unknown' (hd :: left) 0 tl (if count > 0 then List.tl nums else nums)
        | Damaged -> count_with_unknown' (hd :: left) (count + 1) tl nums)
  in
  count_with_unknown' [] 0 springs nums
;;

let expect lst t = if List.fold_left (fun c x -> x == t && c) true lst then 1 else 0

(* let basic_sieve todo nums = *)
(*   let o, d, u = *)
(*     List.fold_left *)
(*       (fun (x, y, z) i -> *)
(*         match i with *)
(*         | Unknown -> x, y, z + 1 *)
(*         | Damaged -> x, y + 1, z *)
(*         | Operational -> x + 1, y, z) *)
(*       (0, 0, 0) *)
(*       todo *)
(*   in *)
(*   Utils.sum nums <= u + d && List.length nums <= o + u + 1 *)
(* ;; *)

let rec strip lst =
  match lst with
  | Operational :: tl -> strip tl
  | _ -> lst
;;

let cwu2 i { springs; nums } =
  print_int i;
  print_char ':';
  let tbl = Hashtbl.create 100000 in
  let rec cwu' damaged todo current_num remaining_nums =
    let cwu_inner damaged todo current_num remaining_nums =
      match todo with
      | [] -> if damaged == current_num && List.length remaining_nums == 0 then 1 else 0
      | Operational :: tl ->
        (match damaged with
         | 0 -> cwu' 0 tl current_num remaining_nums
         | x when x == current_num ->
           (match remaining_nums with
            | h :: t -> cwu' 0 tl h t
            | [] -> cwu' 0 tl 0 [])
         | _ -> 0)
      | Damaged :: tl ->
        if damaged >= current_num
        then 0
        else cwu' (damaged + 1) tl current_num remaining_nums
      | Unknown :: tl ->
        cwu' damaged (Operational :: tl) current_num remaining_nums
        + cwu' damaged (Damaged :: tl) current_num remaining_nums
    in
    (* let todo = if damaged == 0 then strip todo else todo in *)
    match Hashtbl.find_opt tbl (damaged, todo, current_num, remaining_nums) with
    | None ->
      let inner = cwu_inner damaged todo current_num remaining_nums in
      Hashtbl.add tbl (damaged, todo, current_num, remaining_nums) inner;
      inner
    | Some i -> i
  in
  let res = cwu' 0 springs (List.hd nums) (List.tl nums) in
  print_int res;
  print_endline "";
  res
;;

let unfold_input { springs; nums } =
  { springs =
      List.concat
        [ springs
        ; Unknown :: springs
        ; Unknown :: springs
        ; Unknown :: springs
        ; Unknown :: springs
        ]
  ; nums = List.concat [ nums; nums; nums; nums; nums ]
  }
;;

let part2 () =
  let input = parse_input () in
  print_int (List.length input);
  print_endline "";
  let input = List.map unfold_input input in
  List.mapi cwu2 input |> List.fold_left ( + ) 0
;;
(* cwu2 4 (List.nth input 4) *)
