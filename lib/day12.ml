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

type opt =
  { min : int
  ; optional : int
  }

let get_opt lst count =
  let z = { min = count; optional = 0 } in
  let rec get_opt' lst o =
    match lst with
    | [] -> o, []
    | Operational :: tl -> if o.min == 0 && o.optional == 0 then get_opt' tl z else o, tl
    | Damaged :: tl -> get_opt' tl { min = o.min + 1; optional = o.optional }
    | Unknown :: tl -> get_opt' tl { min = o.min; optional = o.optional + 1 }
  in
  get_opt' lst z
;;

let check_possible lst num count =
  (* print_arr lst; *)
  Printf.printf "count: %d, num: %d, lst: " count num;
  print_arr lst;
  let opt, l = get_opt lst count in
  Printf.printf "min: %d, range: %d, rest: " opt.min opt.optional;
  print_arr l;
  opt.min <= num && opt.min + opt.optional >= num
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
  (* let input = List.map unfold_input input in *)
  count_with_unknown2 (List.nth input 2)
;;
(* |> List.fold_left ( + ) 0 *)
