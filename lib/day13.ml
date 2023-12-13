let rec parse_fields () =
  match Utils.parse_matrix (fun x -> x) with
  | [] -> []
  | l -> l :: parse_fields ()
;;

let rec check_mirror l1 l2 =
  match l1, l2 with
  | [], _ -> true
  | _, [] -> true
  | hd1 :: tl1, hd2 :: tl2 ->
    if Utils.list_equal hd1 hd2 then check_mirror tl1 tl2 else false
;;

let count_nonequal x y = List.fold_left (fun c (a, b) -> if a == b then c else c + 1) 0
(List.combine x y)

let rec check_mirror_count l1 l2 =
  match l1, l2 with
  | [], _ -> 0
  | _, [] -> 0
  | hd1 :: tl1, hd2 :: tl2 ->
    (count_nonequal hd1 hd2) + (check_mirror_count tl1 tl2)
;;

let scan_mirror l =
  let rec scan_mirror' left l c =
    if check_mirror left l
    then Some c
    else (
      match l with
      | _ :: [] -> None
      | hd :: tl -> scan_mirror' (hd :: left) tl (c + 1)
      | _ -> None)
  in
  scan_mirror' [ List.hd l ] (List.tl l) 1
;;


let scan_mirror_count l =
  let rec scan_mirror' left l c =
    let off = check_mirror_count left l in
    (* Printf.printf "scan %d: %d\n" c off; *)
    if off == 1 then
        Some c
    else (
      match l with
      | _ :: [] -> None
      | hd :: tl -> scan_mirror' (hd :: left) tl (c + 1)
      | _ -> None)
  in
  scan_mirror' [ List.hd l ] (List.tl l) 1
;;

let get_score l =
  match scan_mirror l with
  | Some x -> Some (100 * x)
  | None -> scan_mirror (Utils.transpose l)
;;

let part1 () =
  let fields = parse_fields () in
  let res = List.filter_map get_score fields in
  Utils.sum res;
;;

let get_score_count l =
  match scan_mirror_count l with
  | Some x -> Some (100 * x)
  | None -> scan_mirror_count (Utils.transpose l)
;;

let part2 () =
  let fields = parse_fields () in
  let res = List.filter_map get_score_count fields in
  Utils.sum res;

