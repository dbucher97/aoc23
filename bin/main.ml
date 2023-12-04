open Aoc23

exception DayNotRegisterd
exception WrongArgs

let run_part1 str =
    match str with
    | "2" -> Day2.part1 ()
    | "3" -> Day3.part1 ()
    | _ -> raise DayNotRegisterd

let run_part2 str =
    match str with
    | "2" -> Day2.part2 ()
    | _ -> raise DayNotRegisterd

type parts_t = Both | Part1 | Part2;;

let () =
    let day, parts = match Array.to_list Sys.argv with
    | _ :: day :: part :: [] -> 
            let parts = match part with
            | "1" -> Part1
            | "2" -> Part2
            | _ -> raise WrongArgs
            in
            day, parts
    | _ :: day :: [] -> day, Both
    | _ -> raise WrongArgs
    in
    print_endline ("==== Day " ^ day ^ " =============");

    let () = match parts with
    | Both | Part1 -> 
        print_endline "Part 1";
        let res = run_part1 day in
        print_int res;
        print_endline "";
    | _ -> ()
    in

    let () = match parts with
    | Both | Part2 ->
        print_endline "Part 2";
        let res = run_part2 day in
        print_int res;
        print_endline "";
    | _ -> ()
    in

    ()
