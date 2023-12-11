open Aoc23

exception DayNotRegisterd
exception WrongArgs

let run_part1 str =
  match str with
  | "2" -> Day2.part1 ()
  | "3" -> Day3.part1 ()
  | "4" -> Day4.part1 ()
  | "5" -> Day5.part1 ()
  | "6" -> Day6.part1 ()
  | "7" -> Day7.part1 ()
  | "8" -> Day8.part1 ()
  | "9" -> Day9.part1 ()
  | "10" -> Day10.part1 ()
  | "11" -> Day11.part1 ()
  | "12" -> Day12.part1 ()
  | "13" -> Day13.part1 ()
  | "14" -> Day14.part1 ()
  | "15" -> Day15.part1 ()
  | "16" -> Day16.part1 ()
  | "17" -> Day17.part1 ()
  | "18" -> Day18.part1 ()
  | "19" -> Day19.part1 ()
  | "20" -> Day20.part1 ()
  | "21" -> Day21.part1 ()
  | "22" -> Day22.part1 ()
  | "23" -> Day23.part1 ()
  | "24" -> Day24.part1 ()
  | "25" -> Day25.part1 ()
  | _ -> raise DayNotRegisterd

let run_part2 str =
  match str with
  | "2" -> Day2.part2 ()
  | "3" -> Day3.part2 ()
  | "4" -> Day4.part2 ()
  | "5" -> Day5.part2 ()
  | "6" -> Day6.part2 ()
  | "7" -> Day7.part2 ()
  | "8" -> Day8.part2 ()
  | "9" -> Day9.part2 ()
  | "10" -> Day10.part2 ()
  | "11" -> Day11.part2 ()
  | "12" -> Day12.part2 ()
  | "13" -> Day13.part2 ()
  | "14" -> Day14.part2 ()
  | "15" -> Day15.part2 ()
  | "16" -> Day16.part2 ()
  | "17" -> Day17.part2 ()
  | "18" -> Day18.part2 ()
  | "19" -> Day19.part2 ()
  | "20" -> Day20.part2 ()
  | "21" -> Day21.part2 ()
  | "22" -> Day22.part2 ()
  | "23" -> Day23.part2 ()
  | "24" -> Day24.part2 ()
  | "25" -> Day25.part2 ()
  | _ -> raise DayNotRegisterd

type parts_t = Both | Part1 | Part2

let () =
  let day, parts =
    match Array.to_list Sys.argv with
    | [ _; day; part ] ->
        let parts =
          match part with "1" -> Part1 | "2" -> Part2 | _ -> raise WrongArgs
        in
        (day, parts)
    | [ _; day ] -> (day, Both)
    | _ -> raise WrongArgs
  in
  print_endline ("==== Day " ^ day ^ " =============");

  let () =
    match parts with
    | Both | Part1 ->
        print_endline "Part 1";
        let res = run_part1 day in
        print_int res;
        print_endline ""
    | _ -> ()
  in

  let () =
    match parts with
    | Both | Part2 ->
        print_endline "Part 2";
        let res = run_part2 day in
        print_int res;
        print_endline ""
    | _ -> ()
  in

  ()
