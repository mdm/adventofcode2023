open Base

type race =
  { time: int;
    distance: int;
  }

let parse_numbers str =
  str
  |> String.split ~on:' '
  |> List.filter ~f:(fun n -> not (String.is_empty n))
  |> List.map ~f:Int.of_string

let parse input =
  let lines = String.split_lines input in
  let times = List.nth_exn lines 0
              |> String.split ~on:':'
              |> List.last_exn
              |> parse_numbers
  in
  let distances = List.nth_exn lines 1
                  |> String.split ~on:':'
                  |> List.last_exn
                  |> parse_numbers
  in
  List.zip_exn times distances
  |> List.map ~f:(fun (time, distance) -> { time; distance; })

let push_wins race push =
  (race.time - push) * push > race.distance

let ways_to_win race =
  List.range 0 race.time
  |> List.filter ~f:(push_wins race)
  
let part1 puzzle =
  puzzle
  |> List.map ~f:ways_to_win
  |> List.map ~f:List.length
  |> List.reduce ~f:( * )
  |> Option.value_exn
  |> Int.to_string

let concat_numbers numbers =
  numbers
  |> List.map ~f:Int.to_string
  |> String.concat
  |> Int.of_string

let big_race races =
  let times = List.map races ~f:(fun race -> race.time) in
  let distances = List.map races ~f:(fun race -> race.distance) in
  { time = concat_numbers times; distance = concat_numbers distances; }

let part2 puzzle =
  puzzle
  |> big_race
  |> ways_to_win
  |> List.length
  |> Int.to_string
