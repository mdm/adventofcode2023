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
  |> List.count ~f:(push_wins race)
  
let part1 puzzle =
  puzzle
  |> List.map ~f:ways_to_win
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

let fast_ways_to_win race =
  let median = race.time / 2 in
  let median_count = if Int.equal (race.time % 2) 0 then 1 else 2 in
  let offset = Float.of_int (-race.time) in
  let sqrt = Float.sqrt (Float.of_int (race.time * race.time - 4 * -1 * -race.distance)) in
  let min_push = Float.min ((offset -. sqrt) /. -2.0) ((offset +. sqrt) /. -2.0) in
  let min_push = Option.value_exn (Float.iround_down (min_push +. 1.0)) in
  if not (push_wins race median) then 0 else median_count + 2 * (median - min_push)

let part2 puzzle =
  puzzle
  |> big_race
  |> fast_ways_to_win
  |> Int.to_string
