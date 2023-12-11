open Base

let parse_line line =
  line |> String.to_list
  
let parse input =
  let lines = String.split_lines input in
  lines |> List.map ~f:parse_line

let expansions puzzle =
  let empty list = list |> List.for_all ~f:(Char.equal '.') in
  let empty_rows puzzle = puzzle
                          |> List.filter_mapi ~f:(fun x row -> if empty row then Some(x) else None)
  in
  let empty_columns puzzle = puzzle
                              |> List.transpose
                              |> Option.value_exn
                              |> empty_rows
  in
  (empty_columns puzzle, empty_rows puzzle)

let galaxies factor puzzle =
  let factor = factor - 1 in
  let (expansions_x, expansions_y) = expansions puzzle in
  let before list n = list |> List.count ~f:((>=) n) in
  let map x y = (factor * (before expansions_x x) + x, factor * (before expansions_y y) + y) in
  let galaxies' y row = row |> List.filter_mapi ~f:(fun x c -> if Char.equal c '#' then Some(map x y) else None) in
  puzzle
  |> List.mapi ~f:galaxies'
  |> List.concat
  |> Array.of_list

let pairs array =
  List.range 0 (Array.length array - 1)
  |> List.map ~f:(fun i -> List.range (i + 1) (Array.length array) |> List.map ~f:(fun j -> (array.(i), array.(j))))
  |> List.concat

let manhattan start goal =
  let (start_x, start_y) = start in
  let (goal_x, goal_y) = goal in
  Int.abs (goal_x - start_x) + Int.abs (goal_y - start_y) 

let part1 puzzle =
  puzzle
  |> galaxies 2
  |> pairs
  |> List.map ~f:(fun (start, goal) -> manhattan start goal)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let part2 puzzle =
  puzzle
  |> galaxies 1000000
  |> pairs
  |> List.map ~f:(fun (start, goal) -> manhattan start goal)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
