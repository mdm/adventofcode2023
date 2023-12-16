open Base

let parse_line line =
  line |> String.to_array
  
let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:parse_line
  |> Array.of_list

let energize prev_x prev_y x y puzzle =
  let history = Hashtbl.create (module Int) in
  let key prev_x prev_y x y = 1000 * 1000 * 1000 * prev_x + 1000 * 1000 * prev_y + 1000 * x + y in
  let rec energize' acc history prev_x prev_y x y puzzle =
    let dir_x = x - prev_x in
    let dir_y = y - prev_y in
    if x < 0 || x >= Array.length puzzle.(0) || y < 0 || y >= Array.length puzzle then
      acc
    else
      match Hashtbl.add history ~key:(key prev_x prev_y x y) ~data:true with
        `Duplicate -> acc
        | `Ok -> match puzzle.(y).(x) with
                   '.' -> energize' ((x, y)::acc) history x y (x + dir_x) (y + dir_y) puzzle
                 | '/' -> (match (dir_x, dir_y) with
                           (0, -1) -> energize' ((x, y)::acc) history x y (x + 1) y puzzle
                         | (1, 0) -> energize' ((x, y)::acc) history x y x (y - 1) puzzle
                         | (0, 1) -> energize' ((x, y)::acc) history x y (x - 1) y puzzle
                         | (-1, 0) -> energize' ((x, y)::acc) history x y x (y + 1) puzzle
                         | _ -> assert false)
                 | '\\' -> (match (dir_x, dir_y) with
                           (0, -1) -> energize' ((x, y)::acc) history x y (x - 1) y puzzle
                         | (1, 0) -> energize' ((x, y)::acc) history x y x (y + 1) puzzle
                         | (0, 1) -> energize' ((x, y)::acc) history x y (x + 1) y puzzle
                         | (-1, 0) -> energize' ((x, y)::acc) history x y x (y - 1) puzzle
                         | _ -> assert false)
                 | '|' -> (match (dir_x, dir_y) with
                           (0, -1) | (0, 1) -> energize' ((x, y)::acc) history x y (x + dir_x) (y + dir_y) puzzle
                         | (1, 0) | (-1, 0) -> List.concat [
                                                             energize' ((x, y)::acc) history x y x (y - 1) puzzle;
                                                             energize' ((x, y)::acc) history x y x (y + 1) puzzle;
                                                           ]
                         | _ -> assert false)
                 | '-' -> (match (dir_x, dir_y) with
                           (1, 0) | (-1, 0) -> energize' ((x, y)::acc) history x y (x + dir_x) (y + dir_y) puzzle
                         | (0, -1) | (0, 1) -> List.concat [
                                                             energize' ((x, y)::acc) history x y (x - 1) y puzzle;
                                                             energize' ((x, y)::acc) history x y (x + 1) y puzzle;
                                                           ]
                         | _ -> assert false)
                 | _ -> assert false
  in
  energize' [] history prev_x prev_y x y puzzle

let count_energized prev_x prev_y x y puzzle = 
  puzzle
  |> energize prev_x prev_y x y
  |> List.dedup_and_sort ~compare:(fun (a1, a2) (b1, b2) -> if Int.equal (Int.compare a1 b1) 0 then Int.compare a2 b2 else Int.compare a1 b1)
  |> List.length

let part1 puzzle =
  puzzle
  |> count_energized (-1) 0 0 0
  |> Int.to_string

let find_max puzzle =
  [
    List.range 0 (Array.length puzzle.(0))
    |> List.map ~f:(fun x -> count_energized x (-1) x 0 puzzle);
    List.range 0 (Array.length puzzle.(0))
    |> List.map ~f:(fun x -> count_energized x (Array.length puzzle.(0)) x (Array.length puzzle.(0) - 1) puzzle);
    List.range 0 (Array.length puzzle)
    |> List.map ~f:(fun y -> count_energized (-1) y 0 y puzzle);
    List.range 0 (Array.length puzzle)
    |> List.map ~f:(fun y -> count_energized (Array.length puzzle) y (Array.length puzzle - 1) y puzzle);
  ]
  |> List.concat
  |> List.reduce ~f:Int.max
  |> Option.value_exn

let part2 puzzle =
  puzzle
  |> find_max
  |> Int.to_string
