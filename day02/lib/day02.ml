open Base

type draw =
  { red: int;
    green: int;
    blue: int;
  }

type game =
  { id : int;
    draws: draw list;
  }

let parse_cubes draw cubes =
  let cubes = String.strip cubes in
  let (amount, color) = match String.split cubes ~on:' ' with
      amount::color::_ -> (Int.of_string amount, color)
    | _ -> assert false
  in
  match color with
    "red" -> { draw with red = draw.red + amount }
  | "green" -> { draw with green = draw.green + amount }
  | "blue" -> { draw with blue = draw.blue + amount }
  | _ -> assert false


let parse_draw draw =
  let cubes = String.split draw ~on:',' in
  List.fold cubes ~init:{ red = 0; green = 0; blue = 0; } ~f:parse_cubes

let parse_game line =
  let (game_info, draws_info) = match String.split line ~on:':' with
      game_info::draws_info::_ -> (game_info, draws_info)
    | _ -> assert false
  in
  let id = String.split game_info ~on:' '
    |> Fn.flip List.nth_exn 1
    |> Int.of_string
  in
  let draws =
    String.split draws_info ~on:';'
    |> List.map ~f:parse_draw
  in
  { id;
    draws;
  }

let parse input =
  let lines = String.split_lines input in
  List.map ~f:parse_game lines

let is_game_possible { draws; _ } =
  List.for_all draws ~f:(fun draw -> draw.red <= 12 && draw.green <= 13 && draw.blue <= 14)

let extract_game_id { id; _ } = id

let part1 puzzle =
  List.filter ~f:is_game_possible puzzle
  |> List.map ~f:extract_game_id
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let min_cubes draw_a draw_b =
  { red = Int.max draw_a.red draw_b.red;
    green = Int.max draw_a.green draw_b.green;
    blue = Int.max draw_a.blue draw_b.blue;
  }

let power { draws; _ } =
  let cubes = List.reduce_exn draws ~f:min_cubes in
  cubes.red * cubes.green * cubes.blue

let part2 puzzle =
  List.map ~f:power puzzle
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
