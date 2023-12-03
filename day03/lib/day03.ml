open Base

type position =
  { x: int;
    y: int;
  }

type number =
  { value: int;
    pos: position;
    len: int;
  }

let parse input =
  let lines = String.split_lines input in
  List.map ~f:String.to_array lines
  |> List.to_array

let extract_digits y x acc c =
  match Char.get_digit c with
    Some(c) -> { value =  c;
                 pos = { x; y; };
                 len = 1;
               }::acc
  | None -> acc

let combine_digits digits =
  let combine acc digit = match acc with
    [] -> [digit]
  | hd::tl -> if Int.equal (hd.pos.x - digit.pos.x) 1 then
      let x = digit.pos.x in
      let y = digit.pos.y in
      let len = hd.len + 1 in
      let value = hd.value + digit.value * (10 ** (len - 1)) in
      { value; pos = { x; y; }; len; }::tl
    else
      digit::acc
  in
  List.fold digits ~init: [] ~f:combine

let extract_numbers_from_line y line =
  List.foldi line ~init:[] ~f:(extract_digits y)
  |> combine_digits

let extract_numbers_from_schematic schematic =
  List.of_array schematic
  |> List.map ~f:List.of_array
  |> List.mapi ~f:extract_numbers_from_line
  |> List.concat

let neighbors n =
  let left = [
      { x = n.pos.x - 1; y = n.pos.y - 1; };
      { x = n.pos.x - 1; y = n.pos.y; };
      { x = n.pos.x - 1; y = n.pos.y + 1; };
    ] in
  let right = [
      { x = n.pos.x + n.len; y = n.pos.y - 1; };
      { x = n.pos.x + n.len; y = n.pos.y; };
      { x = n.pos.x + n.len; y = n.pos.y + 1; };
    ] in
  let top = List.range n.pos.x (n.pos.x + n.len) |> List.map ~f:(fun x -> { x; y = n.pos.y - 1; }) in
  let bottom = List.range n.pos.x (n.pos.x + n.len) |> List.map ~f:(fun x -> { x; y = n.pos.y + 1; }) in
  List.concat [top; right; bottom; left;]

let is_valid_neigbor puzzle neighbor =
  neighbor.x >= 0
  && neighbor.x < Array.length puzzle.(0)
  && neighbor.y >= 0
  && neighbor.y < Array.length puzzle

let is_symbol c =
  Char.(<>) c '.' && not (Char.is_digit c)

let is_part_number puzzle n =
  let symbol_neighbor neighbor = is_valid_neigbor puzzle neighbor && is_symbol puzzle.(neighbor.y).(neighbor.x) in
  List.exists (neighbors n) ~f:symbol_neighbor

let part1 puzzle =
  extract_numbers_from_schematic puzzle
  |> List.filter ~f:(is_part_number puzzle)
  |> List.map ~f:(fun n -> n.value)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let gear_adjacent_part_numbers part_numbers_with_candidates =
  let all_candidates = List.map part_numbers_with_candidates ~f:snd |> List.concat in
  let with_candidate candidate (_, candidates) = List.mem candidates candidate ~equal:(fun a b -> Int.equal a.x b.x && Int.equal a.y b.y) in
  let part_numbers_with_filtered_candidates candidate = List.filter part_numbers_with_candidates ~f:(with_candidate candidate) in
  let exactly_two_part_numbers candidate = match part_numbers_with_filtered_candidates candidate with
    a::b::[] -> Some([fst a; fst b])
  | _ -> None
  in
  List.filter_map all_candidates ~f:exactly_two_part_numbers

let gear_candidates puzzle part_numbers =
  let is_gear_candidate neighbor = is_valid_neigbor puzzle neighbor && Char.equal puzzle.(neighbor.y).(neighbor.x) '*' in 
  List.map part_numbers ~f:(fun part_number -> (part_number, neighbors part_number))
  |> List.map ~f:(fun (part_number, candidates) -> (part_number, List.filter candidates ~f:is_gear_candidate))

let gear_power part_numbers =
  List.map part_numbers ~f:(fun part_number -> part_number.value)
  |> List.fold ~init:1 ~f:( * )

let part2 puzzle =
  extract_numbers_from_schematic puzzle
  |> List.filter ~f:(is_part_number puzzle)
  |> gear_candidates puzzle
  |> gear_adjacent_part_numbers
  |> List.map ~f:gear_power
  |> List.fold ~init:0 ~f:(+)
  |> Fn.flip (/) 2
  |> Int.to_string

  