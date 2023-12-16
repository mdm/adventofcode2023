open Base

let parse_line line =
  line |> String.split ~on:','
  
let parse input =
  let lines = String.split_lines input in
  lines
  |> List.hd_exn
  |> parse_line

let hash input =
  let step acc c = (acc + Char.to_int c) * 17 % 256 in
  let rec hash' acc input =
    match input with
      [] -> acc
    | hd::tl -> hash' (step acc hd) tl
  in
  hash' 0 input

let part1 puzzle =
  puzzle
  |> List.map ~f:(fun str -> str |> String.to_list |> hash)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let parse_step step = 
  let (label, operation) = step |> String.to_list |> List.split_while ~f:Char.is_alpha in
  (String.of_list label, List.hd_exn operation, List.tl_exn operation |> String.of_list |> (fun str -> try Some(Int.of_string str) with _ -> None))

let execute_remove boxes box_nr label =
  let box = boxes.(box_nr) in
  boxes.(box_nr) <- (box |> List.filter ~f:(fun (l, _) -> not (String.equal l label)))

let execute_put boxes box_nr label focal_len =
  let box = boxes.(box_nr) in
  match List.find box ~f:(fun (l, _) -> String.equal l label) with
    Some(_) -> boxes.(box_nr) <- (box |> List.map ~f:(fun (l, f) -> if String.equal l label then (l, focal_len) else (l, f)))
  | None -> boxes.(box_nr) <- ((label, focal_len)::box)

let execute_step boxes step =
  let (label, operation, operand) = step in
  let box_nr = label |> String.to_list |> hash in
  match operation with
    '-' -> execute_remove boxes box_nr label
  | '=' -> execute_put boxes box_nr label (Option.value_exn operand)
  | _ -> assert false

let focusing_power box_nr box =
  let focusing_power' slot focal_len = (box_nr + 1) * (slot + 1) * focal_len in
  box
  |> List.rev
  |> List.foldi ~init:0 ~f:(fun i acc (_, f) -> acc + focusing_power' i f)

let part2 puzzle =
  let boxes = Array.create ~len:256 [] in
  puzzle
  |> List.map ~f:parse_step
  |> List.iter ~f:(execute_step boxes);
  boxes
  |> Array.mapi ~f:focusing_power
  |> Array.fold ~init:0 ~f:(+)
  |> Int.to_string
