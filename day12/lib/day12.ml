open Base

let parse_line line =
  let parts = line |> String.split ~on:' ' in
  let record = List.nth_exn parts 0 |> String.to_list in
  let damaged_groups = List.nth_exn parts 1
                       |> String.split ~on:','
                       |> List.map ~f:Int.of_string
  in
  (record, damaged_groups)
  
  
let parse input =
  let lines = String.split_lines input in
  lines |> List.map ~f:parse_line

let split_record record =
  record
  |> List.group ~break:(fun a b -> not (Char.equal a b))

let to_group_lengths arrangement =
  arrangement
  |> split_record
  |> List.filter ~f:(fun g -> Char.equal (List.hd_exn g) '#')
  |> List.map ~f:List.length

let valid_arrangement arrangement damaged_groups =
  List.equal Int.equal (to_group_lengths arrangement) damaged_groups

let rec count_arrangements acc record damaged_groups =
  match record with
    [] -> if valid_arrangement (List.rev acc) damaged_groups then 1 else 0
  | sprint::record -> match sprint with
                  '?' -> count_arrangements ('.'::acc) record damaged_groups + count_arrangements ('#'::acc) record damaged_groups
                | _ -> count_arrangements (sprint::acc) record damaged_groups

let part1 puzzle =
  puzzle
  |> List.map ~f:(fun (record, damaged_groups) -> count_arrangements [] record damaged_groups)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let compress list =
  list
  |> List.sort_and_group ~compare:Int.compare
  |> List.map ~f:(fun group -> (List.hd_exn group, List.length group))

let uncompress list =
  let rec repeat acc elt n = if Int.equal n 0 then acc else repeat (elt::acc) elt (n - 1) in
  list
  |> List.map ~f:(fun (elt, count) -> repeat [] elt count)
  |> List.concat
  

let damaged_springs record_group =
  record_group
  |> List.filter_mapi ~f:(fun i c -> if Char.equal c '#' then Some(i) else None)
  |> List.group ~break:(fun a b -> not (Int.equal 1 (b - a)))
  |> List.map ~f:(fun g -> (List.hd_exn g, List.length g))

let split_record' record =
  let break a b = match (a, b) with
                    ('?', '#') -> false
                  | ('#', '?') -> false
                  | _ -> not (Char.equal a b)
  in
  record
  |> List.group ~break:break
  |> List.filter ~f:(fun g -> g |> List.hd_exn |> Char.equal '.' |> not)

let rec repeat acc list n sep =
  if Int.equal n 0 then
    List.concat (List.tl_exn acc)
  else
    repeat (sep::list::acc) list (n - 1) sep


let make_lut max_slots max_items =
  let lut = Array.create ~len:(max_slots + 1) (Array.create ~len:(max_items + 1) 0) in
  let make_entry x y = if x < 1 then 1 else lut.(y).(x - 1) + lut.(y - 1).(x) in
  let make_row y row = row |> Array.iteri ~f:(fun x _ -> lut.(y).(x) <- if y < 1 then 0 else make_entry x y) in
  lut
  |> Array.iteri ~f:make_row;
  lut

  let rec splits acc num_record_groups num_checksum_groups =
    if num_checksum_groups < num_record_groups then
      []
    else
      if Int.equal num_record_groups 1 then
        [List.rev ((num_checksum_groups)::acc)]
      else
        List.range 1 num_checksum_groups
        |> List.map ~f:(fun n -> splits (n::acc) (num_record_groups - 1) (num_checksum_groups - n))
        |> List.concat
  
  let count_record_group_arrangements lut record_group checksum_groups =
  let record_group_len = List.length record_group in
  let checksum_groups_len = List.length checksum_groups in
  let checksum_groups_space = checksum_groups |> List.fold ~init:(checksum_groups_len - 1) ~f:(+) in
  let dot_slots = match checksum_groups_space - record_group_len with
                    0 -> Int.max 0 (checksum_groups_len - 1)
                  | 1 -> checksum_groups_len
                  | _ -> if record_group_len < checksum_groups_space then 0 else checksum_groups_len + 1
  in
  let dots = Int.max 0 (record_group_len - dot_slots) in
  match List.findi record_group ~f:(fun _ c -> Char.equal c '#') with
    Some(_i, _) -> 0
  | None -> lut.(dot_slots).(dots)
and match_checksum_group _lut record_group first_spring checksum_groups =
  let _record_group_len = List.length record_group in
  let checksum_groups_len = List.length checksum_groups in
  let match_checksum_group' split =
    let checksum_groups_before = List.take checksum_groups (List.hd_exn split) in
    let num_springs = List.drop checksum_groups (List.hd_exn split) |> List.hd_exn in
    let checksum_groups_after = List.drop checksum_groups (1 + List.hd_exn split) in
    List.range (first_spring - num_springs + 1) (first_spring + 1)
    |> List.filter ~f:Int.is_non_negative
    |> List.map ~f:(fun offset -> let record_group_before = if offset > 0 then List.take record_group (offset - 1) in)
    |> List.fold ~init:0 ~f:(+)
  in
  if List.is_empty record_group || List.is_empty checksum_groups then
    0
  else
    splits [] 2 (checksum_groups_len + 1)
    |> List.map ~f:(List.map ~f:((-) 1))
    |> List.map ~f:match_checksum_group'
    |> List.fold ~init:0 ~f:(+)

let count_arrangements_fast lut (record_groups: char list list) checksum_groups =
  let count_with_split split =
    List.zip_exn record_groups split
    |> List.map ~f:(fun (record_group, n) -> count_record_group_arrangements lut record_group (List.take checksum_groups n))
    |> List.fold ~init:0 ~f:( * )
  in
  splits [] (List.length record_groups) (List.length checksum_groups)
  |> List.map ~f:count_with_split
  |> List.fold ~init:0 ~f:(+)

let part2 puzzle =
  let max f = puzzle
                  |> List.map ~f:(fun input -> 1 + List.length (f input))
                  |> List.reduce ~f:Int.max
                  |> Option.value_exn
  in
  let max_slots = max snd in
  let max_items = max fst in
  let lut = make_lut max_slots max_items in
  let fast (record, checksum_groups) = count_arrangements_fast lut (split_record' record) checksum_groups in
  let extended factor (record, checksum_groups) = (repeat [] record factor ['?'], repeat [] checksum_groups factor []) in
  puzzle
  |> List.map ~f:(extended 1)
  |> List.mapi  ~f:(fun i case -> Stdio.print_string "Case # "; Stdio.print_endline (Int.to_string i);fast case)
  |> List.map ~f:(fun n -> Stdio.print_string "== ";Stdio.print_endline (Int.to_string n); n)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
