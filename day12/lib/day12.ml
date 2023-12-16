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
  let lut = Array.make_matrix ~dimx:(max_slots + 1) ~dimy:(max_items + 1) 0 in
  let make_entry x y = if x < 1 then 1 else lut.(y).(x - 1) + lut.(y - 1).(x) in
  let make_row y = List.range 0 (max_items + 1) |> List.iter ~f:(fun x -> lut.(y).(x) <- if y < 1 then 0 else make_entry x y) in
  List.range 0 (max_slots + 1)
  |> List.iter ~f:make_row;
  lut

let rec splits acc record_groups checksum_groups =
  let checksum_groups_len = List.length checksum_groups in
  if List.length record_groups = 1 then
    [List.rev ((checksum_groups_len)::acc)]
  else
    if List.is_empty checksum_groups then
      splits (0::acc) (List.drop record_groups 1) []
    else
      List.range 1 (checksum_groups_len + 1)
      |> List.filter ~f:(fun n -> List.take checksum_groups n |> List.fold ~init:(n - 1) ~f:(+) |> (fun space_needed -> space_needed <= (List.hd_exn record_groups |> List.length)))
      |> (fun ns -> 0::ns)
      |> List.map ~f:(fun n -> splits (n::acc) (List.drop record_groups 1) (List.drop checksum_groups n))
      |> List.concat
  
let rec count_record_group_arrangements lut record_group checksum_groups =
  let record_group_len = List.length record_group in
  let checksum_groups_len = List.length checksum_groups in
  let checksum_groups_space = checksum_groups |> List.fold ~init:(checksum_groups_len - 1) ~f:(+) in
  let dot_slots = match record_group_len - checksum_groups_space with
                    0 -> 1 (* tight fit *)
                  | _ -> if record_group_len < checksum_groups_space then 0 (* no space *) else checksum_groups_len + 1 (* some room *)
  in
  let dots = Int.max 0 (record_group_len - checksum_groups_space) in
  match List.findi record_group ~f:(fun _ c -> Char.equal c '#') with
    Some(i, _) -> match_checksum_group lut record_group i checksum_groups
  | None -> lut.(dot_slots).(dots)
and match_checksum_group lut record_group first_spring checksum_groups =
  if List.is_empty record_group || List.is_empty checksum_groups then
    0
  else
    let record_group_len = List.length record_group in
    let num_springs = List.hd_exn checksum_groups in
    List.range 0 (first_spring + 1)
    |> List.map ~f:(fun start -> if start + num_springs > record_group_len || (start + num_springs < record_group_len && List.nth_exn record_group (start + num_springs) |> Char.equal '#') then
                                   0
                                 else
                                   count_record_group_arrangements lut (List.drop record_group (start + num_springs + 1)) (List.tl_exn checksum_groups)
                   )
    |> List.fold ~init:0 ~f:(+)
  
let count_arrangements_fast lut record_groups checksum_groups =
  let rec count_with_split record_groups checksum_groups split =
    match (record_groups, split) with
    | (rg_hd::rg_tl, s_hd::s_tl) -> (let result = count_record_group_arrangements lut rg_hd (List.take checksum_groups s_hd) in
                                    (* Stdio.print_string "CWS "; Stdio.print_endline (Int.to_string result); *)
                                    result * count_with_split rg_tl (List.drop checksum_groups s_hd) s_tl                                    
                                    )
    | _ -> 1
  in
  Stdio.print_string "LRG "; Stdio.print_endline (Int.to_string (List.length record_groups));
  Stdio.print_string "LCG "; Stdio.print_endline (Int.to_string (List.length checksum_groups));
  splits [] record_groups checksum_groups
  |> List.map ~f:(count_with_split record_groups checksum_groups)
  |> List.fold ~init:0 ~f:(+)

let part2 puzzle =
  let max f = puzzle
                  |> List.map ~f:(fun input -> 1 + List.length (f input))
                  |> List.reduce ~f:Int.max
                  |> Option.value_exn
  in
  let max_slots = max snd in
  Stdio.print_string "MAXS "; Stdio.print_endline (Int.to_string max_slots);
  let max_items = max fst in
  Stdio.print_string "MAXI "; Stdio.print_endline (Int.to_string max_items);
  let lut = make_lut max_slots max_items in
  let fast (record, checksum_groups) = Stdio.print_endline (String.of_list record);count_arrangements_fast lut (split_record' record) checksum_groups in
  let extended factor (record, checksum_groups) = (repeat [] record factor ['?'], repeat [] checksum_groups factor []) in
  puzzle
  |> List.map ~f:(extended 5)
  |> List.mapi  ~f:(fun i case -> Stdio.print_string "Case # "; Stdio.print_endline (Int.to_string i);fast case)
  |> List.map ~f:(fun n -> Stdio.print_string "== ";Stdio.print_endline (Int.to_string n); n)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
