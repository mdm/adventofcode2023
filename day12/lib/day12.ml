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

let rec repeat acc list n sep =
  if Int.equal n 0 then
    List.concat (List.tl_exn acc)
  else
    repeat (sep::list::acc) list (n - 1) sep

let count_arrangements_fast record damaged_groups =
  let memo = Hashtbl.create (module String) in
  let rec count_arrangements_fast' record damaged_groups in_group =
    let key = String.concat [
      String.of_list record;
      (String.concat ~sep:"," (List.map ~f:Int.to_string damaged_groups));
      Bool.to_string in_group;
    ] in
    match Hashtbl.find memo key with
      Some(result) -> result
    | None -> (let data = count_arrangements_fast_inner record damaged_groups in_group in
              Hashtbl.set memo ~key ~data;
              data)
  and count_arrangements_fast_inner record damaged_groups in_group =
    if List.is_empty record then
      match damaged_groups with
        0::[] -> 1
      | [] -> 1
      | _ -> 0
    else if List.is_empty damaged_groups then
      if List.mem record '#' ~equal:Char.equal then 0 else 1
    else match (record, damaged_groups) with
      ('.'::record_tl, dg_hd::dg_tl) -> if in_group then
                                          if dg_hd = 0 then
                                            count_arrangements_fast' record_tl dg_tl false
                                          else
                                            0
                                        else
                                          count_arrangements_fast' record_tl damaged_groups false
    | ('#'::record_tl, dg_hd::dg_tl) -> if dg_hd = 0 then
                                          0
                                        else
                                          count_arrangements_fast' record_tl ((dg_hd - 1)::dg_tl) true
    | ('?'::record_tl, _) -> (count_arrangements_fast' ('.'::record_tl) damaged_groups in_group) + (count_arrangements_fast' ('#'::record_tl) damaged_groups in_group)
    | _ -> assert false      
  in
  count_arrangements_fast' record damaged_groups false

let part2 puzzle =
  let fast (record, damaged_groups) = count_arrangements_fast record damaged_groups in
  let extended factor (record, damaged_groups) = (repeat [] record factor ['?'], repeat [] damaged_groups factor []) in
  puzzle
  |> List.map ~f:(extended 5)
  |> List.map ~f:fast
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
