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

let valid_arrangement' arrangement group =
  arrangement
  |> to_group_lengths
  |> List.count ~f:(Int.equal group)
  |> Int.equal 1

(* let count_arrangements_single acc used record_group damaged_group =

let rec count_arrangements'' acc used record damaged_groups =
  match (record, damaged_groups) with
    ([], []) -> 1
    | ([], _) -> 0
    | (_, []) -> if List.mem record '#' ~equal:Char.equal then 0 else 1
    | (spring::tl_record, group::tl_groups) -> if used > group then
                                                 0
                                               else
                                                 if Int.equal used group then
                                                   if valid_arrangement' (List.rev acc) group then
                                                     count_arrangements'' [] 0 record tl_groups
                                                   else
                                                     0
                                                 else
                                                   let damaged_run = tl_record |> List.take_while ~f:(Char.equal '#') in
                                                   let rest = tl_record |> List.drop_while ~f:(Char.equal '#') in
                                                   match spring with
                                                     '?' -> count_arrangements'' ('.'::acc) used tl_record damaged_groups + count_arrangements'' (List.concat [damaged_run; ('#'::acc)]) (1 + List.length damaged_run) rest damaged_groups
                                                   | '#' -> count_arrangements'' (List.concat [damaged_run; ('#'::acc)]) (1 + List.length damaged_run) rest damaged_groups
                                                   | '.' -> count_arrangements'' ('.'::acc) used tl_record damaged_groups
                                                   | _ -> assert false *)
      
let part1 puzzle =
  puzzle
  |> List.map ~f:(fun (record, damaged_groups) -> count_arrangements [] record damaged_groups)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let valid_prefixes record_group checksum_groups =
  let rec valid_prefixes' acc record_group checksum_groups =
    match record_group with
      [] -> let prefix = to_group_lengths (List.rev acc) in
            if List.is_prefix checksum_groups ~prefix ~equal:Int.equal then
              [List.length prefix]
            else
              []

    | hd::tl -> match hd with
                  '?' -> List.concat [
                          valid_prefixes' ('#'::acc) tl checksum_groups;
                          valid_prefixes' ('.'::acc) tl checksum_groups;
                        ]
                  | _ -> valid_prefixes' (hd::acc) tl checksum_groups
  in
  valid_prefixes' [] record_group checksum_groups
  |> List.sort_and_group ~compare:Int.compare
  |> List.map ~f:(fun group -> (List.hd_exn group, List.length group))

let compress list =
  list
  |> List.sort_and_group ~compare:Int.compare
  |> List.map ~f:(fun group -> (List.hd_exn group, List.length group))

let uncompress list =
  let rec repeat acc elt n = if Int.equal n 0 then acc else repeat (elt::acc) elt (n - 1) in
  list
  |> List.map ~f:(fun (elt, count) -> repeat [] elt count)
  |> List.concat
  
let rec non_overlapping_prefixes record_group_len checksums_hd checksums_tl =
  let num_fits = record_group_len + 1 - (checksums_hd + 1) + 1 in
  List.range 0 num_fits
  (* |> List.map ~f:(fun offset -> Stdio.print_string "@3 "; Stdio.print_endline (Int.to_string offset); offset) *)
  |> List.map ~f:(fun offset -> let record_group_len' = record_group_len - offset - (checksums_hd + 1) in
                                valid_prefixes' record_group_len' [] checksums_tl)
  |> List.map ~f:uncompress
  |> List.map ~f:(List.map ~f:((+) 1))
  |> List.concat
  |> (fun prefix_lens -> 0::prefix_lens)
  |> compress

and overlapping_prefixes record_group_len springs_hd springs_tl checksums_hd checksums_tl =
  let (springs_start, spring_count) = springs_hd in
  let num_fits = checksums_hd - spring_count + 1 in
  let min_start = springs_start - checksums_hd + spring_count in
  let max_start_excl = min_start + num_fits in
  (* Stdio.print_string "ss "; Stdio.print_endline (Int.to_string springs_start);
  Stdio.print_string "sc "; Stdio.print_endline (Int.to_string spring_count);
  Stdio.print_string "chd "; Stdio.print_endline (Int.to_string checksums_hd);
  Stdio.print_string "fits "; Stdio.print_endline (Int.to_string num_fits);
  Stdio.print_string "min "; Stdio.print_endline (Int.to_string min_start);
  Stdio.print_string "max "; Stdio.print_endline (Int.to_string max_start_excl);
  Stdio.print_string "stl "; Stdio.print_endline (Int.to_string (List.length springs_tl)); *)
  List.range min_start max_start_excl
  (* |> List.map ~f:(fun offset -> Stdio.print_string "@1 "; Stdio.print_endline (Int.to_string offset); offset) *)
  |> List.filter ~f:((<=) 0)
  |> List.filter ~f:(fun offset -> let springs_tl = springs_tl |> List.drop_while ~f:(fun (start, len) -> offset + checksums_hd <= start + len) in
                                   match springs_tl with
                                     [] -> offset + checksums_hd <= record_group_len
                                   | (springs_nxt, _)::_ -> offset + checksums_hd < springs_nxt
                    )
  (* |> List.map ~f:(fun offset -> Stdio.print_string "@2 "; Stdio.print_endline (Int.to_string offset); offset) *)
  |> List.map ~f:(fun offset -> let springs_tl = springs_tl |> List.drop_while ~f:(fun (start, len) -> offset + checksums_hd <= start + len) in
                                non_overlapping_prefixes (offset - 1) checksums_hd checksums_tl
                                |> List.map ~f:(fun (prefix_len, count) -> let record_group_len' = record_group_len - offset - (checksums_hd + 1) in
                                                                  let springs_tl' = springs_tl |> List.map ~f:(fun (start, count) -> (start - (record_group_len - record_group_len'), count)) in
                                                                  let checksums_tl' = List.drop checksums_tl prefix_len in
                                                                  valid_prefixes' record_group_len' springs_tl' checksums_tl'
                                                                  |> uncompress
                                                                  |> (fun lens -> uncompress [(lens, count);] |> List.concat)
                                                                  |> List.map ~f:((+) prefix_len))
                                |> List.concat)
  |> List.map ~f:(List.map ~f:((+) 1))
  |> List.concat
  |> compress

and valid_prefixes' record_group_len damaged_springs checksum_groups =
  (* Stdio.print_string "X "; Stdio.print_endline (Int.to_string record_group_len); *)
  if record_group_len <= 0 then
    [(0, 1)]
  else
  match (damaged_springs, checksum_groups) with
    ([], []) -> [(0, 1)]
  | ([], checksums_hd::checksums_tl) -> non_overlapping_prefixes record_group_len checksums_hd checksums_tl
  | (_, []) ->  []
  | (springs_hd::springs_tl, checksums_hd::checksums_tl) ->
    overlapping_prefixes record_group_len springs_hd springs_tl checksums_hd checksums_tl

let damaged_springs record_group =
  record_group
  |> List.filter_mapi ~f:(fun i c -> if Char.equal c '#' then Some(i) else None)
  |> List.group ~break:(fun a b -> not (Int.equal 1 (b - a)))
  |> List.map ~f:(fun g -> (List.hd_exn g, List.length g))


let valid_prefixes_fast record_group checksum_groups =
  valid_prefixes' (List.length record_group) (damaged_springs record_group) checksum_groups

let split_record' record =
  let break a b = match (a, b) with
                    ('?', '#') -> false
                  | ('#', '?') -> false
                  | _ -> not (Char.equal a b)
  in
  record
  |> List.group ~break:break
  |> List.filter ~f:(fun g -> g |> List.hd_exn |> Char.equal '.' |> not)

let spring_groups_remaining groups =
  groups
  |> List.filter ~f:(List.exists ~f:(Char.equal '#'))
  |> List.is_empty
  |> not

let rec count_arrangements' groups checksum_groups =
  match (groups, checksum_groups) with
    ([], []) -> 1
  | ([], _) -> 0
  | (_, []) -> if spring_groups_remaining groups then 0 else 1
  | (group::tl, _) -> match List.hd_exn group with (* TODO: remove match *)
                   '.' -> count_arrangements' tl checksum_groups
                 | _ -> let prefixes = valid_prefixes_fast group checksum_groups in
                        prefixes
                          |> List.map ~f:(fun (len, count) -> count * count_arrangements' tl (List.drop checksum_groups len))
                          |> List.fold ~init:0 ~f:(+)

let rec repeat acc list n sep =
  if Int.equal n 0 then
    List.concat (List.tl_exn acc)
  else
    repeat (sep::list::acc) list (n - 1) sep

let part2 puzzle =
  let fast (record, checksum_groups) = count_arrangements' (split_record' record) checksum_groups in
  let extended factor (record, checksum_groups) = (repeat [] record factor ['?'], repeat [] checksum_groups factor []) in
  puzzle
  |> List.map ~f:(extended 1)
  |> List.mapi  ~f:(fun i case -> Stdio.print_string "Case # "; Stdio.print_endline (Int.to_string i);fast case)
  |> List.map ~f:(fun n -> Stdio.print_string "== ";Stdio.print_endline (Int.to_string n); n)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
