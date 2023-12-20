open Base

let parse_condition condition =
  let condition = String.to_list condition in
  let category = List.hd_exn condition in
  let compare = List.nth_exn condition 1 in
  let value = List.drop condition 2 |> String.of_list |> Int.of_string in
  (category, compare, value)

let parse_check check =
  match String.split check ~on:':' with
    next::[] -> (None, next)
  | condition::next::[] -> (Some(parse_condition condition), next)
  | _ -> assert false

let parse_checks checks =
  checks
  |> String.split ~on:','
  |> List.map ~f:parse_check

let parse_workflow workflow =
  let name = workflow
             |> String.split ~on:'{'
             |> List.hd_exn
  in
  let checks = workflow
               |> String.split ~on:'{'
               |> Fn.flip List.nth_exn 1
               |> String.strip ~drop:(Char.equal '}')
               |> parse_checks
  in
  (name, checks)

let parse_category_value category =
  category
  |> String.split ~on:'='
  |> List.last_exn
  |> Int.of_string

let parse_part part =
  part
  |> String.strip ~drop:(fun c -> Char.equal c '{' || Char.equal c '}')
  |> String.split ~on:','
  |> List.map ~f:parse_category_value
  
let parse input =
  let lines = String.split_lines input in
  let (workflows, parts) = lines |> List.split_while ~f:(fun line -> not (String.is_empty line)) in
  let workflows = workflows |> List.map ~f:parse_workflow in
  let parts = List.drop parts 1 |> List.map ~f:parse_part in
  let workflows = Hashtbl.of_alist_exn (module String) workflows in
  (workflows, parts)

let process_condition condition part =
  let (category, compare, condition_value) = condition in
  let part_value = match category with
                     'x' -> List.nth_exn part 0
                   | 'm' -> List.nth_exn part 1
                   | 'a' -> List.nth_exn part 2
                   | 's' -> List.nth_exn part 3
                   | _ -> assert false
  in
  match compare with
    '<' -> part_value < condition_value
  | '>' -> part_value > condition_value
  | _ -> assert false
  

let rec process_part workflows current part =
  let workflow = Hashtbl.find workflows current in
  let rec process_part' workflows workflow part =
      match List.hd_exn workflow with
        (Some(condition), next) -> if process_condition condition part then
                                    process_part workflows next part
                                  else
                                    process_part' workflows (List.tl_exn workflow) part
      | (None, next) -> process_part workflows next part
  in
  if String.equal current "A" || String.equal current "R" then
    String.equal current "A"
  else
    process_part' workflows (Option.value_exn workflow) part

let part1 puzzle =
  let (workflows, parts) = puzzle in
  parts
  |> List.filter ~f:(process_part workflows "in")
  |> List.concat
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let process_condition_combinations condition combinations =
  let (category, compare, condition_value) = condition in
  let index = match category with
                     'x' -> 0
                   | 'm' -> 1
                   | 'a' -> 2
                   | 's' -> 3
                   | _ -> assert false
  in
  let (from, until) = combinations.(index) in
  let sat_combinations = Array.copy combinations in
  let unsat_combinations = Array.copy combinations in
  let (sat_range, unsat_range) = (match compare with
                        '<' -> (
                                  (Int.min from condition_value, Int.min until condition_value),
                                  (Int.max from condition_value, Int.max until condition_value)
                                )
                      | '>' -> (
                                  (Int.max from (condition_value + 1), Int.max until condition_value),
                                  (Int.min from condition_value, Int.min until (condition_value + 1))
                                )
                      | _ -> assert false)
  in
  let (sat_from, sat_until) = sat_range in
  let (unsat_from, unsat_until) = unsat_range in
  sat_combinations.(index) <- sat_range;
  unsat_combinations.(index) <- unsat_range;
  ((sat_until - sat_from, sat_combinations), (unsat_until - unsat_from, unsat_combinations))

let rec process_part_combinations workflows current combinations =
  let workflow = Hashtbl.find workflows current in
  let rec process_part_combinations' workflows workflow combinations =
      match List.hd_exn workflow with
        (Some(condition), next) -> let ((_sat_count, sat_combinations), (_unsat_count, unsat_combinations)) = process_condition_combinations condition combinations in
                                   process_part_combinations workflows next sat_combinations
                                   + process_part_combinations' workflows (List.tl_exn workflow) unsat_combinations
      | (None, next) -> process_part_combinations workflows next combinations
  in
  if String.equal current "A" then
    combinations
    |> Array.map ~f:(fun (from, until) -> until - from)
    |> Array.fold ~init:1 ~f:( * )
  else
    if String.equal current "R" then
      0
    else
      process_part_combinations' workflows (Option.value_exn workflow) combinations

let part2 puzzle =
  let (workflows, _) = puzzle in
  [|(1, 4001); (1, 4001); (1, 4001); (1, 4001);|]
  |> process_part_combinations workflows "in"
  |> Int.to_string
