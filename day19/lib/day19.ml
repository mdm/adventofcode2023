open Base

let parse_condition condition =
  let condition = String.to_list condition in
  let category = List.hd_exn condition in
  let compare = match List.nth_exn condition 1 with
                  '<' -> (<)
                | '>' -> (>)
                | _ -> assert false
  in
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
  compare part_value condition_value

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

let part2 _puzzle =
  0
  |> Int.to_string
