open Base

let parse input =
  let lines = String.split_lines input in
  lines |> List.map ~f:(fun line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string)

let rec pairs list =
  match list with
    x::y::_ -> (x, y)::(pairs (List.tl_exn list))
  | _ -> []

let differences list =
  list
  |> pairs
  |> List.map ~f:(fun (x, y) -> y - x)

let rec diffs_until_all_zeros list =
  let all_zeros = List.for_all list ~f:(Int.equal 0) in
  if all_zeros then [list] else list::(diffs_until_all_zeros (differences list))

let next_element list =
  let diff_pairs = diffs_until_all_zeros list
                   |> List.map ~f:List.rev
                   |> List.rev
                   |> List.tl_exn
  in
  diff_pairs
  |> List.fold ~init:0 ~f:(fun acc list -> acc + List.hd_exn list)

let part1 puzzle =
  puzzle
  |> List.map ~f:next_element
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let previous_element list =
  let diff_pairs = diffs_until_all_zeros list
                    |> List.rev
                    |> List.tl_exn
  in
  diff_pairs
  |> List.fold ~init:0 ~f:(fun acc list -> (List.hd_exn list) - acc)
  
let part2 puzzle =
  puzzle
  |> List.map ~f:previous_element
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
