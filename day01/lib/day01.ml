open Base

let parse input = String.split_lines input

let first_digit line =
  line
  |> List.findi ~f:(fun _ c -> Char.is_digit c)
  |> Option.bind ~f:(fun (i, c) -> Char.get_digit c
                                   |> Option.map ~f:(fun d -> (i, d)))

let last_digit line =
  let line = List.rev line in
  first_digit line
  |> Option.map ~f:(fun (i, d) -> (List.length line - i - 1, d))

let digit_name_starting_here line _ digit_name =
  String.is_prefix line ~prefix:digit_name

let digit_named_here (line : string) =
  let maybe_i = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
                |> List.findi ~f:(digit_name_starting_here line) in

  Option.map maybe_i ~f:(fun (i, _) -> i + 1)

let cons_if_some start option list =
  match option with
    Some(value) -> (start, value)::list
  | None -> list

let rec digit_names start acc line =
  match line with
    [] -> List.rev acc
  | hd::tl -> digit_names (start + 1) (cons_if_some start (digit_named_here (String.of_list (hd::tl)) ) acc) tl

let compare a b ~f =
  match (a, b) with
    (Some(i, a), Some(j, b)) -> if f i j then a else b
  | (Some((_, a)), None) -> a
  | (None, Some((_, b))) -> b
  | _ -> assert false

let calibration_value_1 (line : string) =
  let first = first_digit (String.to_list line) |> Option.value_exn |> snd in
  let last = last_digit (String.to_list line) |> Option.value_exn |> snd in
  10 * first + last

let calibration_value_2 (line : string) =
  let named_digits = digit_names 0 [] (String.to_list line) in
  let first_word = List.hd named_digits in
  let last_word = List.rev named_digits |> List.hd in
  let first = compare (first_digit (String.to_list line)) first_word ~f:(<) in
  let last = compare (last_digit (String.to_list line)) last_word ~f:(>) in
  10 * first + last

let part1 puzzle =
  List.map ~f:calibration_value_1 puzzle
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

  let part2 puzzle =
  List.map ~f:calibration_value_2 puzzle
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
