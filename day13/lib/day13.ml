open Base

let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:String.to_list
  |> List.group ~break:(fun a b -> List.is_empty a || List.is_empty b)
  |> List.filter ~f:(fun group -> group |> List.hd_exn |> List.is_empty |> not)
  |> List.map ~f:(fun group -> group |> List.map ~f:Array.of_list |> Array.of_list)

let rec pairs list =
  match list with
    x::y::_ -> (x, y)::(pairs (List.tl_exn list))
  | _ -> []

let find_horizontal_reflection perfect pattern =
  let candidates = List.range 0 (Array.length pattern) |> pairs in
  let extent top bottom = Int.min (top + 1) ((Array.length pattern) - bottom) in
  let full_reflection top bottom offset = Array.equal Char.equal pattern.(top - offset) pattern.(bottom + offset) in
  let with_smudge top bottom offset = Array.zip_exn pattern.(top - offset) pattern.(bottom + offset)
                                      |> Array.count ~f:(fun (a, b) -> Char.equal a b)
                                      |> Int.equal (Array.length pattern.(0) - 1)
  in
  let is_reflection perfect (top, bottom) = (List.range 0 (extent top bottom)
                                            |> List.count ~f:(full_reflection top bottom),
                                            List.range 0 (extent top bottom)
                                            |> List.count ~f:(with_smudge top bottom))
                                            |> (fun (full, smudge) -> if perfect then Int.equal full (extent top bottom) else Int.equal smudge 1 && Int.equal full (extent top bottom - 1))
  in
  candidates
  |> List.find ~f:(is_reflection perfect)
  |> Option.map ~f:fst
  |> Option.map ~f:((+) 1)
  |> Option.map ~f:(( * ) 100)

let find_vertical_reflection perfect pattern =
  pattern
  |> Array.transpose_exn
  |> find_horizontal_reflection perfect
  |> Option.map ~f:(Fn.flip (/) 100)

let part1 puzzle =
  let find_reflection = [find_horizontal_reflection true; find_vertical_reflection true;] in
  puzzle
  |> List.map ~f:(fun pattern -> find_reflection
                                 |> List.filter_map ~f:(fun f -> f pattern)
                                 |> List.hd_exn)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let part2 puzzle =
  let find_reflection = [find_horizontal_reflection false; find_vertical_reflection false;] in
  puzzle
  |> List.map ~f:(fun pattern -> find_reflection
                                 |> List.filter_map ~f:(fun f -> f pattern)
                                 |> List.hd_exn)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
