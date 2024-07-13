open Base

let parse_line parts =
  match parts with
    [left; rights] -> rights
                     |> String.strip
                     |> String.split ~on:' ' 
                     |> List.map ~f:(fun right -> [(left, right); (right, left)])
                     |> List.concat
  | _ -> assert false


let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:(String.split ~on:':')
  |> List.map ~f:parse_line
  |> List.concat

let print_subset name (members, neighbors) =
  let wire_out_s (m, n) = Printf.sprintf "%s-%s" m n in 
  Stdio.printf "%s\n" name;
  Stdio.printf "Members: %s\n" (String.concat members ~sep:", ");
  Stdio.printf "Wires out: %s\n" (String.concat (List.map neighbors ~f:wire_out_s) ~sep:", ")

let divide components =
  let initials = components
                 |> Hashtbl.to_alist
                 |> List.map ~f:(fun (key, values) -> ([key], List.map values ~f:(fun v -> (key,v))))
  in
  let rec divide_inner complete growable =
    match growable with
      [] -> complete
    | (members_a, wires_out_a)::rest ->
      let calc_new_members ms = List.concat [members_a; ms] in
      let calc_new_wires_out ms ws = let ms = calc_new_members ms in
                                     List.concat [wires_out_a; ws]
                                     |> List.filter ~f:(fun (_, n) -> List.mem ms n ~equal:String.equal |> not)
      in
      let (candidates, rest) = List.partition_tf rest
                               ~f:(fun (members, _) -> List.exists members
                                                         ~f:(fun m -> List.mem (List.map wires_out_a ~f:snd) m ~equal:String.equal))
      in
      let (chosen, (members_b, wires_out_b), _) = candidates
                                              |> List.mapi ~f:(fun i (ms, ws) -> (i, (ms, ws), List.length (calc_new_wires_out ms ws)))
                                              |> List.min_elt
                                                 ~compare:(fun (_, _, score1) (_, _, score2) -> Int.compare score1 score2)
                                              |> Option.value_exn
      in
      let rest = List.concat [rest; List.filteri candidates ~f:(fun i _ -> i <> chosen)] in
      let new_members = calc_new_members members_b in
      let new_wires_out = calc_new_wires_out members_b wires_out_b in
      if List.length new_wires_out = 3 then
        divide_inner ((new_members, new_wires_out)::complete) rest
      else
        divide_inner complete ((new_members, new_wires_out)::rest)
  in
  divide_inner [] initials

let dump_graph puzzle filename =
  let edges = puzzle
            |> List.map ~f:(fun (l, r) -> Printf.sprintf "%s -- %s;" l r)
            |> String.concat ~sep:"\n"
  in
  Stdio.Out_channel.write_all filename ~data:(String.concat ["strict graph day25 {\n"; edges; "}"])
  
let part1 puzzle =
  let components = Hashtbl.of_alist_multi (module String) puzzle in
  divide components
  |> List.map ~f:(fun (members, _) -> List.length members)
  |> List.fold ~init:1 ~f:( * )
  |> Int.to_string

let part2 _puzzle =
  "no part two"

