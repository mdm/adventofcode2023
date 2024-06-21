open Base

let parse_line line =
  line
  |> String.to_array

let parse input =
  let lines = String.split_lines input in
  let map = lines
            |> List.map ~f:parse_line
            |> Array.of_list
  in
  let start = map
              |> Array.filter_mapi ~f:(fun y row -> row
                                                    |> Array.findi ~f:(fun _ tile -> Char.equal tile 'S')
                                                    |> Option.map ~f:(fun (x, _) -> (x, y)))
              |> Fn.flip Array.get 0
  in
  (map, start)

let on_grid puzzle x y = x >= 0 && x < Array.length puzzle.(0) && y >= 0 && y < Array.length puzzle

let neighbors puzzle on_grid x y =
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (offset_x, offset_y) -> (x + offset_x, y + offset_y))
  |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> on_grid puzzle neighbor_x neighbor_y)

let bfs map init add on_grid start limit =
  let width = Array.length map.(0) in
  let height = Array.length map in
  let visited = Hashtbl.create (module String) in
  let key x y = Int.to_string x ^ "," ^ Int.to_string y in  
  let queue = Queue.create () in
  let update distance (neighbor_x, neighbor_y) = match Hashtbl.add visited ~key:(key neighbor_x neighbor_y) ~data:distance with
    `Ok -> Queue.enqueue queue (neighbor_x, neighbor_y)
  | `Duplicate -> ()
  in
  let rec bfs_inner acc =
    let front = Queue.dequeue queue in
    match front with
      Some((current_x, current_y)) -> 
        let distance = Hashtbl.find_exn visited (key current_x current_y) in
        let next = if distance < limit then
          neighbors map on_grid current_x current_y
        else
          []
        in
        next
        |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> not (Char.equal map.(neighbor_y % height).(neighbor_x % width) '#'))
        |> List.iter ~f:(update (distance + 1));
        bfs_inner (add acc current_x current_y distance)
    | None -> acc
  in
  update 0 start;
  bfs_inner init
  
let part1 puzzle =
  let (map, start) = puzzle in
  bfs map [] (fun acc x y d -> (x, y, d)::acc) on_grid start 64
  |> List.filter ~f:(fun (_, _, d) -> d % 2 = 64 % 2)
  |> List.length
  |> Int.to_string

let precalc map start =
  let width = Array.length map.(0) in
  let height = Array.length map in
  let (start_x, start_y) = start in
  let init () = Hashtbl.create (module Int) in
  let add acc _x _y d = Hashtbl.update acc d ~f:(fun old -> 1 + Option.value old ~default:0); acc in
  let lookups = [
    bfs map (init ()) add on_grid (width - 1, height - 1) (width * height) (* north west quadrant *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid (start_x, height - 1) (width * height) (* north strip *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid (0, height - 1) (width * height) (* north east quadrant *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid (width - 1, start_y) (width * height) (* west strip *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid start (width * height) (* center *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid (0, start_y) (width * height) (* east strip *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid (width - 1, 0) (width * height) (* south west quadrant *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid (start_x, 0) (width * height) (* south strip *)
    |> Hashtbl.to_alist;
    bfs map (init ()) add on_grid (0, 0) (width * height) (* south east quadrant *)
    |> Hashtbl.to_alist;
  ] in
  let offsets = bfs map [] (fun acc x y d -> (x, y, d)::acc) on_grid start (width * height) in
  let offsets = [
    1 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = 0 && y = 0) |> (fun (_, _, d) -> d));
    0 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = start_x && y = 0) |> (fun (_, _, d) -> d));
    1 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = width - 1 && y = 0) |> (fun (_, _, d) -> d));
    0 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = 0 && y = start_y) |> (fun (_, _, d) -> d));
    0;
    0 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = width - 1 && y = start_y) |> (fun (_, _, d) -> d));
    1 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = 0 && y = height - 1) |> (fun (_, _, d) -> d));
    0 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = start_x && y = height - 1) |> (fun (_, _, d) -> d));
    1 + (List.find_exn offsets ~f:(fun (x, y, _) -> x = width - 1 && y = height - 1) |> (fun (_, _, d) -> d));
  ] in
  List.zip_exn lookups offsets

  let full_odd lookup target =
    List.filter lookup ~f:(fun (d, _) -> not (d % 2 = target % 2))
    |> List.fold ~init:0 ~f:(fun acc (_, count) -> acc + count)

  let full_even lookup target =
    List.filter lookup ~f:(fun (d, _) -> d % 2 = target % 2)
    |> List.fold ~init:0 ~f:(fun acc (_, count) -> acc + count)

  let size target offset height = (target - offset) / height - 1
  let rest_upper target offset height = (target - offset) % height
  let rest_lower target offset height = (target - offset) % height + height
  let partial_upper_odd lookup rest_upper target =
    List.filter lookup ~f:(fun (d, _) -> d < rest_upper && not (d % 2 = target % 2))
    |> List.fold ~init:0 ~f:(fun acc (_, count) -> acc + count)

  let partial_upper_even lookup rest_upper target =
    List.filter lookup ~f:(fun (d, _) -> d < rest_upper && d % 2 = target % 2)
      |> List.fold ~init:0 ~f:(fun acc (_, count) -> acc + count)

  let partial_lower_odd lookup rest_lower target =
    List.filter lookup ~f:(fun (d, _) -> d < rest_lower && not (d % 2 = target % 2))
    |> List.fold ~init:0 ~f:(fun acc (_, count) -> acc + count)

  let partial_lower_even lookup rest_lower target =
    List.filter lookup ~f:(fun (d, _) -> d < rest_lower && d % 2 = target % 2)
    |> List.fold ~init:0 ~f:(fun acc (_, count) -> acc + count)

  let quadrant lookup offset height target =
    let full_odd = full_odd lookup target in
    let full_even = full_even lookup target in
    let size = size target offset height in
    let rest_upper = rest_upper target offset height in
    let rest_lower = rest_lower target offset height in
    let partial_upper_odd = partial_upper_odd lookup rest_upper target in
    let partial_upper_even = partial_upper_even lookup rest_upper target in
    let partial_lower_odd = partial_lower_odd lookup rest_lower target in
    let partial_lower_even = partial_lower_even lookup rest_lower target in
    if size % 2 = 0 then
      ((size / 2) ** 2) * full_even + ((size / 2) * (size / 2 + 1)) * full_odd + (size + 1) * partial_lower_even + (size + 2) * partial_upper_odd
    else
      ((size / 2 + 1) ** 2) * full_even + ((size / 2) * (size / 2 + 1)) * full_odd + (size + 1) * partial_lower_odd + (size + 2) * partial_upper_even

  let strip lookup offset height target =
    let full_odd = full_odd lookup target in
    let full_even = full_even lookup target in
    let size = size target offset height in
    let rest_upper = rest_upper target offset height in
    let rest_lower = rest_lower target offset height in
    let partial_upper_odd = partial_upper_odd lookup rest_upper target in
    let partial_upper_even = partial_upper_even lookup rest_upper target in
    let partial_lower_odd = partial_lower_odd lookup rest_lower target in
    let partial_lower_even = partial_lower_even lookup rest_lower target in
    if size % 2 = 0 then
      (size / 2) * full_even + (size / 2) * full_odd + partial_lower_even + partial_upper_odd
    else
      (size / 2 + 1) * full_even + (size / 2) * full_odd + partial_lower_odd + partial_upper_even

let part2 puzzle =
  let target = 26501365 in
  let (map, start) = puzzle in
  let height = Array.length map in
  let data = precalc map start in
  List.zip_exn [
    quadrant;
    strip;
    quadrant;
    strip;
    (fun lookup _offset _height _target -> List.filter lookup ~f:(fun (d, _) -> d % 2 = target % 2)
    |> List.fold ~init:0 ~f:(fun acc (_, count) -> acc + count));
    strip;
    quadrant;
    strip;
    quadrant;
  ] data
  |> List.map ~f:(fun (f, (lookup, offset)) -> f lookup offset height target)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
