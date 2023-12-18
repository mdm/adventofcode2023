open Base

let parse_line line =
  let parts = line |> String.split ~on:' ' in
  let direction = parts |> List.hd_exn |> String.to_list |> List.hd_exn in
  let distance =  parts |> Fn.flip List.nth_exn 1 |> Int.of_string in
  let color = parts |> Fn.flip List.nth_exn 2 |> String.strip ~drop:(fun c-> String.find "()#" ~f:(Char.equal c) |> Option.is_some) in
  (direction, distance, color)
  
let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:parse_line

let dig_trench puzzle =
  let dig_trench' acc stretch =
    let ((x, y), trench) = acc in
    let (direction, distance, _) = stretch in
    let positions = match direction with
                      'U' -> List.range (y - 1) (y - distance - 1) ~stride:(-1) |> List.map ~f:(fun y -> (x, y))
                    | 'D' -> List.range (y + 1) (y + distance + 1) |> List.map ~f:(fun y -> (x, y))
                    | 'L' -> List.range (x - 1) (x - distance - 1) ~stride:(-1) |> List.map ~f:(fun x -> (x, y))
                    | 'R' -> List.range (x + 1) (x + distance + 1) |> List.map ~f:(fun x -> (x, y))
                    | _ -> assert false
    in
    (List.last_exn positions, positions::trench)
  in
  puzzle
  |> List.fold ~init:((0, 0), []) ~f:dig_trench'
  |> (fun (_, trench) -> List.concat trench)

let print_map map =
  map
  |> Array.map ~f:String.of_array
  |> String.concat_array ~sep:"\n"
  |> Stdio.print_endline
  
let to_map trench =
  let min_x = trench |> List.map ~f:fst |> List.reduce ~f:Int.min |> Option.value_exn in
  let min_y = trench |> List.map ~f:snd |> List.reduce ~f:Int.min |> Option.value_exn in
  let max_x = trench |> List.map ~f:fst |> List.reduce ~f:Int.max |> Option.value_exn in
  let max_y = trench |> List.map ~f:snd |> List.reduce ~f:Int.max |> Option.value_exn in
  let map = Array.make_matrix ~dimx:(max_y - min_y + 3) ~dimy:(max_x - min_x + 3) '.' in
  trench
  |> List.iter ~f:(fun (x, y) -> map.(y - min_y + 1).(x - min_x + 1) <- '#');
  map

let on_grid puzzle x y = x >= 0 && x < Array.length puzzle.(0) && y >= 0 && y < Array.length puzzle

let neighbors puzzle x y =
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (offset_x, offset_y) -> (x + offset_x, y + offset_y))
  |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> on_grid puzzle neighbor_x neighbor_y)

let bfs map =
  let visited = Hashtbl.create (module Int) in
  let queue = Queue.create () in
  let update (neighbor_x, neighbor_y) = match Hashtbl.add visited ~key:(neighbor_y * (Array.length map.(0)) + neighbor_x) ~data:true with
    `Ok -> Queue.enqueue queue (neighbor_x, neighbor_y)
  | `Duplicate -> ()
  in
  let rec bfs_inner acc =
    let front = Queue.dequeue queue in
    match front with
      Some((current_x, current_y)) -> 
        neighbors map current_x current_y
        |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> Char.equal map.(neighbor_y).(neighbor_x) '.')
        |> List.iter ~f:update;
        bfs_inner ((current_x, current_y)::acc)
    | None -> acc
  in
  update (0, 0);
  bfs_inner [] |> List.length

let count_interior trench =
  let map = to_map trench in
  (Array.length map.(0)) * (Array.length map) - (bfs map)

let part1 puzzle =
  puzzle
  |> dig_trench
  |> count_interior
  |> Int.to_string

let part2 _puzzle =
  0
  |> Int.to_string
