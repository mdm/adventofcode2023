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
  let start = map.(0)
              |> Array.findi ~f:(fun _ tile -> Char.equal tile '.')
              |> Option.map ~f:(fun (x, _) -> (x, 0))
              |> Option.value_exn
  in
  let goal = map.(Array.length map - 1)
              |> Array.findi ~f:(fun _ tile -> Char.equal tile '.')
              |> Option.map ~f:(fun (x, _) -> (x, Array.length map - 1))
              |> Option.value_exn
  in
  (map, start, goal)

type edge = {
  destination: (int * int);
  distance: int;
}

type node = {
  position: (int * int);
  edges: edge list;
}

let on_grid map x y = x >= 0 && x < Array.length map.(0) && y >= 0 && y < Array.length map

let neighbors map on_grid x y =
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (offset_x, offset_y) -> (x + offset_x, y + offset_y))
  |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> on_grid map neighbor_x neighbor_y)

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

let is_walkable map x y = Char.equal map.(y).(x) '#' |> not

let is_node map x y =
  is_walkable map x y &&
  (neighbors map on_grid x y
  |> List.filter ~f:(fun (x, y) -> is_walkable map x y)
  |> List.length) >= 3

let find_nodes map =
  map
  |> Array.mapi ~f:(fun y row -> Array.foldi row ~init:[] ~f:(fun x acc _ -> if is_node map x y then (x, y)::acc else acc))
  |> Array.to_list
  |> List.concat

let part1 puzzle =
  let (map, start, _goal) = puzzle in
  let nodes = find_nodes map in
  List.iter nodes ~f:(fun (x, y) -> Stdio.printf "(%d, %d)\n" x y);
  Stdio.printf "# nodes: %d\n" (List.length nodes);
  Stdio.printf "map size: %d x %d\n" (Array.length map.(0)) (Array.length map);
  bfs map [] (fun acc x y d -> (x, y, d)::acc) on_grid start 64
  |> List.filter ~f:(fun (_, _, d) -> d % 2 = 64 % 2)
  |> List.length
  |> Int.to_string

let part2 _puzzle =
  0
  |> Int.to_string
