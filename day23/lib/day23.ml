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

let pair_equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2

let bfs map on_grid start nodes use_slopes =
  let visited = Hashtbl.create (module String) in
  let key x y = Int.to_string x ^ "," ^ Int.to_string y in  
  let queue = Queue.create () in
  let update distance (neighbor_x, neighbor_y) = match Hashtbl.add visited ~key:(key neighbor_x neighbor_y) ~data:distance with
    `Ok -> Queue.enqueue queue (neighbor_x, neighbor_y)
  | `Duplicate -> ()
  in
  let add acc x y distance = {destination = (x, y); distance}::acc in
  let rec bfs_inner acc =
    let front = Queue.dequeue queue in
    match front with
      Some((current_x, current_y)) -> 
        let distance = Hashtbl.find_exn visited (key current_x current_y) in
        if not (pair_equal (current_x, current_y) start) && List.mem nodes (current_x, current_y) ~equal:pair_equal then
          bfs_inner (add acc current_x current_y distance)
        else
          (neighbors map on_grid current_x current_y
          |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> not (Char.equal map.(neighbor_y).(neighbor_x) '#'))
          |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> use_slopes || not (Char.equal map.(neighbor_y).(neighbor_x) '>' && neighbor_x <= current_x))
          |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> use_slopes || not (Char.equal map.(neighbor_y).(neighbor_x) '<' && neighbor_x >= current_x))
          |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> use_slopes || not (Char.equal map.(neighbor_y).(neighbor_x) 'v' && neighbor_y <= current_y))
          |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> use_slopes || not (Char.equal map.(neighbor_y).(neighbor_x) '^' && neighbor_y >= current_y))
          |> List.iter ~f:(update (distance + 1));
          bfs_inner acc)
    | None -> acc
  in
  update 0 start;
  bfs_inner []

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

let add_edges map nodes use_slopes =
  List.map nodes ~f:(fun (x, y) -> {position = (x, y); edges = bfs map on_grid (x, y) nodes use_slopes})

let longest_path nodes start goal =
  let node_of_pos pos = List.find_exn nodes ~f:(fun n -> pair_equal n.position pos) in
  let rec longest_path_inner visited current distance =
    if pair_equal current.position goal then
      distance
    else
      let visited = current::visited in
      current.edges
      |> List.filter ~f:(fun edge -> not (List.exists visited ~f:(fun n -> pair_equal n.position edge.destination)))
      |> List.map  ~f:(fun edge -> longest_path_inner visited (node_of_pos edge.destination) (distance + edge.distance))
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
  in
  longest_path_inner [] (node_of_pos start) 0

let part1 puzzle =
  let (map, start, goal) = puzzle in
  let nodes = find_nodes map in
  let nodes = List.concat [nodes; [start; goal]] in
  let nodes = add_edges map nodes false in
  longest_path nodes start goal
  |> Int.to_string

let part2 puzzle =
  let (map, start, goal) = puzzle in
  let nodes = find_nodes map in
  let nodes = List.concat [nodes; [start; goal]] in
  let nodes = add_edges map nodes true in
  longest_path nodes start goal
  |> Int.to_string
