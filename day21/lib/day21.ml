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

let neighbors puzzle x y =
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (offset_x, offset_y) -> (x + offset_x, y + offset_y))
  |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> on_grid puzzle neighbor_x neighbor_y)

let bfs map start target =
  let visited = Hashtbl.create (module Int) in
  let queue = Queue.create () in
  let update distance (neighbor_x, neighbor_y) = match Hashtbl.add visited ~key:(neighbor_y * (Array.length map.(0)) + neighbor_x) ~data:distance with
    `Ok -> Queue.enqueue queue (neighbor_x, neighbor_y)
  | `Duplicate -> ()
  in
  let rec bfs_inner acc =
    let front = Queue.dequeue queue in
    match front with
      Some((current_x, current_y)) -> 
        let distance = Hashtbl.find_exn visited (current_y * (Array.length map.(0)) + current_x) in
        neighbors map current_x current_y
        |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> not (Char.equal map.(neighbor_y).(neighbor_x) '#'))
        |> List.iter ~f:(update (distance + 1));
        if distance <= target && distance % 2 = target % 2 then
          bfs_inner ((current_x, current_y)::acc)
        else bfs_inner acc
    | None -> acc
  in
  update 0 start;
  bfs_inner []
  |> List.length
  
let part1 puzzle target =
  let (map, start) = puzzle in
  bfs map start target
  |> Int.to_string

let part2 _puzzle =
  0
  |> Int.to_string
