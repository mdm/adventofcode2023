open Base

let parse_line line =
  line |> String.to_list |> Array.of_list
  
let parse input =
  let lines = String.split_lines input in
  lines |> List.map ~f:parse_line |> Array.of_list

let on_grid puzzle x y = x >= 0 && x < Array.length puzzle.(0) && y >= 0 && y < Array.length puzzle

let out_dir puzzle x y in_dir_x in_dir_y =
  match puzzle.(y).(x) with
    '|' -> (match (in_dir_x, in_dir_y) with
             (0, -1) -> Some((0, -1))
           | (1, 0) -> None
           | (0, 1) -> Some((0, 1))
           | (-1, 0) -> None
           | _ -> assert false)
  | '-' -> (match (in_dir_x, in_dir_y) with
             (0, -1) -> None
           | (1, 0) -> Some((1, 0))
           | (0, 1) -> None
           | (-1, 0) -> Some((-1, 0))
           | _ -> assert false)
  | 'L' -> (match (in_dir_x, in_dir_y) with
             (0, -1) -> None
           | (1, 0) -> None
           | (0, 1) -> Some((1, 0))
           | (-1, 0) -> Some((0, -1))
           | _ -> assert false)
  | 'J' -> (match (in_dir_x, in_dir_y) with
             (0, -1) -> None
           | (1, 0) -> Some((0, -1))
           | (0, 1) -> Some((-1, 0))
           | (-1, 0) -> None
           | _ -> assert false)
  | '7' -> (match (in_dir_x, in_dir_y) with
             (0, -1) -> Some((-1, 0))
           | (1, 0) -> Some((0, 1))
           | (0, 1) -> None
           | (-1, 0) -> None
           | _ -> assert false)
  | 'F' -> (match (in_dir_x, in_dir_y) with
             (0, -1) -> Some((1, 0))
           | (1, 0) -> None
           | (0, 1) -> None
           | (-1, 0) -> Some((0, 1))
           | _ -> assert false)
  | '.' -> None
  | 'S' -> None
  | _ -> assert false

let connects puzzle x y prev_x prev_y =
  let in_dir_x = x - prev_x in
  let in_dir_y = y - prev_y in
  let to_pipe = not (Char.equal puzzle.(y).(x) '.') in
  to_pipe && Option.is_some (out_dir puzzle x y in_dir_x in_dir_y)

let next_pipe puzzle x y prev_x prev_y =
  let in_dir_x = x - prev_x in
  let in_dir_y = y - prev_y in
  let (out_dir_x, out_dir_y) = out_dir puzzle x y in_dir_x in_dir_y |> Option.value_exn in
  (x + out_dir_x, y + out_dir_y)
  
let find_loop puzzle =
  let start_line line = line |> Array.findi ~f:(fun _i c -> Char.equal c 'S') in
  let (start_y, start_x) = puzzle
                                |> Array.map ~f:start_line
                                |> Array.findi_exn ~f:(fun _i o -> Option.is_some o)
                                |> (fun start -> (fst start, snd start |> Option.value_exn |> fst))
  in
  let rec find_loop_inner x y prev_x prev_y acc =
    if on_grid puzzle x y then
      if Char.equal puzzle.(y).(x) 'S' then
        Some(acc)
      else
        if connects puzzle x y prev_x prev_y then
          let (next_x, next_y) = next_pipe puzzle x y prev_x prev_y in
          find_loop_inner next_x next_y x y ((x, y)::acc)
        else
          None
    else
      None
  in
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (x, y) -> find_loop_inner (start_x + x) (start_y + y) start_x start_y [(start_x, start_y)])
  |> List.find_exn ~f:Option.is_some
  |> Option.value_exn

let part1 puzzle =
  puzzle
  |> find_loop
  |> List.length
  |> Fn.flip (/) 2
  |> Int.to_string

let in_loop pipe_x pipe_y loop =
  List.mem loop (pipe_x, pipe_y) ~equal:(fun a b -> Int.equal (fst a) (fst b) && Int.equal (snd a) (snd b))

let neighbors puzzle x y =
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (offset_x, offset_y) -> (x + offset_x, y + offset_y))
  |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> on_grid puzzle neighbor_x neighbor_y)

let bfs puzzle loop inner =
  let visited = Hashtbl.create (module Int) in
  let queue = Queue.create () in
  let update (neighbor_x, neighbor_y) = match Hashtbl.add visited ~key:(neighbor_y * (Array.length puzzle.(0)) + neighbor_x) ~data:true with
    `Ok -> Queue.enqueue queue (neighbor_x, neighbor_y)
  | `Duplicate -> ()
  in
  let rec bfs_inner acc =
    let front = Queue.dequeue queue in
    match front with
      Some((current_x, current_y)) -> 
        neighbors puzzle current_x current_y
        |> List.filter ~f:(fun (neighbor_x, neighbor_y) -> not (in_loop neighbor_x neighbor_y loop))
        |> List.iter ~f:update;
        bfs_inner ((current_x, current_y)::acc)
    | None -> acc
  in
  List.iter inner ~f:update;
  bfs_inner []

let rec pairs list =
  match list with
    x::y::_ -> (x, y)::(pairs (List.tl_exn list))
  | _ -> []

let enclosed puzzle loop =
  let ps = pairs ((List.last_exn loop)::(List.rev loop |> List.tl_exn)) in
  let winding =
    let winding' ((prev_x, prev_y), (x, y)) = 
      let in_dir_x = x - prev_x in
      let in_dir_y = y - prev_y in
      let (out_dir_x, out_dir_y) = out_dir puzzle x y in_dir_x in_dir_y |> Option.value_exn in
      if Int.equal out_dir_x in_dir_y && Int.equal out_dir_y (-in_dir_x) then
        -1 (* turn left *)
      else
        if Int.equal out_dir_x (-in_dir_y) && Int.equal out_dir_y in_dir_x then
          1 (* turn right *)
        else
          0
        in
    List.fold ps ~init:0 ~f:(fun acc p -> acc + winding' p)
  in
  let enclosed' ((prev_x, prev_y), (x, y)) =
    let in_dir_x = x - prev_x in
    let in_dir_y = y - prev_y in
    let (out_dir_x, out_dir_y) = out_dir puzzle x y in_dir_x in_dir_y |> Option.value_exn in
    let left (offset_x, offset_y) = match (offset_x, offset_y) with
                             (0, -1) -> (x - 1, y)
                           | (1, 0) -> (x, y - 1)
                           | (0, 1) -> (x + 1, y)
                           | (-1, 0) -> (x, y + 1)
                           | _ -> assert false
    in
    let right (offset_x, offset_y) = match (offset_x, offset_y) with
                             (0, -1) -> (x + 1, y)
                           | (1, 0) -> (x, y + 1)
                           | (0, 1) -> (x - 1, y)
                           | (-1, 0) -> (x, y - 1)
                           | _ -> assert false
    in
    let inner = if winding > 0 then right else left in
    let filter (inner_x, inner_y) =
      if (on_grid puzzle inner_x inner_y) && (not (in_loop inner_x inner_y loop)) then
          Some((inner_x, inner_y);)
      else
        None
    in
    [(in_dir_x, in_dir_y); (out_dir_x, out_dir_y);]
    |> List.filter_map ~f:(fun dir -> dir |> inner |> filter)
  in
  ps
  |> List.map ~f:enclosed'
  |> List.concat
  |> List.dedup_and_sort ~compare:(fun a b -> (fst a * (Array.length puzzle) + snd a) - (fst b * (Array.length puzzle) + snd b))

let part2 puzzle =
  let loop = find_loop puzzle in
  enclosed puzzle loop
  |> bfs puzzle loop
  |> List.length
  |> Int.to_string
