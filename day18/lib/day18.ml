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

type horizontal = {
  y: int;
  x_start: int;
  x_end: int;
}

let horizontals puzzle =
  let horizontals' acc stretch =
    let ((x, y), hs) = acc in
    let (_, _, encoded) = stretch in
    let direction = String.drop_prefix encoded 5 in
    let distance = String.prefix encoded 5 |> (fun s -> String.concat ["0x"; s]) |> Int.Hex.of_string in
    let pos = match direction with
                | "3" -> (x, y - distance)
                | "1" -> (x, y + distance)
                | "2" -> (x - distance, y)
                | "0" -> (x + distance, y)
                | _ -> assert false
    in
    let h = match direction with
                    | "2" -> [{ y; x_start = x - distance; x_end = x; }]
                    | "0" -> [{ y; x_start = x; x_end = x + distance; }]
                    | _ -> []
    in
    (pos, h::hs)
  in
  puzzle
  |> List.fold ~init:((0, 0), []) ~f:horizontals'
  |> (fun (_, trench) -> List.concat trench |> List.sort ~compare:(fun a b -> a.y - b.y))

let count_line hs_active vs_before vs_after =
  let vs = List.concat [vs_before; vs_after] |> List.dedup_and_sort ~compare:Int.compare in
  let pairs = List.zip_exn (List.drop_last_exn vs) (List.drop vs 1) in
  let pair_len (total, inside) (x_start, x_end) =
    let is_horizontal = List.exists hs_active ~f:(fun h -> Int.equal h.x_start x_start && Int.equal h.x_end x_end) in
    if is_horizontal then
      let start_in_before = List.mem vs_before x_start ~equal:Int.equal in
      let end_in_before = List.mem vs_before x_end ~equal:Int.equal in
      let start_in_after = List.mem vs_after x_start ~equal:Int.equal in
      let end_in_after = List.mem vs_after x_end ~equal:Int.equal in
      let switch = (start_in_before && end_in_before) || (start_in_after && end_in_after) in
      let inside = if switch then not inside else inside in
      (total + (x_end - x_start + 1), inside)
    else
      let inside = not inside in
      let len = if inside then x_end - x_start + 1 else 0 in
      let len = if inside && List.exists hs_active ~f:(fun h -> Int.equal h.x_start x_end) then len - 1 else len in
      let len = if inside && List.exists hs_active ~f:(fun h -> Int.equal h.x_end x_start) then len - 1 else len in
      (total + len, inside)
  in
  List.fold pairs ~init:(0, false) ~f:pair_len |> fst

let count_block vs_before y_start y_end =
  let height = y_end - y_start - 1 in
  let pairs = List.zip_exn (List.drop_last_exn vs_before) (List.drop vs_before 1) in
  let pair_len (total, inside) (x_start, x_end) =
    let inside = not inside in
    let size = if inside then (x_end - x_start + 1) * height else 0 in
    (total + size, inside)
  in
  List.fold pairs ~init:(0, false) ~f:pair_len |> fst

let count_interior_fast hs =
  let split hs = 
    let first = List.hd_exn hs in
    List.partition_tf hs ~f:(fun h -> Int.equal h.y first.y)
  in
  let vs_after hs vs_before =
    let vs_active = List.map hs ~f:(fun h -> [h.x_start; h.x_end]) |> List.concat in
    List.concat [
      List.filter vs_before ~f:(fun x -> not (List.mem vs_active x ~equal:Int.equal));
      List.filter vs_active ~f:(fun x -> not (List.mem vs_before x ~equal:Int.equal))
    ] |> List.dedup_and_sort ~compare:Int.compare
  in
  let rec step total hs vs_before y_start =
    if List.is_empty hs then total
    else
      let (hs_active, hs_rest) = split hs in
      let y_end = (List.hd_exn hs_active).y in
      let block = count_block vs_before y_start y_end in
      let vs_after' = vs_after hs_active vs_before in
      let line = count_line hs_active vs_before vs_after' in
      step (total + block + line) hs_rest vs_after' y_end
  in
  let (hs_active, hs_rest) = split hs in
  let y_end = (List.hd_exn hs_active).y in
  let vs_after' = vs_after hs_active [] in
  step (count_line hs_active [] vs_after') hs_rest vs_after' y_end

let part1 puzzle =
  puzzle
  |> dig_trench
  |> count_interior
  |> Int.to_string

let part2 puzzle =
  puzzle
  |> horizontals
  |> count_interior_fast
  |> Int.to_string
