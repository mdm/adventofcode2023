open Base

type cube = {
  x: int;
  y: int;
  z: int;
}

type brick = {
  id: int;
  cubes: cube list;
}

let coords_to_bricks id coords =
  let ((x1, y1, z1), (x2, y2, z2)) =
    match coords with
      [[x1; y1; z1]; [x2; y2; z2]] -> ((x1, y1, z1), (x2, y2, z2))
    | _ -> failwith "Invalid coords"
  in
  let x_min = Int.min x1 x2 in
  let x_max = Int.max x1 x2 in
  let y_min = Int.min y1 y2 in
  let y_max = Int.max y1 y2 in
  let z_min = Int.min z1 z2 in
  let z_max = Int.max z1 z2 in
  let rec loop x y z acc =
    let acc = {x; y; z} :: acc in
    if x < x_max then
      loop (x + 1) y z acc
    else
      if y < y_max then
        loop x_min (y + 1) z acc
      else
        if z < z_max then
          loop x_min y_min (z + 1) acc
        else
          {id; cubes = acc}
  in
  loop x_min y_min z_min []

let parse_coords brick =
  brick
  |> String.split ~on:','
  |> List.map ~f:Int.of_string

let parse_line id line =
  line
  |> String.split ~on:'~'
  |> List.map ~f:parse_coords
  |> (fun coords -> coords_to_bricks id coords)

let parse input =
  let lines = String.split_lines input in
  lines
  |> List.mapi ~f:parse_line

let print_brick brick =
  Stdio.printf "Brick %d\n" brick.id;
  List.iter brick.cubes ~f:(fun cube ->
    Stdio.printf "  %d %d %d\n" cube.x cube.y cube.z)  

let can_drop_cube bricks id cube =
  let blocker brick =
    List.exists brick.cubes ~f:(fun c -> c.x = cube.x && c.y = cube.y && c.z = cube.z - 1)  
  in
  let falling =
    bricks
    |> List.filter ~f:blocker
    |> List.for_all ~f:(fun brick -> brick.id = id)
  in
  (* Stdio.printf "Can drop %d %d %d: %b\n" cube.x cube.y cube.z (cube.z > 1 && falling); *)
  cube.z > 1 && falling

let can_drop_brick bricks brick =
  (* Stdio.printf "Can drop brick %d\n" brick.id; *)
  List.for_all brick.cubes ~f:(can_drop_cube bricks brick.id)

let drop bricks brick =
  List.map bricks ~f:(fun b ->
    if b.id = brick.id then
      {b with cubes = List.map b.cubes ~f:(fun c -> {c with z = c.z - 1})}
    else
      b)

let rec drop_all bricks =
  (* Stdio.printf "\nDropping\n"; *)
  let droppable = List.filter bricks ~f:(can_drop_brick bricks) in
  if List.is_empty droppable then
    bricks
  else
    drop_all (List.fold droppable ~init:bricks ~f:drop)

let rec falling_count acc bricks =
  let falling =
    List.filter bricks ~f:(can_drop_brick bricks)
  in
  let drop_falling =
    List.fold falling ~init:bricks ~f:drop
  in
  if List.is_empty falling then
    acc
    |> List.dedup_and_sort ~compare:(fun a b -> Int.compare a.id b.id)
    |> List.length
  else
    falling_count (List.append acc falling) drop_falling

let unstable_count bricks brick =
  let bricks = 
    bricks
    |> List.filter ~f:(fun b -> b.id <> brick.id)
  in
  let result = falling_count [] bricks in
  (* Stdio.printf "Unstable count %d: %d\n" brick.id result; *)
  result

let can_disintegrate bricks brick =
  bricks
  |> List.filter ~f:(fun b -> b.id <> brick.id)
  |> (fun others -> List.filter others ~f:(can_drop_brick others))
  |> List.is_empty
  
let part1 puzzle =
  puzzle
  (* |> List.map ~f:(fun brick -> print_brick brick; brick) *)
  |> drop_all
  (* |> List.map ~f:(fun brick -> print_brick brick; brick) *)
  |> (fun settled -> List.filter settled ~f:(fun b -> can_disintegrate settled b))
  (* |> List.map ~f:(fun brick -> print_brick brick; brick) *)
  |> List.length
  |> Int.to_string
  
let part2 puzzle =
  puzzle
  |> drop_all
  |> (fun settled -> Stdio.printf "settled\n"; settled)
  |> (fun settled -> List.mapi settled ~f:(fun i b -> Stdio.printf "%d\n" i; unstable_count settled b))
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
