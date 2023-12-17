open Base

type block =
  { x: int;
    y: int;
    dir_x: int;
    dir_y: int;
    straight: int;
    heat_loss: int;
    prev_key: int;
  }


module Heap = Hash_heap.Make (Int)

let parse_line line =
  line
  |> String.to_array
  |> Array.map ~f:(Char.get_digit_exn)
  
let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:parse_line
  |> Array.of_list

let key puzzle x y =
  y * (Array.length puzzle.(0)) + x

let neighbors puzzle visited current =
  [
    (current.dir_x, current.dir_y, false); (* go straight *)    
    (current.dir_y, -current.dir_x, true); (* turn left *)
    (-current.dir_y, current.dir_x, true); (* turn left *)
  ]
  |> List.map ~f:(fun (dir_x, dir_y, turn) -> (current.x + dir_x, current.y + dir_y, dir_x, dir_y, turn))
  |> List.filter ~f:(fun (x, y, _, _, _) -> x >= 0 && x < (Array.length puzzle.(0)) && y >= 0 && y < (Array.length puzzle))
  |> List.filter ~f:(fun (x, y, _, _, _) -> Hashtbl.find visited (key puzzle x y) |> Option.is_none)
  |> List.map ~f:(fun (x, y, dir_x, dir_y, turn) -> if turn then
    { x; y; dir_x; dir_y; straight = 0; heat_loss = current.heat_loss + puzzle.(y).(x); prev_key = key puzzle current.x current.y; }
  else
    { x; y; dir_x; dir_y; straight = current.straight + 1; heat_loss = current.heat_loss + puzzle.(y).(x); prev_key = key puzzle current.x current.y; }
    )
  |> List.filter ~f:(fun neighbor -> neighbor.straight < 3)

let dijkstra puzzle dir_x dir_y =
  let visited = Hashtbl.create (module Int) in
  let queue = Heap.create (fun a b -> if (Int.compare a.heat_loss b.heat_loss) = 0 then Int.compare a.straight b.straight else Int.compare a.heat_loss b.heat_loss) in
  let rec path current =
    Stdio.print_string "(";
    Stdio.print_string (Int.to_string current.x);
    Stdio.print_string ", ";
    Stdio.print_string (Int.to_string current.y);
    Stdio.print_string ") -> ";
    if current.prev_key = 0 then
      (Stdio.print_endline "(0, 0)";)
    else
      path (Hashtbl.find_exn visited current.prev_key)
  in
  let rec dijkstra' () =
    let front = Heap.pop queue in
      match front with
        Some(current) ->
          Stdio.print_string "CRNT ";
          Stdio.print_string (Int.to_string current.x);
          Stdio.print_string " ";
          Stdio.print_string (Int.to_string current.y);
          Stdio.print_string " -> ";
          Stdio.print_endline (Int.to_string current.straight);
          if current.x = (Array.length puzzle.(0) - 1) && current.y = (Array.length puzzle - 1) then
            (let current = Hashtbl.find_exn visited (key puzzle 8 0) in
            path current;
            current.heat_loss)
          else
            (Hashtbl.add_exn visited ~key:(key puzzle current.x current.y) ~data:current;
            neighbors puzzle visited current
            |> List.iter ~f:(fun neighbor ->           Stdio.print_string "NGH ";
            Stdio.print_string (Int.to_string neighbor.x);
            Stdio.print_string " ";
            Stdio.print_string (Int.to_string neighbor.y);
            Stdio.print_string " -> ";
            Stdio.print_endline (Int.to_string neighbor.straight);
    match Heap.find queue (key puzzle neighbor.x neighbor.y) with
                                              Some(old) -> if neighbor.heat_loss < old.heat_loss || (neighbor.heat_loss = old.heat_loss &&neighbor.straight < old.straight) then
                                                              Heap.replace queue ~key:(key puzzle neighbor.x neighbor.y) ~data:neighbor
                                                            else
                                                              ()
                                            | None -> Heap.push_exn queue ~key:(key puzzle neighbor.x neighbor.y) ~data:neighbor

                            );
            dijkstra' ())
      | None -> -1
  in
  Heap.push_exn queue ~key:(key puzzle 0 0) ~data:{ x = 0; y = 0; dir_x; dir_y; heat_loss = 0; straight = -1; prev_key = 0};
  dijkstra' ()

let part1 puzzle =
  (* [(0, -1); (1, 0); (0, 1); (-1, 0);] *)
  (* [(0, -1);] *)
  [(1, 0);]
  |> List.map ~f:(fun (dir_x, dir_y) -> Stdio.print_endline "---"; dijkstra puzzle dir_x dir_y)
  |> List.filter ~f:(fun hl -> Stdio.print_string "*** "; Stdio.print_endline (Int.to_string hl); Int.is_non_negative hl)
  |> List.reduce ~f:Int.min
  |> Option.value_exn
  |> Int.to_string

let part2 _puzzle =
  0
  |> Int.to_string
