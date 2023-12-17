open Base

type block =
  { x: int;
    y: int;
    dir_x: int;
    dir_y: int;
    straight: int;
    heat_loss: int;
    prev_key: string;
  }


module Heap = Hash_heap.Make (String)

let parse_line line =
  line
  |> String.to_array
  |> Array.map ~f:(Char.get_digit_exn)
  
let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:parse_line
  |> Array.of_list

let key _puzzle node =
  String.concat [Int.to_string(node.x); ";"; Int.to_string(node.y); ";"; Int.to_string(node.dir_x); ";"; Int.to_string(node.dir_y); ";"; Int.to_string(node.straight); ]

let neighbors puzzle visited current min_straight max_straight =
  [
    (current.dir_x, current.dir_y, false); (* go straight *)    
    (current.dir_y, -current.dir_x, true); (* turn left *)
    (-current.dir_y, current.dir_x, true); (* turn left *)
  ]
  |> List.filter ~f:(fun (_, _, turn) -> if turn then current.straight >= min_straight else current.straight < max_straight)
  |> List.map ~f:(fun (dir_x, dir_y, turn) -> (current.x + dir_x, current.y + dir_y, dir_x, dir_y, turn))
  |> List.filter ~f:(fun (x, y, _, _, _) -> x >= 0 && x < (Array.length puzzle.(0)) && y >= 0 && y < (Array.length puzzle))
  |> List.map ~f:(fun (x, y, dir_x, dir_y, turn) -> if turn then
                                                      { x;
                                                        y;
                                                        dir_x;
                                                        dir_y;
                                                        straight = 1;
                                                        heat_loss = current.heat_loss + puzzle.(y).(x);
                                                        prev_key = key puzzle current;
                                                      }
                                                    else
                                                      { x;
                                                        y;
                                                        dir_x;
                                                        dir_y; straight = current.straight + 1;
                                                        heat_loss = current.heat_loss + puzzle.(y).(x);
                                                        prev_key = key puzzle current;
                                                      }
                 )
  |> List.filter ~f:(fun neighbor -> Hashtbl.find visited (key puzzle neighbor) |> Option.is_none)

let dijkstra puzzle dir_x dir_y min_straight max_straight =
  let visited = Hashtbl.create (module String) in
  let queue = Heap.create (fun a b -> Int.compare a.heat_loss b.heat_loss) in
  let rec path current =
    Stdio.print_string "(";
    Stdio.print_string (Int.to_string current.x);
    Stdio.print_string ", ";
    Stdio.print_string (Int.to_string current.y);
    Stdio.print_string ")";
    if String.equal current.prev_key "START" then
      (Stdio.print_endline "";)
    else
      (Stdio.print_string " -> ";
      path (Hashtbl.find_exn visited current.prev_key))
  in
  let rec dijkstra' print_path =
    let front = Heap.pop queue in
      match front with
        Some(current) ->
          if current.x = (Array.length puzzle.(0) - 1) && current.y = (Array.length puzzle - 1) && current.straight >= min_straight then
            (if print_path then path current else ();
            current.heat_loss)
          else
            (Hashtbl.add_exn visited ~key:(key puzzle current) ~data:current;
            neighbors puzzle visited current min_straight max_straight
            |> List.iter ~f:(fun neighbor -> match Heap.find queue (key puzzle neighbor) with
                                               Some(old) -> if neighbor.heat_loss < old.heat_loss then
                                                              Heap.replace queue ~key:(key puzzle neighbor) ~data:neighbor
                                                            else
                                                              ()
                                             | None -> Heap.push_exn queue ~key:(key puzzle neighbor) ~data:neighbor

                            );
            dijkstra' print_path)
      | None -> -1
  in
  let start = { x = 0; y = 0; dir_x; dir_y; heat_loss = 0; straight = 0; prev_key = "START"} in
  Heap.push_exn queue ~key:(key puzzle start) ~data:start;
  dijkstra' false

let part1 puzzle =
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (dir_x, dir_y) -> dijkstra puzzle dir_x dir_y 0 3)
  |> List.filter ~f:Int.is_non_negative
  |> List.reduce ~f:Int.min
  |> Option.value_exn
  |> Int.to_string

let part2 puzzle =
  [(0, -1); (1, 0); (0, 1); (-1, 0);]
  |> List.map ~f:(fun (dir_x, dir_y) -> dijkstra puzzle dir_x dir_y 4 10)
  |> List.filter ~f:Int.is_non_negative
  |> List.reduce ~f:Int.min
  |> Option.value_exn
  |> Int.to_string
