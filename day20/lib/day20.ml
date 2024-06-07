open Base
open Stdio

type pulse = Low | High

type module_ = Broadcaster of string list
             | FlipFlop of bool * string list
             | Conjunction of (string * pulse) list * string list


let parse_module line =
  let parts = line |> String.split ~on:' ' in
  let name = parts
             |> List.hd_exn
             |> String.to_list
             |> Fn.flip List.drop 1
             |> String.of_list
  in
  let type_ = parts
              |> List.hd_exn
              |> String.to_list
              |> List.hd_exn
  in
  let outputs = parts
                |> Fn.flip List.drop 2
                |> List.map ~f:(String.strip ~drop:(Char.equal ','))
  in
  match type_ with
    'b' -> ("broadcaster", Broadcaster(outputs))
  | '%' -> (name, FlipFlop(false, outputs))
  | '&' -> (name, Conjunction([], outputs))
  | _ -> assert false

let init_conjunctions (modules: (string * module_) list) =
  let outputs m = match m with
                    Broadcaster(outputs) -> outputs
                  | FlipFlop(_, outputs) -> outputs
                  | Conjunction(_, outputs) -> outputs
  in
  let inputs conjunction = modules
                           |> List.filter ~f:(fun (_, m) -> List.mem (outputs m) conjunction ~equal:String.equal)
                           |> List.map ~f:(fun (name, _) -> name)
                           |> List.dedup_and_sort ~compare:String.compare
                           |> List.map ~f:(fun name -> (name, Low))
  in
  let map_conjunction name m = match m with
                                 Conjunction(_, outputs) -> Conjunction(inputs name, outputs)
                               | _ -> m
  in
  modules
  |> List.map ~f:(fun (name, m) -> (name, map_conjunction name m ))

let process_flipflop modules receiver pulse =
  match pulse with
    High -> None
  | Low -> let next_flipflop = Hashtbl.update_and_return modules receiver ~f:(fun f -> match f with
                                                                                         Some(FlipFlop(state, outputs)) -> FlipFlop(not state, outputs)
                                                                                       | _ -> assert false
                                                                             )
           in
           match next_flipflop with
             FlipFlop(state, _) -> if state then Some(High) else Some(Low)
           | _ -> assert false

let is_high p = match p with
                  High -> true
                | Low -> false

let process_conjunction modules sender receiver pulse =
  let update_state state = state |> List.map ~f:(fun (input, p) -> if String.equal input sender then (input, pulse) else (input, p)) in
  let next_conjunction = Hashtbl.update_and_return modules receiver ~f:(fun f -> match f with
                                                                                   Some(Conjunction(state, outputs)) -> Conjunction(update_state state, outputs)
                                                                                 | _ -> assert false
                                                                       )
  in
  match next_conjunction with
    Conjunction(state, _) -> if List.for_all state ~f:(fun (_, p) -> is_high p) then Some(Low) else Some(High)
  | _ -> assert false

exception RxTurnedOn

let process_pulse modules sender receiver pulse scream =
  match Hashtbl.find modules receiver with
    Some(Broadcaster(outputs)) -> outputs |> List.map ~f:(fun output -> Some(receiver, output, pulse))
  | Some(FlipFlop(_, outputs)) -> let next_pulse = process_flipflop modules receiver pulse in outputs |> List.map ~f:(fun output -> Option.map next_pulse ~f:(fun p -> (receiver, output, p)))
  | Some(Conjunction(_, outputs)) -> let next_pulse = process_conjunction modules sender receiver pulse in outputs |> List.map ~f:(fun output -> Option.map next_pulse ~f:(fun p -> (receiver, output, p)))
  | None -> if scream && String.equal receiver "rx" && not (is_high pulse) then raise RxTurnedOn else []

let rec send_pulses acc modules pulses scream =
  let (lows, highs) = acc in
  let new_lows = pulses
                 |> List.count ~f:(fun (_, _, p) -> not (is_high p))
  in
  let new_highs = pulses
                 |> List.count ~f:(fun (_, _, p) -> is_high p)
  in
  if List.is_empty pulses then
    acc
  else
    pulses
    |> List.map ~f:(fun (sender, receiver, pulse) -> process_pulse modules sender receiver pulse scream)
    |> List.concat
    |> List.filter_map ~f:(fun p -> p)
    |> (fun next_pulses -> send_pulses (lows + new_lows, highs + new_highs) modules next_pulses scream)

let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:parse_module
  |> init_conjunctions
  |> Hashtbl.of_alist_exn (module String)

let dump_graph puzzle filename =
  let nodes = Hashtbl.to_alist puzzle
              |> List.map ~f:(fun (name, m) -> match m with
                                                Broadcaster(_) -> Printf.sprintf "%s [shape=box];" name
                                              | FlipFlop(_, _) -> Printf.sprintf "%s [shape=ellipse, color=blue];" name
                                              | Conjunction(_, _) -> Printf.sprintf "%s [shape=ellipse, color=red];" name
                             )
              |> String.concat ~sep:"\n"
  in
  let edges = Hashtbl.to_alist puzzle
            |> List.map ~f:(fun (name, m) -> match m with
                                              Broadcaster(outputs) -> List.map outputs ~f:(fun output -> Printf.sprintf "%s -> %s;" name output)
                                            | FlipFlop(_, outputs) -> List.map outputs ~f:(fun output -> Printf.sprintf "%s -> %s;" name output)
                                            | Conjunction(_, outputs) -> List.map outputs ~f:(fun output -> Printf.sprintf "%s -> %s;" name output)
                           )
            |> List.concat
            |> String.concat ~sep:"\n"
  in
  Out_channel.write_all filename ~data:(String.concat ["digraph day20 {"; nodes; edges; "}"])
let part1 puzzle =
  List.range 0 1000
  |> List.map ~f:(fun _ -> send_pulses (0, 0) puzzle [("button", "broadcaster", Low)] false)
  |> List.fold ~init:(0, 0) ~f:(fun (lows, highs) (l, h) -> (lows + l, highs + h))  
  |> (fun (lows, highs) -> lows * highs)
  |> Int.to_string

let rec run_until_rx_on modules i =
  try
    let (_, _) = send_pulses (0, 0) modules [("button", "broadcaster", Low)] true in
    run_until_rx_on modules (i + 1)
  with
    RxTurnedOn -> i

let part2 puzzle =
  run_until_rx_on puzzle 1
  |> Int.to_string
