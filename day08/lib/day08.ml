open Base

type destinations =
  { left: int;
    right: int;
  }

let to_int s = s
               |> String.to_list
               |> List.fold ~init:0 ~f:(fun acc c -> 26 * acc + (Char.to_int c - Char.to_int 'A'))

let parse input =
  let lines = String.split_lines input in
  let instructions = lines
                     |> List.hd_exn
                     |> String.to_list
  in
  let lines = List.drop lines 2 in
  let split_line line = line
                            |> String.split ~on:'='
                            |> List.map ~f:String.strip
  in
  let source parts = parts
                     |> List.hd_exn
                     |> to_int
  in
  let destinations parts = parts
                           |> Fn.flip List.nth_exn 1
                           |> String.strip ~drop:(fun c -> Char.equal c '(' || Char.equal c ')')
                           |> String.split ~on:','
                           |> List.map ~f:String.strip
                           |> (fun ds -> {left = List.nth_exn ds 0 |> to_int; right = List.nth_exn ds 1 |> to_int;})
  in
  let alist = lines
              |> List.map ~f:split_line
              |> List.map ~f:(fun parts -> (source parts, destinations parts))
  in
  let map = match Hashtbl.of_alist (module Int) alist with
    `Ok map -> map
  | _ -> assert false
  in
  (instructions, map)

let rec step i len location instructions map =
  let instruction = instructions.(i % len) in
  let destinations = Hashtbl.find_exn map location in
  if Int.equal location (to_int "ZZZ") then i else match instruction with
      'L' -> step (i + 1) len destinations.left instructions map
    | 'R' -> step (i + 1) len destinations.right instructions map
    | _ -> assert false

let part1 puzzle =
  let (instructions, map) = puzzle in
  let len = List.length instructions in
  let instructions = Array.of_list instructions in  
  step 0 len (to_int "AAA") instructions map
  |> Int.to_string

let rec step_all i len locations instructions map =
  let instruction = instructions.(i % len) in
  let destinations location = Hashtbl.find_exn map location in
  let finished = List.for_all locations ~f:(fun location -> Int.equal (location % 26) 25) in
  let lefts locations = List.map locations ~f:(fun location -> (destinations location).left) in
  let rights locations = List.map locations ~f:(fun location -> (destinations location).right) in
  Stdio.print_endline (Int.to_string i);
  if finished then i else match instruction with
      'L' -> step_all (i + 1) len (lefts locations) instructions map
    | 'R' -> step_all (i + 1) len (rights locations) instructions map
    | _ -> assert false

let find_loop i len location instructions map =
  let history = Hashtbl.create (module Int) in
  let rec find_loop_inner i len location instructions map =
    let instruction = instructions.(i % len) in
    let destinations = Hashtbl.find_exn map location in
    match Hashtbl.find history location with
      Some(last) when Int.equal (location % 26) 25 -> (last, i - last)
    | _ ->
        Hashtbl.set history ~key:location ~data:i;
        match instruction with
            'L' -> find_loop_inner (i + 1) len destinations.left instructions map
          | 'R' -> find_loop_inner (i + 1) len destinations.right instructions map
          | _ -> assert false
    in
    find_loop_inner i len location instructions map

let rec gcd u v =
  if v <> 0 then (gcd v (Int.rem u v))
  else (abs u)

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let part2 puzzle =
  let (instructions, map) = puzzle in
  let len = List.length instructions in
  let instructions = Array.of_list instructions in
  let locations = Hashtbl.keys map |> List.filter ~f:(fun location -> Int.equal (location % 26) 0) in
  locations
  |> List.map ~f:(fun location -> find_loop 0 len location instructions map)
  |> List.fold ~init:1 ~f:(fun acc loop -> lcm acc (snd loop))
  |> Int.to_string
