open Base

let parse_line line =
  line |> String.to_array
  
let parse input =
  let lines = String.split_lines input in
  lines
  |> List.map ~f:parse_line
  |> Array.of_list

let rec pairs list =
  match list with
    x::y::_ -> (x, y)::(pairs (List.tl_exn list))
  | _ -> []

let finished puzzle =
  let finished' x y tile = not (Char.equal tile 'O')
                       || Int.equal y 0
                       || not (Char.equal puzzle.(y - 1).(x) '.')
  in
  puzzle
  |> Array.for_alli ~f:(fun y row -> row |> Array.for_alli ~f:(fun x tile -> finished' x y tile))

let rec tilt_north puzzle =
  let tilt_north' x y tile puzzle = if Char.equal tile '.' && y + 1 < Array.length puzzle && Char.equal (puzzle.(y + 1).(x)) 'O' then
                               (puzzle.(y).(x) <- 'O';
                               puzzle.(y + 1).(x) <- '.';)
                             else
                               ()
  in
  if finished puzzle then
    puzzle
  else
    (puzzle    
    |> Array.iteri ~f:(fun y row -> row |> Array.iteri ~f:(fun x tile -> tilt_north' x y tile puzzle));
    tilt_north puzzle)

let total_load puzzle =
  let total_load' _x y tile = if Char.equal tile 'O' then Array.length puzzle - y else 0 in
  puzzle
  |> Array.foldi ~init:0 ~f:(fun y acc row ->acc + (row |> Array.foldi ~init:0 ~f:(fun x acc tile -> acc + (total_load' x y tile))))

let spin_cycle puzzle =
  puzzle
  |> tilt_north
  |> Array.transpose_exn
  |> tilt_north
  |> Array.transpose_exn
  |> Array.rev
  |> tilt_north
  |> Array.rev
  |> Array.map ~f:Array.rev
  |> Array.transpose_exn
  |> tilt_north
  |> Array.transpose_exn
  |> Array.map ~f:Array.rev

let find_loop puzzle =
  let history = Hashtbl.create (module String) in
  let rec find_loop' i puzzle =
    let joined = puzzle |> Array.to_list |> Array.concat |> String.of_array in
    match Hashtbl.find history joined with
      Some(last) -> (last, i - last)
    | _ ->
        Hashtbl.set history ~key:joined ~data:i;
        find_loop' (i + 1) (spin_cycle puzzle)
  in
  find_loop' 0 puzzle

let rec cycles puzzle n =
  if Int.equal n 0 then
    puzzle
  else
    cycles (spin_cycle puzzle) (n - 1)

let part1 puzzle =
  puzzle
  |> tilt_north
  |> total_load
  |> Int.to_string

let part2 puzzle =
  puzzle
  |> Array.copy_matrix
  |> find_loop
  |> (fun (start, len) -> cycles puzzle (start + (1_000_000_000 - start) % len))
  |> total_load
  |> Int.to_string
