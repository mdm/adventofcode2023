open Base

type vec3 = {
  x: int;
  y: int;
  z: int;
}

type hailstone = {
  position: vec3;
  velocity: vec3;
}

let parse_vec3 str =
  let parts = String.split ~on:',' str in
  {x = Int.of_string (List.nth_exn parts 0 |> String.strip);
   y = Int.of_string (List.nth_exn parts 1 |> String.strip);
   z = Int.of_string (List.nth_exn parts 2 |> String.strip)}

let parse_line line =
  let parts = line
              |> String.split ~on:'@'
              |> List.map ~f:parse_vec3
  in
  {position = List.nth_exn parts 0; velocity = List.nth_exn parts 1}

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:parse_line

let intersection_2d h1 h2 =
  let p_x = h2.position.x - h1.position.x in
  let p_y = h2.position.y - h1.position.y in
  let scale value = (Float.of_int value) *. ((Float.of_int h2.velocity.x) /. (Float.of_int (-h2.velocity.y))) in
  let t = ((Float.of_int p_x) +. (scale p_y)) /. ((Float.of_int h1.velocity.x) +. (scale h1.velocity.y)) in
  let u = (t *. (Float.of_int h1.velocity.x) -. (Float.of_int p_x)) /. (Float.of_int h2.velocity.x) in
  if Float.(<) t 0.0 || Float.(<) u 0.0 then
    []
  else  
    let x = (Float.of_int h1.position.x) +. t *. (Float.of_int h1.velocity.x) in
    let y = (Float.of_int h1.position.y) +. t *. (Float.of_int h1.velocity.y) in
    [(x, y)]

let part1 puzzle =
  let test_area_min = 200000000000000.0 in
  let test_area_max = 400000000000000.0 in
  List.mapi puzzle ~f:(fun i h1 -> List.mapi puzzle ~f:(fun j h2 -> if i < j then intersection_2d h1 h2 else []) |> List.concat)
  |> List.concat
  |> List.filter ~f:(fun (x, y) -> Float.(>=) x test_area_min && Float.(<=) x test_area_max && Float.(>=) y test_area_min && Float.(<=) y test_area_max)
  |> List.length
  |> Int.to_string

let part2 _puzzle =
  0
  |> Int.to_string
