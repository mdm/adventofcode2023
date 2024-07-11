open Base
open Z3

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

let solve puzzle =
  let ctx = mk_context [] in
  let solver = Solver.mk_solver ctx None in
  let get_value model var = Model.eval model var true |> Option.value_exn |> Expr.to_string in
  let zero = Expr.mk_numeral_int ctx 0 (Arithmetic.Integer.mk_sort ctx) in
  let spx = Arithmetic.Integer.mk_const_s ctx "spx" in
  let spy = Arithmetic.Integer.mk_const_s ctx "spy" in
  let spz = Arithmetic.Integer.mk_const_s ctx "spz" in
  let svx = Arithmetic.Integer.mk_const_s ctx "svx" in
  let svy = Arithmetic.Integer.mk_const_s ctx "svy" in
  let svz = Arithmetic.Integer.mk_const_s ctx "svz" in
  let hailstone_constraint i h =
    let hpx = Expr.mk_numeral_int ctx h.position.x (Arithmetic.Integer.mk_sort ctx) in
    let hpy = Expr.mk_numeral_int ctx h.position.y (Arithmetic.Integer.mk_sort ctx) in
    let hpz = Expr.mk_numeral_int ctx h.position.z (Arithmetic.Integer.mk_sort ctx) in
    let hvx = Expr.mk_numeral_int ctx h.velocity.x (Arithmetic.Integer.mk_sort ctx) in
    let hvy = Expr.mk_numeral_int ctx h.velocity.y (Arithmetic.Integer.mk_sort ctx) in
    let hvz = Expr.mk_numeral_int ctx h.velocity.z (Arithmetic.Integer.mk_sort ctx) in
    let t = Arithmetic.Integer.mk_const_s ctx (Printf.sprintf "t_%d" i) in
    let t_ge_zero = Arithmetic.mk_ge ctx t zero in
    let eqx = Boolean.mk_eq ctx (Arithmetic.mk_add ctx [hpx; Arithmetic.mk_mul ctx [t; hvx]]) (Arithmetic.mk_add ctx [spx; Arithmetic.mk_mul ctx [t; svx]]) in
    let eqy = Boolean.mk_eq ctx (Arithmetic.mk_add ctx [hpy; Arithmetic.mk_mul ctx [t; hvy]]) (Arithmetic.mk_add ctx [spy; Arithmetic.mk_mul ctx [t; svy]]) in
    let eqz = Boolean.mk_eq ctx (Arithmetic.mk_add ctx [hpz; Arithmetic.mk_mul ctx [t; hvz]]) (Arithmetic.mk_add ctx [spz; Arithmetic.mk_mul ctx [t; svz]]) in
    Boolean.mk_and ctx [t_ge_zero; eqx; eqy; eqz]
  in
  puzzle
  |> List.mapi ~f:hailstone_constraint
  |> List.iter ~f:(fun c -> Solver.add solver [c]);
  match Solver.check solver [] with
  | UNSATISFIABLE -> assert false
  | UNKNOWN -> assert false
  | SATISFIABLE -> let model = Solver.get_model solver |> Option.value_exn in List.fold [spx; spy; spz] ~init:0 ~f:(fun acc var -> acc + (get_value model var |> Int.of_string))


let part2 puzzle =
  solve puzzle
  |> Int.to_string
