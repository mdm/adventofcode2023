open Base
open Stdio

open Day21

exception NoInput of string

let puzzle_args =
  if Array.length (Sys.get_argv ()) > 2 then
    ((Sys.get_argv ()).(1), (Sys.get_argv ()).(2))
  else
    raise (NoInput "Usage: day21 <input> <target>")

let puzzle_input =
  let (filename, _) = puzzle_args in
  let ch = In_channel.create ~binary:true filename in
  let contents = In_channel.input_all ch in
  contents


let () = 
  let (_, target) = puzzle_args in
  let puzzle = parse puzzle_input in
  part1 puzzle (Int.of_string target)
  |> print_endline;
  part2 puzzle
  |> print_endline;
