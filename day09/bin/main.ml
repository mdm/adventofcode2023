open Base
open Stdio

open Day09

exception NoInput of string

let puzzle_filename =
  if Array.length (Sys.get_argv ()) > 1 then
    (Sys.get_argv ()).(1)
  else
    raise (NoInput "Expecting input file as first command-line argument")

let puzzle_input =
  let filename = puzzle_filename in
  let ch = In_channel.create ~binary:true filename in
  let contents = In_channel.input_all ch in
  contents


let () = 
  let puzzle = parse puzzle_input in
  part1 puzzle
  |> print_endline;
  part2 puzzle
  |> print_endline;
