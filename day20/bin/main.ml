open Base
open Stdio

open Day20

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
  part1 (parse puzzle_input)
  |> print_endline;
  dump_graph (parse puzzle_input) (String.concat [puzzle_filename; ".dot"]);
  part2 (parse puzzle_input)
  |> print_endline;
