open Base

type card =
  { id: int;
    winning_numbers: int list;
    my_numbers: int list;
  } [@@deriving show]

let numbers_of_string ns =
  String.strip ns
  |> String.split ~on:' '
  |> List.filter ~f:(fun n -> not (String.is_empty n))
  |> List.map ~f:Int.of_string

let parse_card card =
  let (card_info, numbers) = match String.split card ~on:':' with
    card_info::numbers::_ -> (card_info, numbers)
  | _ -> assert false
  in
  let id = String.split card_info ~on:' ' |> List.last_exn |> Int.of_string in
  let (winning_numbers_raw, my_numbers_raw) = match String.split numbers ~on:'|' with
    winning_numbers_raw::my_numbers_raw::_ -> (winning_numbers_raw, my_numbers_raw)
  | _ -> assert false
  in
  let winning_numbers = numbers_of_string winning_numbers_raw in
  let my_numbers = numbers_of_string my_numbers_raw in
  { id; winning_numbers; my_numbers; }


let parse input =
  let lines = String.split_lines input in
  List.map lines ~f:parse_card

let count_winning_numbers card =
  List.count card.winning_numbers ~f:(fun n -> List.mem card.my_numbers n ~equal:Int.equal)
  
let list_winning_numbers card =
  List.filter card.winning_numbers ~f:(fun n -> List.mem card.my_numbers n ~equal:Int.equal)
    
let part1 puzzle =
  List.map puzzle ~f:count_winning_numbers
  |> List.map ~f:(fun n -> if Int.equal n 0 then 0 else 2 **  (n - 1))
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string

let count_cards cards card =
  let memo = Array.create ~len:(Array.length cards) None in
  let rec count_cards_inner cards card =
    match memo.(card.id - 1) with
      Some(result) -> result
    | None ->
      let winning = count_winning_numbers card in
      let copies = List.range (card.id + 1) (card.id + winning + 1) in
      let result = List.fold copies ~init:1 ~f:(fun acc c -> acc + (count_cards_inner cards cards.(c - 1))) in
      memo.(card.id - 1) <- Some(result);
      result
  in
  count_cards_inner cards card


let part2 puzzle =
  let cards = Array.of_list puzzle in
  List.map puzzle ~f:(count_cards cards)
  |> List.fold ~init:0 ~f:(+)
  |> Int.to_string
