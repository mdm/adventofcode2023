open Base

type hand =
  { cards: char list;
    bid: int;
  }

let parse input =
  let lines = String.split_lines input in
  let parse_line line = match String.split line ~on:' ' with
                          cards::bid::[] -> (String.to_list cards, Int.of_string bid)
                        | _ -> assert false
  in
  lines
  |> List.map ~f:parse_line
  |> List.map ~f:(fun (cards, bid) -> { cards; bid; })

let high_card _runs =
  true

let one_pair runs =
  runs
  |> List.exists ~f:(fun run -> run |> List.length |> Int.equal 2)
  
let two_pair runs =
  runs
  |> List.count ~f:(fun run -> run |> List.length |> Int.equal 2)
  |> Int.equal 2
  
let three_of_a_kind runs =
  runs
  |> List.exists ~f:(fun run -> run |> List.length |> Int.equal 3)

let full_house runs =
  one_pair runs && three_of_a_kind runs

let four_of_a_kind runs =
  runs
  |> List.exists ~f:(fun run -> run |> List.length |> Int.equal 4)

let five_of_a_kind runs =
  runs
  |> List.exists ~f:(fun run -> run |> List.length |> Int.equal 5)
  
let hand_type cards =
  let types = [five_of_a_kind; four_of_a_kind; full_house; three_of_a_kind; two_pair; one_pair; high_card;] in
  let runs = List.sort_and_group cards ~compare:Char.compare in
  types
  |> List.findi ~f:(fun _i f -> f runs)
  |> Option.value_exn
  |> fst

let hand_strength order cards =
  let order_len = List.length order in
  let strength card = List.findi order ~f:(fun _i c -> Char.equal c card) |> Option.value_exn |> fst in
  cards
  |> List.fold ~init:0 ~f:(fun acc card -> order_len * acc + (strength card))
  
let hand_compare a b =
  let order = ['2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A';] in
  let a_type = hand_type a in
  let b_type = hand_type b in
  if Int.equal a_type b_type then
    (hand_strength order a) - (hand_strength order b)
  else
    b_type - a_type
                              
let part1 puzzle =
  puzzle
  |> List.sort ~compare:(fun a b -> hand_compare a.cards b.cards)
  |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + (i + 1) * hand.bid)
  |> Int.to_string

let best_hand_type cards =
  let non_jokers = ['2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A';] in
  let with_joker joker = List.map cards ~f:(fun c -> if Char.equal c 'J' then joker else c) |> hand_type in
  non_jokers
  |> List.fold ~init:Int.max_value ~f:(fun acc joker -> with_joker joker |> Int.min acc)

let hand_compare_with_jokers a b =
  let order = ['J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A';] in
  let a_type = best_hand_type a in
  let b_type = best_hand_type b in
  if Int.equal a_type b_type then
    (hand_strength order a) - (hand_strength order b)
  else
    b_type - a_type
  
let part2 puzzle =
  puzzle
  |> List.sort ~compare:(fun a b -> hand_compare_with_jokers a.cards b.cards)
  |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + (i + 1) * hand.bid)
  |> Int.to_string
