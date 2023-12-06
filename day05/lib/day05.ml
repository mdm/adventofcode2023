open Base

type range =
  { dest_start: int;
    src_start: int;
    len: int;
  }

type map =
  { src: string;
    dest: string;
    ranges: range list;
  }

let rec split_blocks block blocks lines =
  match lines with
    [] -> List.rev ((List.rev block)::blocks)
  | hd::tl -> if String.is_empty hd then
      split_blocks [] ((List.rev block)::blocks) tl
    else
      split_blocks (hd::block) blocks tl

let parse_numbers str =
  str
  |> String.split ~on:' '
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:Int.of_string

let parse_range str =
  match parse_numbers str with
    dest_start::src_start::len::[] ->{ dest_start; src_start; len; }
  | _ -> assert false

let parse_map block =
  let info = block
             |> List.hd_exn
             |> String.split ~on:' '
             |> List.hd_exn
             |> String.split ~on:'-'
  in
  let src = List.hd_exn info in
  let dest = List.last_exn info in
  let ranges = block
               |> List.tl_exn
               |> List.map ~f:parse_range
  in
  { src; dest; ranges; }


let parse input =
  let lines = String.split_lines input in
  let blocks = split_blocks [] [] lines in
  let seeds = blocks
              |> List.hd_exn
              |> List.hd_exn
              |> String.split ~on:':'
              |> List.last_exn
              |> parse_numbers
  in
  let maps = blocks
             |> List.tl_exn
             |> List.map ~f:parse_map
  in
  (seeds, maps)

let in_range seed range =
  seed >= range.src_start && seed < range.src_start + range.len

let map_seed seed map =
  match List.find map.ranges ~f:(in_range seed) with
    Some(range) -> range.dest_start + (seed - range.src_start)
  | None -> seed

let seed_location maps seed =
  List.fold maps ~init:seed ~f:map_seed

let part1 puzzle =
  let (seeds, maps) = puzzle in
  seeds
  |> List.map ~f:(seed_location maps)
  |> List.reduce ~f:Int.min
  |> Option.value_exn
  |> Int.to_string

let seed_ranges seeds =
  let groups = List.groupi seeds ~break:(fun i _ _ -> Int.equal (i % 2) 0) in
  let group_to_range group = match group with
    start::len::[] -> { dest_start = start; src_start = start; len; }
  | _ -> assert false
  in
  List.map groups ~f:group_to_range

let fully_contained seed_range map_range =
  seed_range.src_start >= map_range.src_start && seed_range.src_start + seed_range.len <= map_range.src_start + map_range.len

let overlapping seed_range map_range =
  seed_range.src_start < map_range.src_start + map_range.len && seed_range.src_start + seed_range.len > map_range.src_start

let split_range seed_range map_range =
  let left = if seed_range.src_start < map_range.src_start then
      [{ dest_start = seed_range.src_start;
         src_start = seed_range.src_start;
         len = map_range.src_start - seed_range.src_start;
      }]
    else [] in
  let inner = 
    let start = Int.max seed_range.src_start map_range.src_start in
    let end_ = Int.min (seed_range.src_start + seed_range.len) (map_range.src_start + map_range.len) in
    [{ dest_start = start;
        src_start = start;
        len = end_ - start;
    }]
  in
  let right = if seed_range.src_start + seed_range.len > map_range.src_start + map_range.len then
      [{ dest_start = map_range.src_start + map_range.len;
         src_start = map_range.src_start + map_range.len;
         len = (seed_range.src_start + seed_range.len) - (map_range.src_start + map_range.len);
      }]
    else [] in
  List.concat [left; inner; right;]

let first_overlap seed_range map =
  map.ranges
  |> List.find ~f:(overlapping seed_range)
  |> Option.map ~f:(split_range seed_range)

let rec map_seed_range map seed_range =
  let mapped_range map_range = [{ seed_range with
      dest_start = map_range.dest_start + (seed_range.src_start - map_range.src_start);
      src_start = map_range.dest_start + (seed_range.src_start - map_range.src_start);
    }] in
  match List.find map.ranges ~f:(fully_contained seed_range) with
    Some(map_range) -> mapped_range map_range
  | None -> match first_overlap seed_range map with
              Some(seed_ranges) -> map_seed_ranges seed_ranges map
            | None -> [seed_range]
and map_seed_ranges seed_ranges map =
  seed_ranges
  |> List.map ~f:(map_seed_range map)
  |> List.concat

let seed_range_to_location_ranges maps seed_range =
  List.fold maps ~init:[seed_range] ~f:map_seed_ranges

let min_location ranges =
  ranges
  |> List.map ~f:(fun r -> r.src_start)
  |> List.reduce ~f:Int.min
  |> Option.value_exn

let part2 puzzle =
  let (seeds, maps) = puzzle in
  seeds
  |> seed_ranges
  |> List.map ~f:(seed_range_to_location_ranges maps)
  |> List.map ~f:min_location
  |> List.reduce ~f:Int.min
  |> Option.value_exn
  |> Int.to_string
