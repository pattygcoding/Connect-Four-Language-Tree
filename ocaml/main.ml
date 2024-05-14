(* Connect Four game in OCaml *)

type player = Red | Yellow

let string_of_player = function
  | Red -> "R"
  | Yellow -> "Y"

let empty = "."

let rows = 6
let cols = 7

let create_board () = Array.make_matrix rows cols empty

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      print_string cell;
      print_string " ";
    ) row;
    print_newline ()
  ) board;
  for i = 1 to cols do
    Printf.printf " %d" i
  done;
  print_newline ()

let drop_piece board col player =
  let rec find_row r =
    if r >= rows then
      None
    else if board.(r).(col) = empty then
      Some r
    else
      find_row (r + 1)
  in
  match find_row 0 with
  | None -> false
  | Some row ->
      board.(row).(col) <- string_of_player player;
      true

let check_winner board player =
  let check_line dx dy =
    let rec check_count x y count =
      if x < 0 || x >= rows || y < 0 || y >= cols || board.(x).(y) <> string_of_player player then
        count
      else
        check_count (x + dx) (y + dy) (count + 1)
    in
    check_count
  in
  let directions = [ (1, 0); (0, 1); (1, 1); (1, -1) ] in
  let check_all_directions x y =
    List.exists (fun (dx, dy) ->
      let count1 = check_line dx dy x y 0 in
      let count2 = check_line (-dx) (-dy) x y 0 in
      count1 + count2 - 1 >= 4
    ) directions
  in
  let array_existsi f arr =
    let rec existsi i =
      if i >= Array.length arr then false
      else f i arr.(i) || existsi (i + 1)
    in
    existsi 0
  in
  array_existsi (fun r row ->
    array_existsi (fun c cell ->
      cell = string_of_player player && check_all_directions r c
    ) row
  ) board

let rec play_game board player =
  print_board board;
  Printf.printf "Player %s, choose a column (1-7): " (string_of_player player);
  let col = read_int () - 1 in
  if col < 0 || col >= cols then
    (print_endline "Invalid column. Try again."; play_game board player)
  else if not (drop_piece board col player) then
    (print_endline "Column is full. Try again."; play_game board player)
  else if check_winner board player then
    (print_board board; Printf.printf "Player %s wins!\n" (string_of_player player))
  else
    play_game board (if player = Red then Yellow else Red)

let () =
  let board = create_board () in
  play_game board Red
