
module Piece =
  struct 

    type t = O | X | E

  end

type t = Piece.t list list


let newGrid = [
    [Piece.E; Piece.E; Piece.E];
    [Piece.E; Piece.E; Piece.E];
    [Piece.E; Piece.E; Piece.E]
  ]

let rec isWinnerLine t =
  match t with
  | [] -> false
  | [Piece.X; Piece.X; Piece.X]::tail -> true
  | [Piece.O; Piece.O; Piece.O]::tail -> true
  | head::tail -> isWinnerLine tail

let rec isWinnerColumn t =
  match t with
  | lst1::lst2::lst3::[] ->
     let rec scan_column lst1 lst2 lst3 =
       match lst1, lst2, lst3 with
       |[], [], [] -> false
       |Piece.X::n1, Piece.X::n2, Piece.X::n3 -> true
       |Piece.O::n1, Piece.O::n2, Piece.O::n3 -> true
       |_::tail1, _::tail2, _::tail3 -> scan_column tail1 tail2 tail3
       |_, _, _ -> false
     in scan_column lst1 lst2 lst3
  |_ -> false

let rec isWinnerDiag t =
  match t with
  | lst1::lst2::lst3::[] ->
     let rec scan_diag lst1 lst2 lst3 =
       match lst1, lst2, lst3 with
       |Piece.X::_::_::[],
        _::Piece.X::_::[],
        _::_::Piece.X::[] -> true
       |Piece.O::_::_::[],
        _::Piece.O::_::[],
        _::_::Piece.O::[] -> true
       |_::_::Piece.X::[],
        _::Piece.X::_::[],
        Piece.X::_::_::[] -> true
       |_::_::Piece.O::[],
        _::Piece.O::_::[],
        Piece.O::_::_::[] -> true
       |_ -> false
     in scan_diag lst1 lst2 lst3
  |_ -> false

let isWinner t =
  if isWinnerLine t || isWinnerColumn t || isWinnerDiag t
  then true
  else false

let test_grid_setup =
  let test_grid_empty = [
      [Piece.E; Piece.E; Piece.E];
      [Piece.E; Piece.E; Piece.E];
      [Piece.E; Piece.E; Piece.E]
    ] in

  let test_grid_no_winner = [
      [Piece.X; Piece.E; Piece.X];
      [Piece.E; Piece.E; Piece.E];
      [Piece.E; Piece.X; Piece.E]
    ] in

  let test_grid_1st_line = [
      [Piece.X; Piece.X; Piece.X];
      [Piece.E; Piece.E; Piece.E];
      [Piece.E; Piece.E; Piece.E]
    ] in

  let test_grid_3rd_line = [
      [Piece.E; Piece.E; Piece.E];
      [Piece.E; Piece.E; Piece.E];
      [Piece.O; Piece.O; Piece.O]
    ] in

  let test_grid_1st_column = [
      [Piece.O; Piece.E; Piece.E];
      [Piece.O; Piece.E; Piece.E];
      [Piece.O; Piece.E; Piece.E]
    ] in

  let test_grid_2nd_column = [
      [Piece.E; Piece.X; Piece.E];
      [Piece.E; Piece.X; Piece.E];
      [Piece.E; Piece.X; Piece.E]
    ] in

  let test_grid_1st_diag = [
      [Piece.X; Piece.E; Piece.E];
      [Piece.E; Piece.X; Piece.E];
      [Piece.E; Piece.E; Piece.X]
    ] in

  let test_grid_2nd_diag = [
      [Piece.E; Piece.E; Piece.O];
      [Piece.E; Piece.O; Piece.E];
      [Piece.O; Piece.E; Piece.E]
    ] in

  if isWinner test_grid_empty == true then print_endline "true" else print_endline "false";
  if isWinner test_grid_no_winner == true then print_endline "true" else print_endline "false";
  if isWinner test_grid_1st_line == true then print_endline "true" else print_endline "false";
  if isWinner test_grid_3rd_line == true then print_endline "true" else print_endline "false";
  if isWinner test_grid_1st_column == true then print_endline "true" else print_endline "false";
  if isWinner test_grid_2nd_column == true then print_endline "true" else print_endline "false";
  if isWinner test_grid_1st_diag == true then print_endline "true" else print_endline "false";
  if isWinner test_grid_2nd_diag == true then print_endline "true" else print_endline "false";
