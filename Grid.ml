module Piece =
  struct 

    type t = O | X | E

  end

type t = {
    pieces 	: Piece.t list list;
    winner 	: Piece.t;
    is_winner	: bool;
  }

let newGrid () = {
    pieces 	= [[Piece.E; Piece.E; Piece.E];
                   [Piece.E; Piece.E; Piece.E];
                   [Piece.E; Piece.E; Piece.E]];

    winner 	= Piece.E;

    is_winner 	= false
  }

let piece_to_str p =
  match p with
  | Piece.O -> "O"
  | Piece.X -> "X"
  | Piece.E -> "E"

let plist_to_tuple plst =
  match plst with
  | p1::p2::p3::[] ->
     (piece_to_str p1, piece_to_str p2, piece_to_str p3)
  | _ -> ("Error", "Error", "Error")

let toStringList t = List.map plist_to_tuple t.pieces
            
let rec isWinnerLine pieces =
  match pieces with
  | [] -> Piece.E
  | [Piece.X; Piece.X; Piece.X]::tail -> Piece.X
  | [Piece.O; Piece.O; Piece.O]::tail -> Piece.O
  | head::tail -> isWinnerLine tail

let rec isWinnerColumn pieces =
  match pieces with
  | lst1::lst2::lst3::[] ->
     let rec scan_column lst1 lst2 lst3 =
       match lst1, lst2, lst3 with
       |[], [], [] -> Piece.E
       |Piece.X::n1, Piece.X::n2, Piece.X::n3 -> Piece.X
       |Piece.O::n1, Piece.O::n2, Piece.O::n3 -> Piece.O
       |_::tail1, _::tail2, _::tail3 -> scan_column tail1 tail2 tail3
       |_, _, _ -> Piece.E
     in scan_column lst1 lst2 lst3
  |_ -> Piece.E

let rec isWinnerDiag pieces =
  match pieces with
  | lst1::lst2::lst3::[] ->
     let rec scan_diag lst1 lst2 lst3 =
       match lst1, lst2, lst3 with
       |Piece.X::_::_::[],
        _::Piece.X::_::[],
        _::_::Piece.X::[] -> Piece.X
       |Piece.O::_::_::[],
        _::Piece.O::_::[],
        _::_::Piece.O::[] -> Piece.O
       |_::_::Piece.X::[],
        _::Piece.X::_::[],
        Piece.X::_::_::[] -> Piece.X
       |_::_::Piece.O::[],
        _::Piece.O::_::[],
        Piece.O::_::_::[] -> Piece.O
       |_ -> Piece.E
     in scan_diag lst1 lst2 lst3
  |_ -> Piece.E

let isWinner pieces =
  if (isWinnerLine pieces) <> Piece.E ||
       (isWinnerColumn pieces) <> Piece.E ||
         (isWinnerDiag pieces) <> Piece.E
  then true
  else false

let getWinner pieces =
  if (isWinnerLine pieces) <> Piece.E then (isWinnerLine pieces)
  else if (isWinnerColumn pieces) <> Piece.E then (isWinnerColumn pieces)
  else if (isWinnerDiag pieces) <> Piece.E then (isWinnerDiag pieces)
  else Piece.E

let dropPiece x y piece t =
  if x < 0 || x > 2 || y < 0 || y > 2 then raise ( invalid_arg "Please respect 0<=x<=2, 0<=y<=2" )
  else let rec alter_piece x lst =
         match x, lst with
         | _, [] -> []
         | 0, head::tail ->
            if head == Piece.E then
              piece::(alter_piece (x - 1) tail)
            else
              ( invalid_arg "Please respect 0<=x<=2, 0<=y<=2" )
         | _, head::tail -> head::(alter_piece (x - 1) tail) in
       let rec alter_line x y lst =
         match y, lst with
         | _, [] -> []
         | 0, head::tail -> (alter_piece x head)::(alter_line x (y - 1) tail)
         | _, head::tail -> head::(alter_line x (y - 1) tail)
       in let new_grid = alter_line x y t.pieces in
          { pieces = new_grid; winner = (getWinner new_grid); is_winner = (isWinner new_grid) }

let test_grid_setup () =
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
