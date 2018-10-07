
module Piece =
  struct 

    type t = O | X | E

  end

type t = {
    pieces 	: Piece.t list list;
    winner 	: Piece.t;
  }

let newGrid () = {
    pieces 	= [[Piece.E; Piece.E; Piece.E];
              [Piece.E; Piece.E; Piece.E];
              [Piece.E; Piece.E; Piece.E]];

    winner 	= Piece.E;
  }

let piece_to_str p =
  match p with
  | Piece.O -> "O"
  | Piece.X -> "X"
  | Piece.E -> "-"

let plist_to_lstring plst =
  match plst with
  | p1::p2::p3::en ->
     [" "^ (piece_to_str p1) ^" "^ (piece_to_str p2) ^" "^ (piece_to_str p3)]
  | _ -> ["Error"]

let toStringList t = (List.map plist_to_lstring t.pieces)

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

let spaceline line =
    let rec loop line = match line with
      | []      -> false
      | hd::tl  -> if hd = Piece.E then true else loop tl 
    in loop line


let space grid =
  let rec loop grid = match grid with
      | []      -> false
      | hd::tl  -> if (spaceline hd) = true then true else loop tl
    in loop grid.pieces

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

let get_value_winner t = t.winner

let getWinner pieces =
  if (isWinnerLine pieces) <> Piece.E then (isWinnerLine pieces)
  else if (isWinnerColumn pieces) <> Piece.E then (isWinnerColumn pieces)
  else if (isWinnerDiag pieces) <> Piece.E then (isWinnerDiag pieces)
  else Piece.E

let change_column_at column x (piece: Piece.t ) = 
  let rec loop_column icolumn column new_column valid = match column, icolumn with
      | [], _                            -> (new_column,valid)
      | hd::tl, icolumn when icolumn = x -> if (hd = Piece.E) then loop_column (icolumn+1) tl (new_column @ [piece]) true else loop_column (icolumn+1) tl (new_column @ [hd]) false
      | hd::tl, _                        -> loop_column (icolumn+1) tl (new_column @ [hd]) valid
  in loop_column 1 column [] true

let dropPiece y x (piece: Piece.t ) t =  if t.winner <> Piece.E then (t, false) else
 let rec loop_row iline grid newgrid valid = match grid, iline with
    | [], _                        -> let newt = { pieces = newgrid; winner = (getWinner newgrid) } in (newt, valid)
    | hd::tl, iline when iline = y -> let (new_column, v) = (change_column_at hd x piece) in loop_row (iline+1) tl (newgrid @ [new_column]) v
    | hd::tl, _                    -> loop_row (iline+1) tl (newgrid @ [hd]) valid
  in loop_row 1 t.pieces [] true
