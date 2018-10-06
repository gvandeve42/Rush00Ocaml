module Piece :
sig

  type t = O | X | E

end

type t

val newGrid : unit -> t
val toStringList : t -> (string * string * string) list
val dropPiece : int -> int -> Piece.t -> t -> t
