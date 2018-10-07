 
type 'a t = Group of ('a t) list * Grid.Piece.t * int | Solo of 'a

let ft_power x y =
	if y = 0 then 1
	else let rec loop v r =
			if r > 1 then
				loop (v * x) (r - 1)
			else v
		in loop x y

let get_w elem = match elem with | Solo elem -> Grid.get_value_winner elem | Group (_, winner, _) -> winner

let rec isWinnerLine lgroup =  match lgroup with
	| [] -> Grid.Piece.E
	| v1::v2::v3::tail when (get_w v1) = (get_w v2) && (get_w v2) = (get_w v1) -> get_w v1
	| head::tail -> isWinnerLine tail

let rec isWinnerColumn lgroup = match lgroup with
		| col11::col12::col13::col21::col22::col23::col31::col32::col33::[] ->
		if (get_w col11) = (get_w col12) && (get_w col12) = (get_w col13) then (get_w col11)
		else if (get_w col21) = (get_w col22) && (get_w col22) = (get_w col23) then (get_w col21)
		else if (get_w col31) = (get_w col32) && (get_w col32) = (get_w col33) then (get_w col31)
		else Grid.Piece.E
		| _ -> Grid.Piece.E

let rec isWinnerDiag lgroup = match lgroup with
	| col11::_::col12::_::col2::_::col31::_::col32::[] ->
		if (get_w col11) = (get_w col2) && (get_w col2) = (get_w col32) then (get_w col2)
		else if (get_w col12) = (get_w col2) && (get_w col2) = (get_w col31) then (get_w col2)
		else Grid.Piece.E
	| _ -> Grid.Piece.E

let rec getWinner lgroup = 
	if (isWinnerLine lgroup) <> Grid.Piece.E then (isWinnerLine lgroup)
	else if (isWinnerColumn lgroup) <> Grid.Piece.E then (isWinnerColumn lgroup)
	else if (isWinnerDiag lgroup) <> Grid.Piece.E then (isWinnerDiag lgroup)
	else Grid.Piece.E

let rec print_line line = match line with
	| [] -> ()
	| hd::tl -> print_string hd; if tl != [] then print_string " | " ; print_line tl

let rec print_bottom depth = match depth with
	| 0 -> print_endline ""
	| _ -> print_string "________" ; print_bottom (depth-1) 

let rec print_board lst =
	let rec loop lst i = match lst with
		| [] -> ()
		| hd::tl -> print_line hd; print_endline ""; if i = 2 && tl != []then begin print_bottom (List.length hd); loop tl (0) end else loop tl (i+1)
	in loop lst 0

let rec new_board depth = match depth with
	| 0 -> let grid = Grid.newGrid () in Solo grid
	| _ -> 	let newListGroup =  let rec gen_grid_t dep i lst = match i with 
									| 0 -> lst
									| _ -> gen_grid_t dep (i - 1) ((new_board dep) :: lst )
								in gen_grid_t (depth -1) 9 []
			in Group ( newListGroup, Grid.Piece.E, depth )


let add_to mainlst elems start = 
	let rec loop_toadd mstart estart mainlst elems newlst = match mainlst, elems with
		| [], _ -> newlst
		| _, [] -> newlst
		| mhd::mtl, ehd::etl -> if mstart = estart then loop_toadd (mstart+1) (estart+1) mtl etl ( newlst @ [mhd @ ehd] )
								else loop_toadd (mstart+1) (estart) mtl elems ( newlst @ [mhd] )
	in loop_toadd 0 start mainlst elems []


let merge_lst mainlst newlst depth i = match i with
	| 1 -> newlst
	| i when i = 4 || i = 7 -> mainlst @ newlst
	| i when i = 2 || i = 3 -> add_to mainlst newlst 0
	| i when i = 5 || i = 6 -> add_to mainlst newlst (ft_power 3 depth)
	| _ -> add_to mainlst newlst ((ft_power 3 depth) * 2)

(* add generation of winner of depth n *)
let rec get_list_str board = match board with
	| Solo grid -> (Grid.toStringList grid)
	| Group (listg, _, depth) -> let newListStirng = let rec gen_lgrid listg i lst = match listg with 
														| [] -> lst
														| hd::tl -> gen_lgrid tl (i + 1) (merge_lst lst (get_list_str hd) depth i)
													in gen_lgrid listg 1 []
								in newListStirng

let in_range (y, x) (gy, gx) depth = if y > gy && y <= ((ft_power 3 depth) + gy) && x > gx && x <= ((ft_power 3 depth) + gx) then true else false

let rec add_move y x piece board = match board with
		| Solo (grid)			-> let (gr, v) = Grid.dropPiece y x piece grid in (Solo (gr), v)
		| Group (listg, winner, de)	-> 
		if 
			winner <> Grid.Piece.E then (board, false)
		else
			let (newListGroup, valid) = 
				let rec loop_group listg y_x gy gx i newlst valid = match listg, y_x , i with
							| [], _, _																		-> (newlst, valid)
							| hd::tl , (y, x), i when (in_range (y, x) (gy , gx) de) && (i = 3 || i = 6)	-> let (nl, v) = (add_move (y-gy) (x-gx) piece hd) in loop_group tl (y,x) (gy + ft_power 3 de) (0) (i+1) (newlst @ [nl]) v
							| hd::tl , (y, x), _ when (in_range (y, x) (gy , gx) de)						-> let (nl, v) = (add_move (y-gy) (x-gx) piece hd) in loop_group tl (y,x) gy (gx + ft_power 3 de) (i+1) (newlst @ [nl]) v
							| hd::tl , _, i when (i = 3 || i = 6)											-> loop_group tl (y,x) (gy + ft_power 3 de) (0) (i+1) (newlst @ [hd]) valid
							| hd::tl ,y_x, _																-> loop_group tl (y,x) (gy) (gx + ft_power 3 de) (i+1) (newlst @ [hd]) valid
				in loop_group listg (y,x) 0 0 1 [] true
			in (Group (newListGroup, getWinner newListGroup, de), valid)

let pritn_bool b = if b = true then print_endline "true" else print_endline "false"

let test_board () = 
	let depth = 1 in 
	let board = new_board depth in
	print_board ( get_list_str board ); print_endline "";
	let (move, v) = add_move 5 5 Grid.Piece.X board in print_board ( get_list_str move ); pritn_bool v ;print_endline "";
	let (move, v) = add_move 5 5 Grid.Piece.O move in print_board ( get_list_str move ); pritn_bool v ;print_endline "";
	let (move, v) = add_move 1 1 Grid.Piece.O move in print_board ( get_list_str move ); pritn_bool v ;print_endline "";
	let (move, v) = add_move 2 1 Grid.Piece.O move in print_board ( get_list_str move ); pritn_bool v ;print_endline "";
	let (move, v) = add_move 3 1 Grid.Piece.O move in print_board ( get_list_str move ); pritn_bool v ;print_endline "";
	let (move, v) = add_move 3 3 Grid.Piece.O move in print_board ( get_list_str move ); pritn_bool v ;print_endline ""









