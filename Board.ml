
type 'a t = Group of ('a t) list * Grid.Piece.t * int | Solo of 'a

let ft_power x y =
	if y = 0 then 1
	else let rec loop v r =
			if r > 1 then
				loop (v * x) (r - 1)
			else v
		in loop x y


let rec print_line line = match line with
	| [] -> ()
	| hd::tl -> print_string hd; if tl != [] then print_string " | " ; print_line tl

let rec print_bottom depth = match depth with
	| 0 -> print_endline ""
	| _ -> print_string "________" ; print_bottom (depth-1) 


let rec print_board lst i = match lst with
	| [] -> ()
	| hd::tl -> print_line hd; print_endline ""; if i = 2 && tl != []then begin print_bottom (List.length hd); print_board tl (0) end else print_board tl (i+1)


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

(* add generation of winer for depth n *)
let rec get_list_str board = match board with
	| Solo grid -> (Grid.toStringList grid)
	| Group (listg, _, depth) -> let newListStirng = let rec gen_lgrid listg i lst = match listg with 
														| [] -> lst
														| hd::tl -> gen_lgrid tl (i + 1) (merge_lst lst (get_list_str hd) depth i)
													in gen_lgrid listg 1 []
								in newListStirng

let add_move y x board = ()

(* match board with 
	| Solo grid -> print_endline "Solo of depth : 0";
	| Group (_, _, depth) -> print_string "Group of depth : "; print_int depth; print_endline "" *)

let test_board () = 
	let board = new_board 1 in 
	print_board ( get_list_str board ) 0