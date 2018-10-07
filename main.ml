(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: amerelo <amerelo@student.42.fr>            +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/10/06 16:30:20 by amerelo           #+#    #+#             *)
(*   Updated: 2018/10/07 23:29:56 by amerelo          ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let test_if_int str = 
	let len = String.length str in 
	let rec loop i valid = match i with
		| i when i = len -> valid
		| i -> let get1 = String.get str i in if (int_of_char) get1 < (int_of_char '0') || ((int_of_char str.[i]) > (int_of_char '9')) then loop (i+1) false
					else loop (i+1) valid
	in loop 0 true

let test_list lst = if (List.length lst) <> 2 then ((0,0), false)
	else let rec loop lst = match lst with
			| []		-> ((0,0), false)
			| _::[]		-> ((0,0), false)
			| v1::v2::tl-> 
				if (test_if_int (String.trim v1)) && (test_if_int (String.trim v2)) then 
						(( (int_of_string (String.trim v1)), (int_of_string (String.trim v2)) ), true )
				else ((0,0), false)
		in loop lst

let pars_line line = test_list (String.split_on_char ' ' (String.trim line))
let out_of_bounds (y, x) depth = if ((Board.ft_power 3 depth) < x) || ((Board.ft_power 3 depth) < y) || (x < 1) || (y < 1) then true else false 


let pritn_bool b = if b = true then print_endline "true" else print_endline "false"

let rec loop_line depth board p = 
	let line = read_line () in
	let (elem, v) = pars_line line in match elem, v with
		| _, v 	when v = false						-> print_endline "Incorrect format try again."; loop_line depth board p
		| e, _ when ((out_of_bounds e (depth + 1)) = true)-> print_endline "Out of bounds try again." ; loop_line depth board p
		| (y, x), _ 										-> let (board, r) = Board.add_move y x p board in match board, r with
																| _, v when v = false	-> print_endline "Illegal move try again." ; loop_line depth board p
																| newboard, _ 			-> newboard

let print_player_turn player = match player with
		| false -> print_endline "O's turn to play."
		| true -> print_endline "X's turn to play."

let getplayer player =  match player with
		| false -> Grid.Piece.O
		| true -> Grid.Piece.X

let changeplayer player =  match player with
		| false -> true
		| true -> false

let player_win player = match player with
		| false -> print_endline "end of game player O's Win. \n"
		| true -> print_endline "end of game player O's Win. \n "


let main () = 
	let depth = 1 in
	let board = Board.new_board depth in
	let player = false in
	Board.print_board board;

	let rec loopgame player board = 
		print_player_turn player;
		let newboard = loop_line depth board (getplayer player) in
		Board.print_board newboard;

		let ret = Board.can_i_play newboard in match ret with
			| false -> print_endline "No More moves  "; player_win player
			| true -> if (Board.get_w newboard) = Grid.Piece.E then loopgame (changeplayer player) newboard
					else player_win player
	in loopgame player board 


let () = main ()