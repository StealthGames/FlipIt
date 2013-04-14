(* Type ***********************************************************************)

type history =
{
	mutable player1: float list;
	mutable player2: float list;
	mutable last_flip: float;
}


(* Reference ******************************************************************)

let history =
{
	player1 = [];
	player2 = [];
	last_flip = 0.;
}


(* Functions ******************************************************************)

let init () =
	history.player1 <- [];
	history.player2 <- [];
	history.last_flip <- 0.

let add player_number time =
	if (time < history.last_flip)
	then failwith "History.add_to_history Error: Invalid flip time";
	history.last_flip <- time;
	match player_number with
		| 1 -> history.player1 <- time :: history.player1
		| 2 -> history.player2 <- time :: history.player2
		| _ -> failwith "History.add_to_history Error: Unknow player number"

let print () =
	Printer.latex history.player1 history.player2

