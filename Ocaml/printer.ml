open Unix
open Format


(* Primary printer ************************************************************)

let time () =
	let time = time () in
	let local = localtime time in
	let year = string_of_int (local.tm_year + 1900) in
	let mon = (if local.tm_mon < 9 then "0" else  "") ^ string_of_int (local.tm_mon + 1) in
	let day = string_of_int local.tm_mday in
	let hour = string_of_int local.tm_hour in
	let min = string_of_int local.tm_min in
	let sec = (if local.tm_sec < 10 then "0" else "") ^ (string_of_int local.tm_sec) in
	year ^ "/" ^  mon ^ "/" ^ day ^ " " ^ hour ^ ":" ^ min ^ ":" ^ sec

let print_header () =
	printf "// Game simulation@\n";
	printf "// Time: %s@\n" (time ())

let rec print_flip player_number time =
	printf "%d\t%f@\n" player_number time

let rec print_lists l1 l2 = match (l1, l2) with
	| (hd1 :: tl1, hd2 :: tl2) ->
		if (hd1 > hd2)
		then
			begin
			print_lists tl1 l2;
			print_flip 1 hd1;
			end
		else
			begin
			print_lists l1 tl2;
			print_flip 2 hd2;
			end
	| (hd1 :: tl1, []) ->
		print_flip 1 hd1;
		print_lists tl1 []
	| ([], hd2 :: tl2)->
		print_flip 2 hd2;
		print_lists [] tl2
	| ([], []) ->
		()

let console l1 l2 =
	print_header ();
	print_lists l1 l2

