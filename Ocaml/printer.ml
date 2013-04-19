open Format


(* Primary printer ************************************************************)

let primary l1 l2 =
	let rec print_flip player_number time =
		printf "%d %f@\n" player_number time
	in
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
	in
	print_lists l1 l2


(* Latex generator ************************************************************)

let l s =
	printf "%s@\n" s

let latex l1 l2 =
	let out_channel = open_out "latex/a.tex" in
	let add file =
		let in_channel = open_in file in
		try
			while (true) do
				let text = (input_line in_channel) ^ "\n" in
				output_string out_channel text;
			done
		with
			End_of_file -> ()
	in
(* TODO
	let generate_code l1 l2 =
		blabla
	in
*)
	add "latex/header.tex";
	add "latex/drawrectangle.tex";
(* TODO
	add "latex/drawbullet.tex";
	output_string out_channel (generate_code l1 l2)
*)
	add "latex/footer.tex"

