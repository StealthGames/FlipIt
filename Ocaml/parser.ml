
(* Type ***********************************************************************)

type regexp =
{
	regexp : Str.regexp;
	fields : (int * string option) list;
}


(* Regular expression *********************************************************)

let line_regexp =
{
	regexp = Str.regexp "\\([1-2]\\)\\([ \t]\\)\\([0-9]+\\.[0-9]*\\)";
	fields = [(1, Some "1"); (3, Some "0.0")]
}


(* Functions ******************************************************************)

let regexp_match r string =
	let get (pos, default) =
		try
			Str.matched_group pos string
		with
			Not_found ->
		      match default with
		      | Some s -> s
		      | _ -> raise Not_found
	in
	try
		if Str.string_match r.regexp string 0
		then Some (List.map get r.fields)
		else None
	with Not_found -> None

let parse_line line = match regexp_match line_regexp line with
	| Some (player :: time :: []) -> (int_of_string player, float_of_string time)
	| _ -> failwith "Error: parser.ml parse_flip"

let rec split l1 l2 = function
	| [] -> (l1, l2)
	| (1, t) :: tl -> split (t :: l1) l2 tl
	| (2, t) :: tl -> split l1 (t :: l2) tl
	| _ -> failwith "Error: parser.ml split"

let parse file_name =
	let in_channel = open_in file_name in
	let rec parse_in_channel temp =
		try
			let line = input_line in_channel in
			try
				parse_in_channel ((parse_line line) :: temp)
			with
				_ -> parse_in_channel temp
		with
			End_of_file -> temp
	in
	let l = parse_in_channel [] in
	split [] [] l


(* Test part *******************************************************************
let (l1, l2) = parse Sys.argv.(1)

let _ = List.iter (fun t -> Format.printf "%f@\n" t) l1
let _ = Format.printf "@\n"
let _ = List.iter (fun t -> Format.printf "%f@\n" t) l2
let _ = Format.printf "@\n"
*******************************************************************************)

