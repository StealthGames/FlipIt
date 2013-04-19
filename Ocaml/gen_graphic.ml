let gen_graphic tab1 tab2 =
  let tab1 = List.sort Pervasives.compare tab1 and tab2 = List.sort Pervasives.compare tab2 in
  let rec aux lastpos lastplayer = function
      ([],[]) -> Format.sprintf "\\drawrect{%f}{%f}{%f}{%f}{%s}\n" lastpos 13. 0. 1. (if lastplayer = 1 then "blue" else "red") 
    | (a::q, []) -> (if lastplayer <> 1 then Format.sprintf "\\drawrect{%f}{%f}{%f}{%f}{%s}\n" lastpos a (-1.) 0. "red" else "")^(aux a 1 (q,[])) 
    | ([], a::q) -> (if lastplayer <> -1 then Format.sprintf "\\drawrect{%f}{%f}{%f}{%f}{%s}\n" lastpos a 0. 1. "blue" else "")^(aux a (-1) ([],q))
    | (a1::q1,a2::q2) when a1 <= a2 -> (if lastplayer <> 1 then Format.sprintf "\\drawrect{%f}{%f}{%f}{%f}{%s}\n" lastpos a1 (-1.) 0. "red" else "")^(aux (if lastplayer <> 1 then a1 else lastpos) 1 (q1,a2::q2))
    | (a1::q1,a2::q2) -> (if lastplayer <> -1 then Format.sprintf "\\drawrect{%f}{%f}{%f}{%f}{%s}\n" lastpos a2 0. 1. "blue" else "")^(aux (if lastplayer <> -1 then a2 else lastpos) (-1) (a1::q1,q2)) in
  let rec aux2 height color = function
      [] -> ""
    | a::q -> (Format.sprintf "\\drawbullet{%f}{%f}{%s}\n" a height color)^(aux2 height color q) in
    (aux2 2. "blue" tab1)^(aux2 (-2.) "red" tab2)^(aux 0. 1 (tab1,tab2))
let _ = print_string (gen_graphic [1.; 3.; 5.; 7.;9.] [0.5; 5.; 6.1;7.5])

