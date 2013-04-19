let gen_graphic tab1 tab2 =
  let maxpos = max (List.fold_left max 0. tab1) (List.fold_left max 0. tab2) in 
  let tab1 = List.map (function x -> x/.maxpos*.12.) (List.sort Pervasives.compare tab1) and tab2 = List.map (function x -> x/.maxpos*.12.) (List.sort Pervasives.compare tab2) in
  let rec aux lastpos lastplayer = function
      ([],[]) -> Format.sprintf "\\drawrectforplayer{%f}{%f}{%d}\n" lastpos 13. lastplayer 
    | (a::q, []) -> (
        if lastplayer <> 1 
           then Format.sprintf "\\drawrectforplayer{%f}{%f}{%d}\n" lastpos a (-1)
           else "")^(aux a 1 (q,[])) 
    | ([], a::q) -> (if lastplayer <> -1 then Format.sprintf "\\drawrectforplayer{%f}{%f}{%d}\n" lastpos a 1 else "")^(aux a (-1) ([],q))
    | (a1::q1,a2::q2) when a1 <= a2 -> (if lastplayer <> 1 then Format.sprintf "\\drawrectforplayer{%f}{%f}{%d}\n" lastpos a1 (-1) else "")^(aux (if lastplayer <> 1 then a1 else lastpos) 1 (q1,a2::q2))
    | (a1::q1,a2::q2) -> (if lastplayer <> -1 then Format.sprintf "\\drawrectforplayer{%f}{%f}{%d}\n" lastpos a2 1 else "")^(aux (if lastplayer <> -1 then a2 else lastpos) (-1) (a1::q1,q2)) in
  let rec aux2 player = function
      [] -> ""
    | a::q -> (Format.sprintf "\\drawbulletforplayer{%f}{%d}\n" a player)^(aux2 player q) in
    (aux2 1 tab1)^(aux2 (-1) tab2)^(aux 0. 1 (tab1,tab2))
let _ = print_string (gen_graphic [1.; 3.; 5.; 7.;9.] [0.5; 5.; 6.1;7.5])

