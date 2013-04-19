let gen_graphic tab1 tab2 =
  let maxpos = max (List.fold_left max 0. tab1) (List.fold_left max 0. tab2) in 
  let tab1 = List.map (function x -> x/.maxpos*.16.) (List.sort Pervasives.compare tab1) and tab2 = List.map (function x -> x/.maxpos*.16.) (List.sort Pervasives.compare tab2) in
  let rec aux lastpos lastplayer = function
      ([],[]) -> Format.sprintf "\\drawrectforplayer{%f}{%f}{%d}\n" lastpos 17. lastplayer 
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
    (aux2 1 tab1)^(aux2 (-1) tab2)^(aux 0. (if List.hd tab1 <= List.hd tab2 then 1 else -1) (tab1,tab2))
let calc_listn n =
  let rec aux = function
   0 -> [0]
     | n -> n::(aux (n-1))
  in List.rev (aux n) 

let _ = print_string (gen_graphic (List.map (function x -> 7.*.(float_of_int x) +. 0.5) (calc_listn 4))  (List.map (function x -> 5.*.(float_of_int x)) (calc_listn 5)))

