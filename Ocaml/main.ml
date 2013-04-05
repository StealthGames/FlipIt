(* ************************************************************************** *)
(*                                                                            *)
(* ************************************************************************** *)

type methode =
    { next_move : unit -> float;
      give_info : float*int -> unit;
      init : unit -> unit;
    };;

(* ************************************************************************** *)
(*                                                                            *)
(* ************************************************************************** *)

(* creer une methode periodique a partir de la periode et de la phase *)
let new_methode_period period phase =
  let time = ref phase in
  {
    next_move = (function () -> (time := !time +. period; !time -. period));
    give_info = (function x -> ());
    init = function () -> (time := phase; ())
  }
;;

let print_score (a,b) =
	print_string (
		"joueur 1 : "^(string_of_float a)^
		"\njoueur 2 : "^(string_of_float b)^"\n")
;;

(* ************************************************************************** *)
(*                                                                            *)
(* ************************************************************************** *)

(* fait une partie entre deux methodes donnees durant un temps time *)
(* renvoit les deux score sous forme de couple joueur1/joueur2      *)
let game met1 met2 time =
  let add_s (a,b) g = function | 1 -> (a+.g,b) | _ -> (a,b+.g) in
  let rec boucle s (t1, t2) t p =
    if time<=(min t1 t2) then (add_s s (time-.t) p)
    else (
	if t1<t2 then (
	    met1.give_info (t,p);
	    boucle (add_s s (t1 -. t) p) (met1.next_move (), t2) t1 1 )
	else (
	    met2.give_info (t,p);
	    boucle (add_s s (t2 -. t) p) (t1, met2.next_move ()) t2 2 )
    )
  in 
    met1.init (); met2.init ();
    boucle (0.,0.) (met1.next_move (), met2.next_move ()) 0. 1
;;

(* ************************************************************************** *)
(*                                                                            *)
(* ************************************************************************** *)

let period1 = float_of_string Sys.argv.(1);;
let period2 = float_of_string Sys.argv.(2);;

print_score (game (new_methode_period period1 0.) (new_methode_period period2 0.5) 50.);;

(*print_score (game (new_methode_period 2. 0.) (new_methode_period 3. 0.5) 50.);;*)

