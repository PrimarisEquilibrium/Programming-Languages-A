(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int ;
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw ;

exception IllegalMove;

(* put your solutions for problem 2 here *)

(* Produces SOME lst where str is not in lst, NONE if string is not in list. *)
fun all_except_option (s : string, los : string list) : string list option =
    case los of
	[] => NONE
      | s'::los' => if same_string(s', s)
		    then SOME los'
		    else case all_except_option(s, los') of
			     NONE => NONE
			   | SOME n => SOME (s'::n);


(* Produces a list of strings found in the same sublists as the target, excluding the target itself. *)
fun get_substitutions1 (xs : string list list, s : string) : string list =
    case xs of
	[] => []
      | x::xs' => case all_except_option(s, x) of
			  NONE => get_substitutions1(xs', s)
			| SOME v => v @ get_substitutions1(xs', s)
							  

(* Tail recursive implimentation of get_substitutions1. *)
fun get_substitutions2 (xs : string list list, s : string) : string list =
    let
	fun get_substitutions2_acc (xs : string list list, s : string, rsf : string list) : string list =
	    case xs of
		[] => rsf
	      | x::xs' => case all_except_option(s, x) of
			      NONE => get_substitutions2_acc(xs', s, rsf)
			    | SOME v => get_substitutions2_acc(xs', s, rsf @ v)
    in
	get_substitutions2_acc(xs, s, [])
    end;


type full_name = {first: string, middle: string, last: string};

(* Produces a list of full_name's that are similar to the passed name. *)
fun similar_names (xs : string list list, {first: string, middle: string, last: string}) : full_name list =
    let
	val name = {first=first, middle=middle, last=last};
	fun gen_names [] = []
	  | gen_names(s::los') = {first=s, middle=middle, last=last} :: gen_names(los');
    in
	name :: gen_names(get_substitutions2(xs, first))
    end;


(* Produces the color of a card. *)
fun card_color (c : card) : color =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Hearts, _) => Red
      | (Diamonds, _) => Red;



(* Produces the value of a card. *)
fun card_value (c : card) : int =
    case c of
	(_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num n) => n;


(* Removes any cards that are equal to c, if c does not exist raise e. *)
fun remove_card (loc : card list, c : card, e : exn) : card list =
    let
	fun card_exists (loc : card list, c : card) : bool =
	    case loc of
		[] => false
	      | c'::loc' => if c' = c then true else card_exists(loc', c);

	fun remove_card_main (loc : card list, c : card) : card list =
	    case loc of
		[] => []
	      | c'::loc' => if c' = c then remove_card_main(loc', c)
			    else c' :: remove_card_main(loc', c)
    in
	if card_exists(loc, c) then
	   remove_card_main(loc, c)
	else raise e
    end;


(* Produces true if all the cards in the list are the same color, otherwise false. *)
fun all_same_color (loc : card list) : bool =
    case loc of 
	c1::c2::loc' => if card_color c1 = card_color c2 then
			    true andalso all_same_color(loc')
			else false
      | _ => true; 


(* Produces the sum of all the cards in the list. *)
fun sum_cards (loc : card list) : int =
    let
	fun sum_cards_acc (loc : card list, rsf : int) =
	    case loc of
		[] => rsf
	      | c::loc' => sum_cards_acc(loc', rsf + card_value c);
    in
	sum_cards_acc(loc, 0)
    end;


(* Produces the final game score for a given list of held-cards. The rules are as followed:
    - sum > goal: 3 * (sum - goal)
    - sum < goal: 3 * (goal - sum)
   Depending on the final arrangements of held-cards the rules are as followed:
    - Cards are all different colors: preliminary score / 2
    - Cards are all the same color: preliminary score *)
fun score (loc : card list, g : int) =
    let
	val s = sum_cards loc;
	val ps = if s > g
		 then 3 * (s - g)
		 else (g - s)
    in
	if all_same_color loc
	then ps div 2
	else ps
    end;


(* Produces the final score after officiating a game with a list of game cards, the list of moves, and a goal. *)
fun officiate (loc : card list, lom : move list, g : int) =
    let
	fun officiate_main (loc : card list, lom : move list, hc : card list) =
	    if sum_cards hc > g then score(hc, g) else
	    case lom of
		[] => score(hc, g)
	      | Discard c::lom' => officiate_main(remove_card(loc, c, IllegalMove), lom', hc)
	      | Draw::lom' => case loc of
				  [] => score(hc, g)
				| c::loc' => officiate_main(loc', lom', c :: hc);
    in
	officiate_main(loc, lom, [])
    end;


