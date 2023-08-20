use "hw2.sml";

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val t1_all_exc_opt = all_except_option ("string", []) = NONE;
val t2_all_exc_opt = all_except_option ("string", ["string"]) = SOME [];;
val t3_all_exc_opt = all_except_option ("string", ["string",
						   "integer"]) = SOME ["integer"];

val t1_get_substitutions1 = get_substitutions1 ([[]], "") = []
val t2_get_substitutions1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val t3_get_substitutions1 = get_substitutions1 ([["a", "b", "c"],
						 ["b", "c"]],
						"a") = ["b", "c"];

val t1_get_substitutions2 = get_substitutions2 ([[]], "") = []
val t2_get_substitutions2 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val t3_get_substitutions2 = get_substitutions2 ([["a", "b", "c"],
						 ["b", "c"]],
						"a") = ["b", "c"];

val t1_similar_names = similar_names ([[]],
				      {first="Fred", middle="W", last="Smith"}) =
		       [{first="Fred", middle="W", last="Smith"}]
val t2_similar_names = similar_names ([["Fred","Fredrick"],
				       ["Elizabeth","Betty"],
				       ["Freddie","Fred","F"]],
				      {first="Fred", middle="W", last="Smith"}) =
		       [{first="Fred", last="Smith", middle="W"},
			{first="Fredrick", last="Smith", middle="W"},
			{first="Freddie", last="Smith", middle="W"},
			{first="F", last="Smith", middle="W"}]

val t1_card_color = card_color (Clubs, Num 2) = Black
val t2_card_color = card_color (Spades, Num 6) = Black
val t3_card_color = card_color (Hearts, Jack) = Red
val t4_card_color = card_color (Diamonds, Num 2) = Red

val t1_card_value = card_value (Clubs, Num 2) = 2
val t2_card_value = card_value (Hearts, Jack) = 10
val t3_card_value = card_value (Spades, Ace) = 11

val t1_remove_card = ((remove_card ([], (Hearts, Ace), IllegalMove); false)
		      handle IllegalMove => true)
val t2_remove_card = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val t3_remove_card = remove_card ([(Hearts, Ace),
				   (Spades, Num 3)],
				  (Hearts, Ace), IllegalMove) = [(Spades, Num 3)]
val t4_remove_card = ((remove_card ([(Spades, Num 3)], (Hearts, Ace), IllegalMove); false)
		      handle IllegalMove => true | _ => false = true)

val t1_all_same_color = all_same_color [] = true
val t2_all_same_color = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val t3_all_same_color = all_same_color [(Hearts, Ace), (Spades, Ace)] = false

val t1_sum_cards = sum_cards [] = 0
val t2_sum_cards = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val t3_sum_cards = sum_cards [(Hearts, Jack),(Clubs, Num 2)] = 12
val t4_sum_cards = sum_cards [(Spades, Ace),(Diamonds, King)] = 21

val t1_score = score ([(Hearts, Num 2),(Clubs, Num 4)], 10) = (10 - (2 + 4))
val t2_score = score ([(Hearts, Num 13),(Clubs, Num 8)], 10) = 3 * ((13 + 8) - 10)
val t3_score = score ([(Hearts, Num 2),(Clubs, Num 4),(Spades, Jack)], 7) = 3 * ((2 + 4 + 10) - 7)
val t4_score = score ([(Hearts, Num 2),(Hearts, Num 4),(Diamonds, Jack)], 7) = 3 * ((2 + 4 + 10) - 7) div 2
								    
val t1_officiate = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val t2_officiate = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                              [Draw,Draw,Draw,Draw,Draw],
                              42)
		   = 3
val t3_officiate = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                               [Draw,Discard(Hearts,Jack)],
                               42);
		     false) 
		    handle IllegalMove => true)
