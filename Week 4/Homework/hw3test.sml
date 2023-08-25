use "hw3.sml";

(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val t1_only_capitals = only_capitals [] = []
val t2_only_capitals = only_capitals ["A","B","C"] = ["A","B","C"]
val t3_only_capitals = only_capitals ["a","b","c"] = []

val t1_longest_string1 = longest_string1 [] = ""
val t2_longest_string1 = longest_string1 ["A","bc","C"] = "bc"
						 
val t1_longest_string2 = longest_string2 [] = ""
val t2_longest_string2 = longest_string2 ["A","bc","C"] = "bc"
val t3_longest_string2 = longest_string2 ["bc", "cd"] = "cd"


val t1_longest_string3 = longest_string3 ["A","bc","C"] = "bc"
							      
val t1_longest_string4 = longest_string4 ["A","B","C"] = "C"

val t1_longest_string_helper = longest_string_helper (fn (i1, i2) => i1 > i2)
						     ["A", "bc", "C"] = "bc"

val t1_longest_capitalized = longest_capitalized [] = ""
val t2_longest_capitalized = longest_capitalized ["A","bc","C"] = "A"
val t3_longest_capitalized = longest_capitalized ["A","bc","CA"] = "CA"

val t1_rev_string = rev_string "" = ""
val t2_rev_string = rev_string "abc" = "cba"

val t1_first_answer = ((first_answer (fn x => NONE) []); false)
		      handle NoAnswer => true = true
val t2_first_answer = first_answer (fn x => if x > 3 then SOME x else NONE)
				   [1,2,3,4,5] = 4
										    
val t1_all_answers = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val t2_all_answers = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1, 1, 1] = SOME [1, 1, 1];
val t3_all_answers = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME [];

val t1_count_wildcards = count_wildcards (Variable "e") = 0
val t2_count_wildcards = count_wildcards Wildcard = 1
val t3_count_wildcards = count_wildcards (TupleP [Wildcard, Variable "x"]) = 1

val t1_cnt_wild_and_var_len = count_wild_and_variable_lengths (Variable("a")) = 1
val t2_cnt_wild_and_var_len = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "x"]) = 2
										    
val t1_count_some_var = count_some_var ("x", Variable("x")) = 1
val t2_count_some_var = count_some_var ("xy", Variable("x")) = 0
val t3_count_some_var = count_some_var ("abc", (TupleP [Variable("ab"), Variable("abc")])) = 1
						       
val t1_check_pat = check_pat (Variable("x")) = true
val t2_check_pat = check_pat (TupleP [Variable("x"), Variable("x")]) = false
val t3_check_pat = check_pat (TupleP [Variable("x"),
				      Variable("y"),
				      Variable("z")]) = true
val t4_check_pat = check_pat (ConstructorP ("C", Variable("x"))) = true;
val t5_check_pat = check_pat (ConstructorP ("C", (TupleP [Variable("x"),
							  Variable("x")]))) = false;
					     
val t1_match = match (Const 12, Wildcard) = SOME []
val t2_match = match (Const 8, Variable "x") = SOME [("x", Const 8)]
val t3_match = match (Unit, UnitP) = SOME []
val t4_match = match (Const 9, ConstP 9) = SOME []
val t5_match = match (Tuple  [Unit, Const 2, Unit],
		      TupleP [UnitP, ConstP 2, Variable "e"]) = SOME [("e", Unit)]
val t6_match = match (Tuple  [Unit, Unit, Unit],
		      TupleP [UnitP, ConstP 2, Variable "e"]) = NONE
val t7_match = match (Constructor("t1", Const 1), ConstructorP("t1", ConstP 1)) = SOME []
val t8_match = match (Constructor("t2", Const 1), ConstructorP("t1", ConstP 1)) = NONE
val t9_match = match (Const 12, UnitP) = NONE

val t1_first_match = first_match (Unit, []) = NONE
val t2_first_match = first_match (Unit, [UnitP]) = SOME []
