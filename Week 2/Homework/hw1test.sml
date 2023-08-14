use "hw1.sml";

(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val t1_is_older = is_older ((1,2,3),(1,2,3)) = false
val t2_is_older = is_older ((1,2,3),(2,3,4)) = true
val t3_is_older = is_older ((2,3,4),(1,2,3)) = false
val t4_is_older = is_older((2011, 3, 31), (2011, 4, 28)) = true;

val t1_number_in_month = number_in_month ([],1) = 0
val t2_number_in_month = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val t1_number_in_months = number_in_months ([],[2,3,4]) = 0
val t2_number_in_months = number_in_months ([(2012,2,28),(2013,12,1),
					     (2011,3,31),(2011,4,28)],
					    []) = 0
val t3_number_in_months = number_in_months ([],[]) = 0
val t4_number_in_months = number_in_months ([(2012,2,28),(2013,12,1),
					     (2011,3,31),(2011,4,28)],
					    [2,3,4]) = 3

val t1_dates_in_month = dates_in_month ([],1) = []
val t2_dates_in_month = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val t3_dates_in_month = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),
								       (2013,2,1)]

val t1_dates_in_months = dates_in_months ([],[]) = []
val t2_dates_in_months = dates_in_months ([(2012,2,28)],[]) = []
val t3_dates_in_months = dates_in_months ([],[2,3,4]) = []
val t4_dates_in_months = dates_in_months ([(2012,2,28),(2013,12,1),
					   (2011,3,31),(2011,4,28)],[2,3,4]) =
			 [(2012,2,28),(2011,3,31),(2011,4,28)]
                                   
val t1_get_nth = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val t1_date_to_string = date_to_string (2013, 6, 1) = "June 1, 2013"

val t1_number_before_reaching_sum  = number_before_reaching_sum (0, [1,2,3,4,5]) = 0
val t2_number_before_reaching_sum  = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val t3_number_before_reaching_sum  = number_before_reaching_sum (15, [1,2,3,4,5]) = 4

val t1_what_month = what_month 70 = 3
val t2_what_month = what_month 31 = 1

val t1_month_range  = month_range (1, 1) = [1]
val t2_month_range  = month_range (2, 1) = []
val t3_month_range  = month_range (31, 34) = [1,2,2,2]

val t1_oldest = oldest([]) = NONE
val t2_oldest = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val t1_number_in_months_challenge = number_in_months ([],[2,3,4]) = 0
val t2_number_in_months_challenge  = number_in_months ([(2012,2,28),(2013,12,1),
							(2011,3,31),(2011,4,28)],
						       []) = 0
val t3_number_in_months_challenge = number_in_months ([],[]) = 0
val t4_number_in_months_challenge = number_in_months ([(2012,2,28),(2013,12,1),
						       (2011,3,31),(2011,4,28)],
						      [2,3,4]) = 3

val t1_reasonable_date = reasonable_date (0, 11, 26) = false;
val t2_reasonable_date = reasonable_date (2023, 13, 26) = false;
val t3_reasonable_date = reasonable_date (2023, ~2, 26) = false;
val t4_reasonable_date = reasonable_date (2023, 3, 32) = false;
val t5_reasonable_date = reasonable_date (2023, 3, 0) = false;
val t6_reasonable_date = reasonable_date (2023, 2, 29) = false;
val t7_reasonable_date = reasonable_date (2024, 2, 29) = true;
val t8_reasonable_date = reasonable_date (2023, 8, 14) = true;
