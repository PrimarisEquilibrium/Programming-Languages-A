(* int list -> int
   Produces the sum of all values in the list. *)

fun sum_list (xs : int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs);

val t1_sum_list = sum_list [] = 0;
val t2_sum_list = sum_list [1, 2, 3] = 6;


(* int list -> int
   Produces the product of all values in the list. *)

fun list_product (xs : int list) =
    if null xs
    then 1
    else hd xs * list_product(tl xs);

val t1_list_product = list_product [] = 1;
val t2_list_product = list_product [1, 2, 3] = 6;


(* int -> int list
   Produces a countdown from n to 1, (n, n-1, ..., 1). *)

fun countdown (x : int) =
    if x = 0
    then []
    else x :: countdown(x - 1);

val t1_countdown = countdown 0 = [];
val t2_countdown = countdown 7 = [7, 6, 5, 4, 3, 2, 1];


(* (int list, int list) -> int list
   Produces a new int list where ys is appended to xs *)

fun append (xs : int list, ys : int list) =
    if null xs then
	ys
    else
	hd xs :: append(tl xs, ys);

val t1_append = append([], []) = [];
val t2_append = append([1], []) = [1];
val t3_append = append([], [2]) = [2];
val t4_append = append([1], [2]) = [1, 2];
val t5_append = append([1, 2, 3], [4, 5, 6]) = [1, 2, 3, 4, 5, 6];


(* (int * int) list -> int
   Produces the sum of all values in the list, including both int's in each pair. *)

fun sum_pair_list (xs : (int * int) list) =
    if null xs
    then 0
    else (#1 (hd xs)) + (#2(hd xs))
	 + sum_pair_list(tl xs);

val t1_sum_pair_list = sum_pair_list([]) = 0;
val t2_sum_pair_list = sum_pair_list([(1, 2), (3, 4), (5, 6)]) = 21;


(* (int * int) list -> int list
   Produces an int list containing the first element in each pair. *)

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else (#1(hd xs)) :: (firsts(tl xs));

val t1_firsts = firsts([]) = [];
val t2_firsts = firsts([(2, 6), (9, 2), (14, 0)]) = [2, 9, 14];
