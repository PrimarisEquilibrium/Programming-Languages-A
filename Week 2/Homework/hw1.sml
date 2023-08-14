(* Data Defintions *)

(*
Date is (Year * Month * Day)
Interp. A calendar date, where:
 - Year: Pos int
 - Month: int[1, 12]
 - Day: int [1, 31]

fun fun-for-date (d : int * int * int) =
    (#1 d) (* Year: Pos int *)
    (#2 d) (* Month: int[1, 12] *)
    (#3 d) (* Day: int [1, 31] *)
*)


(* Functions *)

(* Date, Date -> bool
   Produces true if the first date comes before the second date, false if both dates are the same. *)
fun is_older (d1 : (int * int * int), d2 : (int * int * int)) =
    let
	val (y1, m1, dd1) = d1;
	val (y2, m2, dd2) = d2;
    in
        (y1 < y2) orelse
        (y1 = y2 andalso m1 < m2) orelse
        (y1 = y2 andalso m1 = m2 andalso dd1 < dd2)
    end;

(* Date list, Month -> int
   Produces the amount of dates in the list are in the given month. *)
fun number_in_month (lod : (int * int * int) list, m : int) =
    if null lod
    then 0
    else
	if m = (#2 (hd lod))
	then 1 + number_in_month(tl lod, m)
	else number_in_month(tl lod, m);

(* Date list, Month list -> int
   Produces the amount of dates in the list are in the given list of months. *)
fun number_in_months (lod : (int * int * int) list, lom : int list) =
    if null lod orelse null lom
    then 0
    else number_in_month(lod, hd lom) +
	 number_in_months(tl lod, tl lom);


(* Date list, Month -> Date list
   Produces the dates that are in the passed month. *)
fun dates_in_month (lod : (int * int * int) list, m : int) =
    if null lod
    then []
    else
	if m = (#2 (hd lod))
	then hd lod :: dates_in_month(tl lod, m)
	else dates_in_month(tl lod, m);


(* Date list, Month list -> Date list
   Produces the dates that are in the passed list of months. *)
fun dates_in_months (lod : (int * int * int) list, lom : int list) =
    if null lod orelse null lom
    then []
    else dates_in_month(lod, hd lom) @
	 dates_in_months(tl lod, tl lom);


(* string list, int -> string
   Produces the nth element of the list.
   ASSUME: length of str list >= int
   NOTE: Uses one-based indexing. *)
fun get_nth (los : string list, n : int) =
    if n = 1
    then hd los
    else get_nth(tl los, n-1);


(* Date -> string
   Produces the string representation of the date. *)
fun date_to_string (d : (int * int * int)) =
    let
	val months = ["January", "February", "March",
		      "April", "May", "June",
		      "July", "August", "September",
		      "October", "November", "December"]
	val month = get_nth(months, #2 d)
	val day = Int.toString(#3 d)
	val year = Int.toString(#1 d)
    in
	month ^ " " ^ day ^ ", " ^ year
    end;


(* int, int list -> int
   Produces an int such that the first n elements of the list add to less than sum (int), however the first n + 1
   elements add to sum or more.
   ASSUME: int and all elements in int list are all positive. Additionally the sum of the entire int list is greater
           than sum. *)
fun number_before_reaching_sum (sum : int, loi : int list) =
    if sum <= (hd loi)
    then 0
    else 1 + number_before_reaching_sum(sum - (hd loi), tl loi);



(* int -> Month
   Produces the month a day of a year is in.
   ASSUME: int must be between the range of [1, 365]. *)
fun what_month (d : int) =
    let
	val daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(d, daysInMonths)
    end;


(* int, int -> int list
   Produces a list between [d1, d2] where each day is mapped to what month the current date is. *)
fun month_range (d1 : int, d2 : int) =
    if d1 > d2
    then []
    else what_month(d1) :: month_range(d1+1, d2);


(* Date list -> Date option
   Produces the SOME d if d is the oldest date in the given list of dates, otherwise NONE. *)
fun oldest (lod : (int * int * int) list) =
    if null lod
    then NONE
    else let
	fun oldest_notempty (lod : (int * int * int) list) =
	    if null(tl lod)
	    then SOME(hd lod)
	    else
		let
		    val tl_ans = oldest(tl lod);
		in
		    if is_older(hd lod, valOf tl_ans)
		    then SOME(hd lod)
		    else tl_ans
		end
    in
	oldest_notempty(lod)
    end;


(* Date list, int list -> int
   Similar to number_in_months but when a month is present multiple times in the second argument, it would not have
   any additional effect beyond being present once. *)
fun number_in_months_challenge (lod : (int * int * int) list, loi : int list) =
    let
	fun exists_in_list (i : int, loi : int list) =
	    if null loi
	    then false
	    else
		if i = (hd loi)
		then true
		else exists_in_list(i, tl loi);
	
	fun remove_duplicates (loi : int list) =
	    if null loi
	    then []
	    else
		if exists_in_list(hd loi, tl loi)
		then remove_duplicates(tl loi)
		else hd loi :: remove_duplicates(tl loi);
    in
	number_in_months(lod, remove_duplicates loi)
    end;


(* Date -> bool
   Produces true if the date is a reasonable date in the common era, the following is the criteria:
    - Postive year (cannot be 0)
    - Month between 1-12
    - Day appropriate for the month
    - Handle leap years, years divisible by 4 not by 100 *)
fun reasonable_date (dt : (int * int * int)) =
    let
	val (y, m, d) = dt;

	fun get_nth_int (i : int, loi : int list) =
	    if i = 1
	    then hd loi
	    else get_nth_int(i-1, tl loi);

	fun valid_year (y : int) =
	    y > 0;

	fun valid_month (m : int) =
	    m >= 1 andalso m <= 12;

	fun valid_day (dt : (int * int * int)) =
	    let
		val (y, m, d) = dt;
		val daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
		val is_leap = (y mod 4 = 0) andalso not(y mod 100 = 0);
	    in
		if is_leap andalso d = 29
		then true
		else
		    0 < d andalso d <= get_nth_int(m, daysInMonths)
	    end;
    in
	valid_year y andalso valid_month m andalso valid_day dt
    end;
