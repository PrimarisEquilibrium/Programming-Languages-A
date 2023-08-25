(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer;

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern;

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu;

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end;

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string;

(**** you can put all your code here ****)

(* Produces a list only containing strings that start with an uppercase letter. *)
fun only_capitals (los : string list) : string list =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) los;


(* Produces the longest string in the list. *)
fun longest_string1 (los : string list) : string =
    foldl (fn (str, acc) => if String.size str > String.size acc then str else acc) "" los;


(* Produces the longest string in the list. In the event of a tie produce the string closest to the end of the list. *)
fun longest_string2 (los : string list) : string =
    foldl (fn (str, acc) => if String.size str >= String.size acc then str else acc) "" los;


(* The foldl functions handling string size. *)
fun longest_string_helper (f : (int * int -> bool)) (los: string list) : string =
    foldl (fn (str, acc) => if f(String.size str, String.size acc)
			    then str
			    else acc)
	  ""
	  los;


val longest_string3 = longest_string_helper (fn (i1, i2) => i1 > i2);
val longest_string4 = longest_string_helper (fn (i1, i2) => i1 >= i2);


(* Produces the longest string in the list that is capitalized, "" if no such string exists.
   ASSUME: All strings have atleast 1 character. *)
fun longest_capitalized (los : string list) : string =
    foldl (fn (str, acc) => if String.size str > String.size acc andalso
			       Char.isUpper(String.sub(str, 0)) then str else acc)
	  ""
	  los;


(* Reverses a string. *)
fun rev_string (s : string) : string =
    (implode o rev o explode) s;


(* Produces an option of the first answer recieved. If returns NONE for all elements raise NoAnswer. *)
fun first_answer (f : ('a -> 'b option)) (loa : 'a list) : 'b =
    case loa of
	[] => raise NoAnswer
      | a::loa' => case f a of
		       NONE => first_answer f loa'
		     | SOME n => n;


(* Produces the all the answers of f applied to loa. If any element returns NONE, the function is evaluated to NONE. *)
fun all_answers (f : ('a -> 'b list option)) (loa : 'a list) : 'b list option =
    let
	fun all_answers_acc (loa : 'a list, acc : 'b list option) : 'b list option =
	    case (loa, acc) of
		([], _) => acc
	      | (a::loa', SOME acc_lst) => (case f a of
					        NONE => NONE
					      | SOME x => all_answers_acc (loa', SOME (x @ acc_lst)))
	      | _ => NONE;
    in
	all_answers_acc (loa, SOME [])
    end;


(* Produces the total amount of Wildcard's a pattern contains *)
fun count_wildcards (p : pattern) : int =
    g (fn () => 1) (fn x => 0) p;


(* Produces the total amount of Wildcard's plus the sum of the string lengths of all the Variables. *)
fun count_wild_and_variable_lengths (p : pattern) : int =
    g (fn () => 1) String.size p;


(* Produces the number of times the string appears in the variable *)
fun count_some_var ((s : string), (p : pattern)) : int =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p
    

(* Produces true if all the variables appearing the pattern are distinct from one another; Otherwise false. *)
fun check_pat (p : pattern) : bool =
    let
	fun pat_vars_to_str (p : pattern) : string list =
	    case p of
		Variable x          => [x]
	      | TupleP ps           => List.foldl (fn (p, str_acc) => (pat_vars_to_str p) @ str_acc) [] ps
	      | ConstructorP (_, p) => pat_vars_to_str p
	      | _                   => [];

	fun unique_list (los : string list) : bool =
	    case los of
		[] => true
	      | s::los' => not (List.exists (fn x => x = s) los');
    in
	(unique_list o pat_vars_to_str) p
    end;


(* Produces a SOME list of bindings if valu matches pattern; Otherwise NONE if pattern does not match. *)
fun match (v : valu, p : pattern) : (string * valu) list option =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const vi, ConstP pi) => if v1 = pi then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
						     then match(v, p)
						     else NONE
      | _ => NONE;


(* Produces the first pattern that matches the value; Otherwise NONE *)
fun first_match (v : valu, lop : pattern list) : (string * valu) list option =
    SOME (first_answer (fn p => match(v, p)) lop) handle NoAnswer => NONE;
