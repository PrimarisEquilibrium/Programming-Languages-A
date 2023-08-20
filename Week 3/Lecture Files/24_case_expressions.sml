(* Programming Languages, Dan Grossman *)
(* Section 2: Case Expressions *)

datatype mytype = TwoInts of int * int 
		| Str of string 
		| Pizza

(* mytype -> int *)
fun f (x : mytype) = 
    case x of 
	Pizza => 3 
      | Str s => 8
      | TwoInts(i1,i2) => i1 + i2;

(*    | Pizza => 4; (* redundant case: error *) *)
(* fun g x = case x of Pizza => 3 (* missing cases: warning *) *)


datatype shape = Circle of real
	       | Rectangle of (real * real);
(* Interp. A shape, either a circle or rectangle. Circle takes a radius and rectangle takes
           a width and length. *)
val S1 = Circle 5.0;
val S2 = Rectangle(4.0, 6.0);
(*
fun fun_for_shape (s : shape) =
    case s of
	Circle r => NONE
      | Rectangle(r1, r2) => NONE;
*)

(* shape -> real
   Produces the area of the given shape. *)
fun area (s) =
    let
	val pi = 3.14159;
    in
	case s of
	    Circle r => pi * (r * 2.0)
	  | Rectangle(r1, r2) => r1 * r2
    end;

val t1_area = abs(area S1 - 31.42) < 0.1 = true;
val t2_area = abs(area S2 - 24.0) < 0.1 = true;
