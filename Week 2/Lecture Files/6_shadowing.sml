(* Programming Languages, Dan Grossman *)
(* Section 1: Examples to Demonstrate Shadowing *)

val a = 10;

val b = a * 2;

val a = 5;

val c = b;

val d = a;

val a = a + 1;

val f = a * 2;
