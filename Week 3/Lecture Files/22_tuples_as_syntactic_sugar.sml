val a_pair = (3+1, 4+2);
val another_pair = {2=5, 1=6};
#1 a_pair + #2 another_pair; (* -> 9 *)

(* Tuples are records but with syntactic sugar: (e1, ..., en) = {1=e1, ..., n=en} *)
