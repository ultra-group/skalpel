(* old test case name: code240.sml *)

(* These two lines are typable *)
datatype 'a t = C of 'a | D
datatype u = datatype t
(* This is typable *)
val _ : int u = C 1
(* This isn't typable: constructor clashes *)
val _ : bool u = (C 1) : bool t
(* This isn't typable: constructor clashes *)
val _ : bool u = (C 1) : int t
val _ : int u = (C 1) : bool t
(* This isn't typable: arity clash *)
val _ : u = C 1;
