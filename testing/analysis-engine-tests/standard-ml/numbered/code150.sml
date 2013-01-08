(* old test case name: code150.sml *)

(* we should otain an error because u can't be a 'a t *)
datatype 'a t = T of 'a t;
T (x : u);
