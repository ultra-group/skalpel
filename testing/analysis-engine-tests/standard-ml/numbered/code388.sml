(* old test case name: code388.sml *)

(* untypable *)
datatype 'a u = C of 'a | D of 'a v withtype 'b v = 'b u;
(D (C 1)) : bool v;;
