(* old test case name: code305.sml *)

datatype t = C of int
signature foo = sig val x : int end
(* typable *)
structure bar : foo = struct val C x = C 1 end
(* untypable *)
structure bla : foo = struct val C y = C 1 end;
