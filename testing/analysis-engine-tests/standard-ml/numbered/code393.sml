(* old test case name: code393.sml *)

(* typable *)
functor f (s : sig end) = struct end;
structure S = f(Int);
