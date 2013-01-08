(* old test case name: code419.sml *)

(* Untypable, but not arity error because the 2 instances of X.v
 * are different *)
type t = int X.v
structure S = struct structure X = struct type v = int end end
open S
val _ : X.v = true;
