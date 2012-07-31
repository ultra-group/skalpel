(* old test case name: code499.sml *)

(* Untypable - The opening of S.T hides f because S
 * does not define T.  *)
structure S = struct end;
fun f () = ();
open S.T;
val _ = f 1;
