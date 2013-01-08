(* old test case name: code422.sml *)

(* Untypable *)
signature s = sig type u type v type w type z end
structure S :> s = _structS
open S
overload c : 'a -> 'a -> 'a with 'a in (u, w, z, word)
fun f x = (fn x => c x 0w0) (x : w);
