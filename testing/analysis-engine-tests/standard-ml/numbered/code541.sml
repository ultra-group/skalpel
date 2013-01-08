(* old test case name: code541.sml *)

(* Untypable.
 *
 * The specification y included in s is unmatched in S. *)

signature s1 = sig val x : unit end
signature s2 = sig val y : bool end
signature s  = sig include s1 include s2 end
structure S :> s = struct val x = () val x = true end;
