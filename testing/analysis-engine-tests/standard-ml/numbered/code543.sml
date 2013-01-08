(* old test case name: code543.sml *)

(* Untypable.
 *
 * The declaration y does not match the type of its specificaiton. *)

signature s1 = sig val x : unit end
signature s2 = sig val y : bool end
signature s  = sig include s1 include s2 end
structure S :> s = struct val x = () val y = 1 end;
