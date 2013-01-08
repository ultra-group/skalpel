(* old test case name: code545.sml *)

(* Untypable.
 *
 * the value y is missing in S.
 * The error could be solved by replacing the first x by y in S. *)

signature s  = sig val x : unit val y : bool end
structure S :> s = struct val x = true val x = () end;
