(* old test case name: code25.sml *)

(* There are 2 non-connected sets of constraints in the let binding. *)
(* We would benefit from checking that. *)
let
    datatype u = U of (int t, bool) t
    datatype m = M of (int p -> bool) p
    datatype ('a, 'b) w = W of 'a * 'b
    and v = V of (int, bool) w
    val x = V (W (0, 0))
in ()
end;
