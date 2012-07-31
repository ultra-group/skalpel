(* old test case name: code562.sml *)

(* Untypable.
 *
 * Similar to 558. *)

functor F (val x : int -> int) =
        struct val _ = x true end
structure T = F(val x = fn z => z)
