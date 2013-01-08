(* old test case name: code548.sml *)

(* Untypable
 *
 * x is declared as an integer but specified as a Boolean. *)

structure S =
struct
functor F (X : sig val x : bool end) = struct end;
end;

open S

structure T = F(val x = 1);
