(* old test case name: code534.sml *)

(* Untypable.
 *
 * Similar to testcase 528, but here f's specification
 * does not contain any explicit type variable. *)

type 'a t = int
structure X : sig val f : 'a t -> 'a t end = struct
  fun g () = let val f = fn x => x in f end
  val f = g ()
end;
