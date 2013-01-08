(* old test case name: code527.sml *)

(* Untypable
 *
 * Type constructor clash within a structure body. *)

structure S : sig type t val x : t end = struct
  type t = int
  val x = 1
  val _ = () : t
end;
