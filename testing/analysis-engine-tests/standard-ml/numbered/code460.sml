(* old test case name: code460.sml *)

(* Untypable - Type constructor clash involving -> and a type function *)
structure S = struct
  type 'a t = 'a -> 'a
  type 'a w = 'a t
  datatype 'a t = T of 'a t
  val rec (g : 'a w) = fn v => v
  val rec f = fn v => T g
end;
