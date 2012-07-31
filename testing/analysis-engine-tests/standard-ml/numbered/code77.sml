(* old test case name: code77.sml *)

(* arity clashes between empty sequence and sequence of length 1 *)
datatype 'a t = Nil of unit | Cons of 'a * ('a t);
type 'a int = 'a t;
let val x = Nil () in (Cons (1, x), Cons (true, x)) end;
val x = Nil : unit -> int int;
