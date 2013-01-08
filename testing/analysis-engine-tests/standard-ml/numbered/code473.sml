(* old test case name: code473.sml *)

(* Untypable - restrictive signature matching constraint *)
structure S = struct fun f x = x end
val _ = S.f ()
structure T : sig val f : unit -> unit end = S
val _ = T.f 1;
