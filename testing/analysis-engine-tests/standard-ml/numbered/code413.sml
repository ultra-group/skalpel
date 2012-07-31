(* old test case name: code413.sml *)

(* Untypable *)
structure S = struct
val x = ()
structure P = struct val x = true end
structure Q = struct val f = fn x => x + 1 end
end
open S
structure P = struct open P end
open P Q
val _ = f x
