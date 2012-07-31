(* old test case name: code414.sml *)

(* Untypable *)
structure S = struct
val x = ()
structure P = struct structure R = struct val x = true end end
structure Q = struct val f = fn x => x + 1 end
end
open S
structure P = struct open P end
open P
open R Q (* different from open P R Q *)
val _ = f x
