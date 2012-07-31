(* old test case name: code261.sml *)

(* This is untypable *)
signature S = sig val x : 'a end
structure st1 :> S = struct val x = true end
structure st2 :  S = struct val x = true end
val _ = st1.x : bool
val _ = st2.x : bool
