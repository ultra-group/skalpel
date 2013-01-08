(* old test case name: code325.sml *)

(* Only x is free *)
structure S = struct
exception e
fun f () = (raise e; ())
end
val _ = (x, S.f () handle e => () | h => ());;
