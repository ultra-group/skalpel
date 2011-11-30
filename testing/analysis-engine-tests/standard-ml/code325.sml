(* Only x is free *)
structure S = struct
exception e
fun f () = (raise e; ())
end
val _ = (x, S.f () handle e => () | h => ());
