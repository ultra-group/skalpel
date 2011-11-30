(* Untypable *)
structure S = struct val f = (fn _ => fn _ => 0) () end
open S
val _ = (f 1, f true)

(* Untypable *)
structure S = struct fun f z x = z x end
val _ = fn z => let open S
		in (f z 1, f z true) end
