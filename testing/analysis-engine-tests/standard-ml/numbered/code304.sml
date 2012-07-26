(* untypable *)
signature s = sig (*type t*) val v : int end
structure S : s = struct (*type u = int*) val w = 1 end
