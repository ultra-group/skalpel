(* untypable *)
signature s = sig val v : int val g : int end
structure S : s = struct val w = 1 val g = 1 end
(* untypable *)
signature s = sig val v : int val f : int end
structure S : s = struct val w = 1 val g = 1 end
