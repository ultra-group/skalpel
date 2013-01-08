(* old test case name: code269.sml *)

structure S1 = struct val x = true end
structure S2 = struct val y = S1.x fun f g = g () () end
structure S3 = struct val _ = S2.y (*+ 1*) fun f _ = S2.f f end;
