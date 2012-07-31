(* old test case name: code446.sml *)

(*EXAMPLE1: Untypable*)
val _ = fn z => let val x = 1
		    val _ = let val y = z
				val z = 1
			    in fn w => (w y, w z)
			    (* forces the first z to be an int *)
			    end
		in fn w => (w z, w true)
		(* forces the first z to be a bool*)
		end

(*EXAMPLE2: Untypable*)
val f = (fn x => x) (fn x => x) (*monomorphic identity*)
structure S = struct
val y = f
val f = fn x => x + 1
val _ = y (f 1)
end
val _ = f true
