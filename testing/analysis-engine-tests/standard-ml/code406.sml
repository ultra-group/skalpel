(* Typable *)
val _ = fn x => let val u = fn v => let val m = v x
				    in (m, fn z => z) end
		in let val y = fn z => x z
		   in (y 1(*, y true*)) end
		end
