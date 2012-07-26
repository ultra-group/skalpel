(* Untpable *)
val _ = fn x => (let val f = fn z : 'a => x z;
		 in f 1 end; fn z : 'a => z)
