(* Untypable *)
structure S = struct
  datatype 'a t = c of 'a
  val rec f = fn c => let val rec g = fn c x => x
		      in c
		      end
end
