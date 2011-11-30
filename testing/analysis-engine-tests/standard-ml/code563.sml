(* Untypable *)

structure S = struct
  datatype 'a u = U
  datatype 'a v = V
  datatype 'a t = T of 'a
  val rec f = fn v => let val rec g = fn w => v (T U)
		      in v (T V)
		      end
end
