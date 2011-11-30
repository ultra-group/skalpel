(* Untypable - Monomorphism and opening *)
structure S = struct
  structure Y = struct end
  structure T = struct
    structure X = struct end
    datatype 'a u = U
    open X
    val rec dummy3 = fn x => x
  end
  datatype 'a t = T
  open T
  val rec f = fn x => let val rec g = fn y => x y
		      in let val rec dummy1 = fn dummy2 => g U
			 in g T
			 end
		      end
end
