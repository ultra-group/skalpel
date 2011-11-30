(* EXAMPLE 1 - Typable because S is free *)
structure S = struct
  open S
  datatype 'a t = T
  datatype 'a u = U
  val rec d1 = fn z => let val rec d2 = fn v => z T
		       in z U
		       end
end

(* EXAMPLE 2 - Untypable *)
structure S = struct
  datatype 'a t = T
  datatype 'a u = U
  val rec f = fn C x => let val rec g = fn v => x T
			in x U
			end
end
