(* untypable *)
datatype 'a t = C of ('a,
		      'a)
			 t
			 t

datatype 'a u = C of ('a, 'a) u u
