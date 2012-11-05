functor F (S : sig val myEqual : ''a * ''a -> bool end) =
	struct
	val _ = S.myEqual (5.0, 3.0)
	end
