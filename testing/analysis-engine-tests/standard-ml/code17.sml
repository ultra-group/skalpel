let
    val x = true
    val y = x
in
    let
	val f = if true then true else y
    in
	(let
	     datatype u = U and v = V and t = f1 | f2 | f of int | f3 and m = M
	 in f
	 end) f
    end
end
