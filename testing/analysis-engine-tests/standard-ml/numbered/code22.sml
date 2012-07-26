let
    datatype t = y
    val g = fn x =>
	       fn y =>
		  let
		      val f = if y
			      then fn z => z + 1
			      else fn z => z
		  in f y
		  end
in ()
end
