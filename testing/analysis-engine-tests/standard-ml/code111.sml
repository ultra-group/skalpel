datatype 'a t = Blue   of 'a
              | Green  of 'a
	      | Red    of 'a
	      | Yellow of 'a
	      | Pink   of 'a
type 'a u = 'a t * bool * 'a
val x = (Red 2, 1, true)
val y : int u = x
