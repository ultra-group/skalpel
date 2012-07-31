(* old test case name: code220.sml *)

(* Here is a typical error report, for comparison:

ex2.sml:21.1-25.24 Error: operator and operand don't agree [tycon mismatch]
  operator domain: int * int -> 'Z list * int
  operand:         int * int -> int * int
  in expression:
    mapActL f

*)

fun mapActR f (xs,state) =
    let	fun iter (x,(xs,state)) =
	    let val (x,state) = f (x,state)
	    in (x :: xs, state) end
    in foldr iter (nil,state) xs
    end

fun mapActL f (xs,state) =
    let fun iter (x,(xs,state)) =
	    let val (x,state) = f (x,state)
	    in (xs @ x, state) end
    in foldl iter (nil,state) xs end

fun isEven n = n div 2 = 0

(* multiplies each odd element of xs by 2,
 keeps track by how much the list sum gets incremented *)
fun doubleOdds xs =
    let fun f (n,inc) = if isEven n
			then ( n, inc )
                        else ( 2 * n, inc + n)
    in mapActL f (xs,0) end
