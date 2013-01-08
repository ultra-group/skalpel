(* old test case name: code229.sml *)

(* Here is a typical error report, for comparison:

ex4.sml:10.9-10.22 Error: operator and operand don't agree [literal]
  operator domain: int * int
  operand:         int
  in expression:
    (compare d) n

*)

fun compare d (x,y) = x-d >= y

fun f (n,m) (d,e) =
    let fun square x = x * x
	fun fac x = if x = 0 then 1 else fac (x-1)
	val x = square n
	val y = fac (n div m)
    in if x > y
       then compare d n m
       else compare e (n,m)
    end;
