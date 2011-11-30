(* Here is a typical error report, for comparison:

ex5.sml:4.17-5.30 Error: operator is not a function [circularity]
  operator: 'Z
  in expression:
    (fst y) (snd y)
ex5.sml:7.1-7.28 Error: operator and operand don't agree [literal]
  operator domain: (bool -> bool) -> 'Z
  operand:         (int -> int) -> bool
  in expression:
    f (fn g => g <exp> > 0)
ex5.sml:9.1-9.41 Error: operator and operand don't agree [literal]
  operator domain: (bool -> bool) -> 'Z
  operand:         (int -> bool) -> int
  in expression:
    f
      (fn g =>
            (case (g <exp>)
              of <pat> => <exp>
               | <pat> => <exp>))

*)

fun fst (x,y) = x
fun snd (x,y) = y

fun f x = let val y = ( x (fn x => x), x (fn x => true) )
	  in fst y (snd y) end

val u = f (fn g => g 0 > 0)

val v = f (fn g => if g 0 then 0 else 1)
