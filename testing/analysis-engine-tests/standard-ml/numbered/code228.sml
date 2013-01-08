(* old test case name: code228.sml *)

(* Here is a standard error report, for comparison:

ex3.sml:48.1-50.18 Error: case object and rules don't agree [tycon mismatch]
  rule domain: int list
  object: int list list
  in expression:
    (case ((powerset o sort leq) xs)
      of y :: ys => (insert y) ys
       | nil => <errorvar> :: nil)
*)

fun insert leq x =
    let fun f nil       = x :: nil
          | f (y :: ys) = if leq (x,y)
			  then x :: y :: ys
			  else y :: f ys
    in f end

fun sort leq =
    let val insert = insert leq
	fun f nil       = nil
          | f (y :: ys) = insert y (f ys)
    in f end

fun leq (x,y) = x <= y
val insert = insert leq

fun powerset xs =
    let fun add (x,nil)        = nil
          | add (x, xs :: xss) = (insert x xs) :: add (x,xss)
    in case xs of
	   nil => nil :: nil
	 | x :: xs => add (x, powerset xs)
    end

fun set_insert eq x =
    let fun f nil       = x :: nil
          | f (y :: xs) = if eq (x,y)
			  then y :: xs
			  else y :: f xs
    in f end

fun list_eq (nil,nil)          = true
  | list_eq (x :: xs, y :: ys) = x = y andalso list_eq (xs,ys)
  | list_eq _                  = false

val set_insert = set_insert list_eq

fun discard_least nil = nil
  | discard_least (xs :: xss) =
    if null xs
    then set_insert xs (discard_least xss)
    else set_insert (tl xs) (discard_least xss)

fun silly xs =
    case (powerset o (sort leq)) xs of
	(y :: ys) => insert y ys
      | nil => y :: nil;
