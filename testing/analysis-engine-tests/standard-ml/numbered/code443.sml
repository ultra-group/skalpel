(* old test case name: code443.sml *)

(* Untypable
 * Sum being a strdec, the type of + is set to being:
 *   (int * int) -> int,
 * but later sum is used on a list of reals .*)
fun sum [] = 0
  | sum (x :: xs) = x + (sum xs)

val _ = sum [1.1]
