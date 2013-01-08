(* old test case name: code85.sml *)

(* Contains free identifiers. Can be typable with:
 * datatype t = f of int | g *)
val ex = fn (f x) => (x + 1; g)
fun f x = x;
