(* Typable *)
datatype 'a t = C of 'a * 'a
fun f x = C (x, x)
