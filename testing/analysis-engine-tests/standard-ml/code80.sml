(* type inference should be OK, but semantic error: Bind *)
(* SML/NJ complains about the types though *)
datatype t = f
fun f f = f
val _ = f 1
