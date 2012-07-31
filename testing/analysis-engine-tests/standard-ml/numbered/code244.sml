(* old test case name: code244.sml *)

(* names shouldn't be in the error *)
datatype t = T
datatype u = U
val _ = fn z => (z T, z U)
