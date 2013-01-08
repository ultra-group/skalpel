(* old test case name: code516.sml *)

(* Example using abstype *)
datatype t = T of int

abstype t = T of bool
with fun f x = T x
end

val _ = T true (* untypable *)
val _ = T 1    (* typable   *)
val _ = f 1    (* untypable *)
val _ = f true (* typable   *);
