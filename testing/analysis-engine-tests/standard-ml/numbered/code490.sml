(* old test case name: code490.sml *)

(* Typable *)
infix f
fun x f y = (x, y)

structure S = struct
infix f
fun x f y = (x, y)
end

open S

val _ = 1 f 2;
