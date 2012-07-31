(* old test case name: code433.sml *)

(* Untypable *)
signature s = sig type t = bool end

structure S1 :> s = struct
type t = int
fun f (x : t) = x + 1
end
