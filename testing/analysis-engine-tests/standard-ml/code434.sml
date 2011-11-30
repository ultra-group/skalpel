(* Untypable *)
signature s = sig type t = bool end

structure S1 :> s = struct
type t = _typet
fun f (x : t) = x + 1
end
