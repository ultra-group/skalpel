(* old test case name: code441.sml *)

(* Typable *)
signature s = sig val f : 'a -> 'a end
structure S : s = struct fun f x : 'a = x end

(* Typable *)
signature s = sig val f : int -> int end
structure S : s = struct fun f x : 'a = x end

(* Untypable - The signature matching is too general *)
signature s = sig val f : 'a -> 'a end
structure S : s = struct fun f x : int = x end

(* Typable *)
fun f x : 'a = x
val _ = f 1
