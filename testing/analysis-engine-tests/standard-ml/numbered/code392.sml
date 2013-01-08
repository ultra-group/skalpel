(* old test case name: code392.sml *)

(* typable *)
signature s = sig
    type t
    val c : int -> t
end

structure S :> s = struct
datatype t = c of int
val (c x) = c 1
end

(* untypable *)
val (S.c x) = S.c 1;
