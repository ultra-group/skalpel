(* old test case name: code465.sml *)

(* Untypable - functors *)
functor F (S : sig val x : int end) = struct
val _ = S.x + 1
end

structure X = struct val x = true end

structure T = F(X);
