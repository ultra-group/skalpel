(* old test case name: code493.sml *)

(* Untypable *)
signature s = sig structure V : sig type t val x : t end end where type V.t = int
(*structure S :> s = _S*)
functor F (S : s) = struct
val _ = S.V.x : bool
end

(* Untypable *)
signature s = sig type t val x : t end where type t = int
(*structure S :> s = _S*)
functor F (S : s) = struct
val x = S.x : bool
end
