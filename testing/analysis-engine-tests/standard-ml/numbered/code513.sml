(* old test case name: code513.sml *)

(* Untypable *)
signature s = sig datatype 'a t = C of 'a end where type 'a t = bool;
functor F (S : s) = struct val _ = S.C true : unit S.t end;
