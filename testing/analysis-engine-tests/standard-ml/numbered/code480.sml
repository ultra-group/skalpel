(* old test case name: code480.sml *)

(* Untypable because of the application of T.g to true *)
signature FS = sig type 'a t val f : 'a t -> 'a t end where type 'a t = int
functor F (S : FS) : sig type u val g : u -> u end = struct
type u = int
val _ = S.f 1
val g = S.f
end
structure X = struct type 'a t = int fun f x = x + 1 end
structure T = F(X)
val _ = T.g true;
