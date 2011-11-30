(* Untypable because of the type t in X that does not match the
 * specification given by FS. *)
signature FS = sig type 'a t val f : 'a t -> 'a t end where type 'a t = 'a
functor F (S : FS) : sig type u val g : 'a S.t -> 'a S.t end = struct
type u = int S.t
val _ = S.f 1
val g = S.f
end
structure X = struct type 'a t = int fun f x = x end
structure T = F(X)
val _ = T.g true

(* Simpler version *)
functor F (S : sig type 'a t = 'a end) = struct end
structure T = F(struct type 'a t = int end)
