(* Typable *)
structure S : sig type 'a t end =
struct type 'a t = int end
val _ = 1 : int S.t;

(* Untypable - arity clash *)
structure S : sig type 'a t end =
struct type 'a t = int end
val _ = 1 : S.t;

(* Untypable - arity clash and type constructor clash *)
structure S :> sig type 'a t end =
struct type 'a t = int end
val _ = 1 : S.t;
