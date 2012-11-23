(* the next two lines are typable *)
structure S : sig type t end = struct type t = int; val x : t = 6 end;
fun x (y:S.t) (z:S.t) = y = z;

(* this is NOT typable *)

(* note if we do type t = real in the signature then we get a slice *)
structure S :> sig type t end = struct type t = real; val x : t = 6.0 end;
fun x (y:S.t) (z:S.t) = y = z;
