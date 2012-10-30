structure S :> sig type t end =
struct
   type t = int
end;

fun x (y:S.t) (z:S.t) = (y=z)
