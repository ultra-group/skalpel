structure S :> sig type t = real end =
struct
   type t = real
end;

fun x (y:S.t) (z:S.t) = (y=z)
