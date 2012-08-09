structure S : sig val x : ''a -> ''a end =
struct
fun x y = y
end;

S.x 5.0
