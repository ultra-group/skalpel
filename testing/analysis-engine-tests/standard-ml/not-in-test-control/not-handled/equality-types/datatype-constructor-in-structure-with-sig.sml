(* the slice we get is correct, but we are missing the minimal error
 * slice that omits the signature *)

structure S : sig datatype ''a mydt = firstCons of ''a end =
struct
datatype ''a mydt = firstCons of ''a
end;

S.firstCons(5.0)
