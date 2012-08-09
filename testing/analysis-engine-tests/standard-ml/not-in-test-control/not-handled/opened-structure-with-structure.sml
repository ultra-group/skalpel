(* jpirie: notice that with the signature removed we get a type error slice for this
 * why is this the case? *)

structure S : sig datatype ''a mydt = firstCons of ''a end =
struct
datatype ''a mydt = firstCons of ''a
end;

S.firstCons(5.0)
