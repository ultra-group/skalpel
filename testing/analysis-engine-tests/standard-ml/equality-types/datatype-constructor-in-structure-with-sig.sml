structure S : sig datatype ''a mydt = firstCons of ''a end =
struct
datatype ''a mydt = firstCons of ''a
end;

S.firstCons(5.0)
