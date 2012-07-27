structure S = struct
datatype ''a mydt = firstCons of ''a
end;

open S;

firstCons(5.0);
