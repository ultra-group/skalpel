structure X :> sig type t = real; val x : t end =
struct
   type t = real;
   val x = 1.0;
end;

X.x = X.x;
