structure X :> sig type t; val x : t end = struct type t = int ; val x = 1 end;
X.x = X.x;
