structure X : sig val t : ''a * ''a -> bool end = struct fun t (x,y) = x = y end;
X.t (5.0, 6.0);
