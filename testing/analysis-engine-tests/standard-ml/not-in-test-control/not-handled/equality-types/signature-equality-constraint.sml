structure X : sig val t : ''a * ''a -> bool end = struct val t = op = end;
X.t (5.0, 6.0);
