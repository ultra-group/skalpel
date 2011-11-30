structure S = struct
val g = let val f = fn _ => [] in f end
end;
val f = S.g ();
val _ = (1 :: f, true :: f)
