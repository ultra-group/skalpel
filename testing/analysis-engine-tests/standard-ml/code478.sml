fun sumList nil z = z
  | sumList (h::t) z = sumList t (h+z);

val a = sumList [1,2,3] 0;

val b = sumList [1.1,2.2,3.3] 0.0;
