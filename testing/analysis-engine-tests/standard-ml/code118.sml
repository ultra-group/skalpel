fun g x y z = if z then x + y else y
fun f [] y = y
  | f [x] y = g x y true
  | f (x :: xs) y = x + (f xs y)
