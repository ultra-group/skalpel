fun f (x, y) = Int.toString(x + y);
fun g (x, y) = x ^ y;
infix 4 f;
infixr 4 g;
3 f 3 g "test";
