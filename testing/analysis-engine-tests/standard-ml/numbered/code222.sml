fun apply2 (f, _) (x, _) = (f x, f x)
fun apply1 (f1, f2) (x1, x2) b =
    if b
    then (f1 x1, f2 x2)
    else apply2 (f1, f2) (x1, x2)
fun swap (x1, x2) = (x2, x1)
fun cons x =
    let
	val g = fn h => h :: x
    in swap (apply1 (g, g) (1, true) false) end
