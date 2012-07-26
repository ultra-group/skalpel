val f2 = fn x => let val g = (fn h => h :: x) in (g 1, g true) end
val f3 = fn x => let val g = (fn h => x :: h) in (g [1], g [true]) end
