val f = (fn x => x) (fn x => x)
val h = let val g = (fn h => f h) in (g 1, g true) end
