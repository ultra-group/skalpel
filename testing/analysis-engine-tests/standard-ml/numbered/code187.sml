val f = u1 u2
val _ = let val g = (fn h => h :: f) in (g 1, g true) end
