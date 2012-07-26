val f = (fn x => x) []
val h = let val g = (fn h => h::f) in (1 :: (g 1), true :: (g true)) end
