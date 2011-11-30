(**SML-TES-SPEC val f : 'a -> 'b -> 'a*)
fun g x = (x (f 1 true), x (f true 1))
