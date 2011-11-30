type xyzzy = int
type ('a,'b) g = 'a
type 'a f = ('a,int) g
overload + : 'a * 'a  -> 'a with 'a in (Int31.int, Int32.int, real f, xyzzy)
(*overload + : 'a * 'a  -> 'a as Int.+ and Real.+*)
val _ = 1.1 + 1.1
