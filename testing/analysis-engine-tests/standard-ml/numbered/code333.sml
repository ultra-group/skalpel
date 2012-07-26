type xyzzy = int
type ('a,'b) g = 'a
type 'a f = ('a,int) g
overload + : 'a * 'a  -> 'a with 'a in (real f, xyzzy)
(* typable *)
val _ = 1.1 + 1.1
(* untypable *)
val _ = true + true
