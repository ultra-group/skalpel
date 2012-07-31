(* old test case name: code72.sml *)

(* Untypable.  Page 19: scope of explicit type variables *)
val ex1 = let val id : 'a -> 'a = fn z => z in id id end
val ex2 = (let val id : 'a -> 'a = fn z => z in id id end; fn z => z : 'a)
val ex3 = (let val id : 'a -> 'a = fn z => z in (x : 'a; id id) end; fn z => z : 'a)
fun ex4 (x : 'a -> bool) = x 1
val ex5 =
    let
	val x = (let val id : 'a -> 'a = fn z => z in id id end; fn z => z : 'a)
    in fn z => z : 'a
    end
