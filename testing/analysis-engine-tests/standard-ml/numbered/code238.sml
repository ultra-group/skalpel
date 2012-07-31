(* old test case name: code238.sml *)

(* highlighting of forcing of statuses *)
fun f () = ()
  | f () = ()
val f = true
val _ = f + 1

(* context dependencies for value polymorphism *)
fun g () = let exception e of 'a in ((fn (e y) => y), e) end;
let val (x, y) = g () in (x (y true), x (y 1)) end;

(* merging of errors *)
val {foo, bar, bla} = {fooo=1, barr=1, bla=1}

(**)
val x = true
val _ = 1 + x
