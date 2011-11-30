(* not well-typed: value polymorphism
   and more precisely the condition: C(longvid) in {c, e} (the interesting one) *)
fun g () = let exception e of 'a in ((fn (e y) => y), e) end;
let val (x, y) = g () in (x (y true), x (y 1)) end;
(* --- *)
let val h = g in (#1 (h ()) ((#2 (h ())) true), #1 (h ()) ((#2 (h ())) 1)) end;
(* this is an error *)
let exception e of 'a in e 1 end;
(* these are well-typed *)
let val f = (fn () => let exception e of 'a in e end) in f () 1 end;
let val (x, y) = g () val (a, b) = g () in (x (y true), a (b 1)) end;
